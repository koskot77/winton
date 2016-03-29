#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <list>
#include <map>
#include <thread>
#include <future>

size_t nCores = 4;

using namespace std;

// compile: g++ -Wl,--no-as-needed -g -Wall -std=c++11 -o test test.cc -lpthread

// mean, sd, and cor functions equivalent to their R analogs
double mean(const double *vals, size_t size){
    double sum = 0;
    for(unsigned int i=0; i<size; i++) sum += vals[i];
    return sum/size;
}

double sd(const double *vals, size_t size){
    double sum1 = 0, sum2 = 0;
    for(unsigned int i=0; i<size; i++){
        sum2 += vals[i]*vals[i];
        sum1 += vals[i];
    }
    return sqrt( (sum2-sum1*sum1/size)/(size-1) ); // unbiased
}

double cor(const double *a, double meanA, double sdA, const double *b, double meanB, double sdB, size_t size){
    double sum = 0;
    for(unsigned int i=0; i<size; i++)
        sum += a[i] * b[i];
    return size/double(size-1) * (sum/size-meanA*meanB) / sdA / sdB ;
}

////////////////////////////////////////////////////

// Canonical union-find for fast clustering
class UnionFind {
private:
    map<int,int>         node2cluster;
    map<int, list<int> > cluster2nodes;

public:
    int joinClusters(int cluster1, int cluster2){
        if( cluster1 == cluster2 ) return cluster1;
        list<int> &nodes1 = cluster2nodes[cluster1];
        list<int> &nodes2 = cluster2nodes[cluster2];
        if( nodes1.size()==0 || nodes2.size()==0 ) return 0;
        int newCluster = 0;
        if( nodes1.size() < nodes2.size() ){
            newCluster = cluster2;
            for(list<int>::const_iterator n = nodes1.begin(); n != nodes1.end(); n++)
                node2cluster[*n] = newCluster;
            nodes2.insert(nodes2.end(),nodes1.begin(),nodes1.end());
            cluster2nodes.erase(cluster1);
        } else {
            newCluster = cluster1;
            for(list<int>::const_iterator n = nodes2.begin(); n != nodes2.end(); n++)
                node2cluster[*n] = newCluster;
            nodes1.insert(nodes1.end(),nodes2.begin(),nodes2.end());
            cluster2nodes.erase(cluster2);
        }
        return 0;
    }

    int findCluster(int node) { return node2cluster[node]; }

    int nClusters(void) const { return cluster2nodes.size(); }

    const map<int, list<int> >& clusters(void) const { return cluster2nodes; }

    UnionFind(int maxNodes){
        for(int i=1; i<=maxNodes; i++){
             node2cluster [i] = i;
             cluster2nodes[i].push_back(i);
        }
    }
};

// Canonical quick sort for ... sorting

enum {BEGIN_POS=1, MEDIAN_POS=2, END_POS=3};
unsigned int pivotMethod = MEDIAN_POS;

unsigned long long pivotElementPos(float *array, unsigned long long length){
    if( length<1 ) exit(1);
//    return (rand()%length);
    switch( pivotMethod ){
        case BEGIN_POS  : return 0; break;
        case END_POS    : return length-1; break;
        case MEDIAN_POS :
            unsigned long long middlePos = ( length % 2 ? length/2 : length/2-1 );
            float a = array[0];
            float b = array[middlePos];
            float c = array[length-1];
            if( (a>=b && b>=c) || (a<=b && b<=c) ){ return middlePos; }
            if( (b>=a && a>=c) || (b<=a && a<=c) ){ return 0; }
            if( (a>=c && c>=b) || (a<=c && c<=b) ){ return length-1; }
            break;
    }
    exit(1);
}

void swap(float &i,              float &j)             { float tmp=j; j=i; i=tmp; }
void swap(unsigned long long &i, unsigned long long &j){ int   tmp=j; j=i; i=tmp; }

unsigned long long partition(float *array, unsigned long long pivotPos, unsigned long long *payload, unsigned long long length){
    if( length <= 1 ) return 0;
    if( pivotPos >= length ) exit(1);

    // preprocessing step
    float pivot = array[pivotPos];
    if( pivotPos != 0 ){
        swap( array  [pivotPos], array  [0] );
        swap( payload[pivotPos], payload[0] );
        pivotPos = 0;
    }

    unsigned long long i,j,k=1;
    for(i=1,j=1; j<length; j++){
        if( array[j] < pivot ){
            if( i != j ){
                swap( array  [i], array  [j] );
                swap( payload[i], payload[j] );
            }
            i++;
        }
        if( array[j] == pivot ) k++;
    }

    if( i == 1 && k == length ) // all elements are identical
        return i;

    swap( array  [i-1], array  [0] );
    swap( payload[i-1], payload[0] );

    return i;
}

void recure(float *array, unsigned long long *payload, unsigned long long length){

    unsigned long long i = partition(array, pivotElementPos(array,length), payload, length);

    float *first  = &(array[0]);
    float *second = &(array[i]);
    unsigned long long *plfirst  = &(payload[0]);
    unsigned long long *plsecond = &(payload[i]);

    if( i>1 )
        recure(first,  plfirst, i-1);
    if( length-i>1 )
        recure(second, plsecond, length-i);

}

void splitQuickSort(float *array, unsigned long long *payload, unsigned long long length){

    const unsigned int maxDepth = 5, maxBlocks = (1<<maxDepth);

    if( length < maxBlocks ){
        recure(array, payload, length);
        return;
    } 

    // consider those a perfectly balanced tree-like structure
    float              *subarray  [maxBlocks*2];
    unsigned long long *subpayload[maxBlocks*2];
    unsigned long long  sublength [maxBlocks*2];

    subarray  [0] = array;
    subpayload[0] = payload;
    sublength [0] = length;

    // unroll recursion into two linear loops
    for(unsigned int depth=0; depth<maxDepth; depth++){

        unsigned int subTasksSeen = (1<<depth)-1;

        for(unsigned int subtask=0; subtask<unsigned(1<<depth); subtask++){

            unsigned long long pos = partition( subarray  [ subtask + subTasksSeen ],
                                                pivotElementPos(
                                                    subarray [ subtask + subTasksSeen ],
                                                    sublength[ subtask + subTasksSeen ]
                                                ),
                                                subpayload[ subtask + subTasksSeen ],
                                                sublength [ subtask + subTasksSeen ] );

            unsigned int subTasksAtThisDepth = (1<<(depth+1))-1;

            subarray  [ 2*subtask+0 + subTasksAtThisDepth ] = subarray  [ subtask + subTasksSeen ];
            subarray  [ 2*subtask+1 + subTasksAtThisDepth ] = subarray  [ subtask + subTasksSeen ] + pos;
            subpayload[ 2*subtask+0 + subTasksAtThisDepth ] = subpayload[ subtask + subTasksSeen ];
            subpayload[ 2*subtask+1 + subTasksAtThisDepth ] = subpayload[ subtask + subTasksSeen ] + pos;
            sublength [ 2*subtask+0 + subTasksAtThisDepth ] = pos-1;
            sublength [ 2*subtask+1 + subTasksAtThisDepth ] = sublength [ subtask + subTasksSeen ] - pos;
        }
    }

    const size_t maxNumThreads = nCores;
    std::future<void> results [ maxNumThreads ];

    for(unsigned int block=0; block<maxBlocks; block++){

        unsigned int offset = maxBlocks - 1;

        if( sublength[block+offset] == 0 ) continue;

        // identify a free thread
        size_t freeThread = 0;
        for( ;  results[freeThread].valid() &&
                results[freeThread].wait_for(std::chrono::milliseconds(100)) != std::future_status::ready ; )
            if( freeThread == maxNumThreads-1 ) freeThread = 0; else freeThread++;

        // submit
        results[freeThread] = std::async(std::launch::async, recure, subarray[block+offset], subpayload[block+offset], sublength[block+offset]);

        printf("  finished  block %d/%d in thread %ld\n",block,maxBlocks,freeThread);
    }
    printf("  finalizing ... \n");

    // wait until all threads finish
    for(size_t thr=0; thr<maxNumThreads; thr++)
        if( results[thr].valid() ) results[thr].wait();

}

////////////////////////////////////////////////////

// Global variables that'll be accessed by all threads
unsigned long      length;            // length of the time series
double             **series;          // input series[size][length]
map<int, pair<double,double> > cache; // cache of mean and sd for the input data

unsigned int read_csv(const char *csvFileName, unsigned int seriesBeginIndex, unsigned int seriesEndIndex, bool header=true){

    if( seriesEndIndex <= seriesBeginIndex ){
        printf("  seriesBeginIndex:%d <= seriesEndIndex:%d\n",seriesBeginIndex,seriesEndIndex);
        return 0;
    }

    length = seriesEndIndex - seriesBeginIndex;

    FILE *input = NULL;
    if( (input = fopen(csvFileName,"rt")) == NULL ){ printf("  %s doesn't exist",csvFileName); return 0; }

    char *buff = NULL;
    size_t len = 0;

    if( header && getline(&buff, &len, input)!=-1 ){
        printf("  read header: %s\n",buff);
        free(buff);
        buff = NULL;
    }

    // find out how many more lines we need to read
    long startPos = ftell(input);
    unsigned long nLines = 0;
    while( getline(&buff, &len, input) != -1 ) nLines++;
    fseek(input, startPos, SEEK_SET);

    series = new double* [nLines];

    for(unsigned int line = 0, nItems = 0; !feof(input) && getline(&buff, &len, input) != -1; line++){

        if( nItems == 0 ){
            nItems  = 1;
            for(unsigned int pos=0; pos<strlen(buff); pos++) if( buff[pos] == ',' ) nItems++;
        }

        series[line] = new double [length];
        bzero( series[line], sizeof(double)*(length) ); 

        // parse the comma-separated list of arbitrary length
        char *item = buff;
        unsigned int index = nItems;
        while( --index >= 0 ){
            if( (item = strrchr(buff,',')) != NULL ) *item++ = '\0'; else item = buff;
            if( strlen(item) ){
                if( index >= seriesBeginIndex && index < seriesEndIndex ){
                    series[line][index-seriesBeginIndex] = strtold(item, NULL);
                }
                if( item == buff ) break;
            }
            cache[line].first  = mean(series[line],length);
            cache[line].second = sd  (series[line],length);
        }

    }

    fclose(input);
    return nLines;
}

// Global variables that'll be accessed by all threads
float              *correlations;     // evaluated correlations[size*(size-1)/2]
unsigned long long *indices;          // indexes for sorting[size]

bool computeCorrelations(unsigned long long begin, unsigned long long end){
    for(unsigned long long index=begin; index<end; index++){
        unsigned long row1  = (unsigned long)( (1+sqrt(1+8*index))/2 );
        unsigned long row2  = index - row1*(row1-1)/2;
        indices     [index] = index;
        correlations[index] = -fabs( cor(series[row1],cache[row1].first,cache[row1].second,
                                         series[row2],cache[row2].first,cache[row2].second, length ) );
    }
    return true;
}

void correlate(unsigned int maxRows){
    correlations       = new  float              [maxRows*(maxRows-1)/2];
    indices            = new  unsigned long long [maxRows*(maxRows-1)/2];
    bzero(correlations,sizeof(float)             *maxRows*(maxRows-1)/2);
    bzero(indices,     sizeof(unsigned long long)*maxRows*(maxRows-1)/2);

    const size_t maxNumThreads = nCores;
    std::future<bool> results [ maxNumThreads ];
    const unsigned long long maxIndex  = maxRows*(maxRows-1)/2;
    const unsigned int       maxBlocks = 100;
    for(unsigned int block=0; block<maxBlocks; block++){
        unsigned long long begin =  block    * maxIndex / maxBlocks ;
        unsigned long long   end = (block+1) * maxIndex / maxBlocks ;

        // identify a free thread
        size_t freeThread = 0;
        for( ;  results[freeThread].valid() &&
                results[freeThread].wait_for(std::chrono::milliseconds(100)) != std::future_status::ready ; )
            if( freeThread == maxNumThreads-1 ) freeThread = 0; else freeThread++;

        // submit
        results[freeThread] = std::async(std::launch::async, computeCorrelations, begin, end);

        printf("  finished  block %d/%d in thread %ld\n",block,maxBlocks,freeThread);
    }
    printf("  finalizing ... \n");

    // wait until all threads finish
    for(size_t thr=0; thr<maxNumThreads; thr++)
        if( results[thr].valid() ) results[thr].wait();

}


void cluster(UnionFind &uf, double cutoffDistance = 0.7){
    unsigned long maxRows = uf.nClusters();
    for(unsigned long long i=0; i<maxRows*(maxRows-1)/2; i++){
        unsigned long long index = indices[i];
        unsigned long row1  = (unsigned long)( (1+sqrt(1+8*index))/2 );
        unsigned long row2  = index - row1*(row1-1)/2;
        if( -correlations[i] < cutoffDistance ) break;
        int node1 = row1+1;
        int node2 = row2+1;
        int cluster1 = uf.findCluster(node1);
        int cluster2 = uf.findCluster(node2);
        if( cluster1 != cluster2 ) uf.joinClusters(cluster1,cluster2);
    }
}

