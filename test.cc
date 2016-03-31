#include <stdio.h>   // printf, fopen, fclose, getline ...
#include <stdlib.h>  // strtod
#include <string.h>  // bzero
#include <iostream> 
#include <fstream>
#include <sstream>
#include <map>

// all of the packages below live in the cluscorr R package, but will be used in a standalone mode below
extern size_t nCores;
#include "utilities.cc"
#include "quicksort.cc"
#include "correlator.cc"

// Compile (make sure you have the cluscorr R package locally):
//   g++ -Wl,--no-as-needed -g -Wall -std=c++11 -I./clustcorr/src/ -o test test.cc -lpthread
// Run:
//   download the train.csv or test.csv files from the competition
//   impute them with inpute.R
//   create local tmp directory
//   execute ./test

unsigned int read_csv(const char *csvFileName, unsigned int seriesBeginIndex, unsigned int seriesEndIndex, double* &buffer, bool header=true){

    if( seriesEndIndex <= seriesBeginIndex ){
        printf("  seriesBeginIndex:%d <= seriesEndIndex:%d\n",seriesBeginIndex,seriesEndIndex);
        return 0;
    }

    size_t length = seriesEndIndex - seriesBeginIndex;

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

    for(unsigned int line = 0, nItems = 0; !feof(input) && getline(&buff, &len, input) != -1; line++){

        if( nItems == 0 ){
            nItems  = 1;
            for(unsigned int pos=0; pos<strlen(buff); pos++) if( buff[pos] == ',' ) nItems++;
            buffer = new double [nLines*length];
            bzero( buffer, sizeof(double)*(nLines*length) ); 
        }

        double (*series)[length] = (double (*)[length]) buffer;

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
        }

    }

    free(buff);
    fclose(input);
    return nLines;
}

using namespace std;

int main(void){

    // set number of cores
    nCores = 4;

    printf("Reading\n");
    double *buffer = 0;
    unsigned int maxRows = read_csv("impTrain.csv", 26, 205, buffer, false);
    unsigned int maxColumns = 205-26;
    printf(" done reading %d lines\n",maxRows);

    // we will have to pass over all of the arguments by reference as we call R-complient function 
    double cutoffDistance = 0.7;
    int dimensions[2] = {(int)maxRows,(int)maxColumns};
    int clustering[maxRows];
    clusterCorrelations( dimensions, buffer, &cutoffDistance, clustering );
    // the output in the clustering argument compies with R numbering scheme, i.e. starts from 1

    printf("Saving clusters with more than 2 elements\n");
    map< int,list<int> > clusters;

    for(size_t i=0; i<maxRows; i++)
        clusters[ clustering[i] ].push_back(i+1); // let's be consistent in starting from 1

    const double (*series)[maxColumns] = (const double (*)[maxColumns])buffer;

    for(auto &clust : clusters ){
        if( clust.second.size() > 1 ){
            ostringstream str;
            str<<"tmp/cl"<<(clust.first-1)<<".csv";
            ofstream file(str.str());
            for(auto element : clust.second){
                unsigned long row1 = ( clust.first > element ? (clust.first-1) : (element-1) );
                unsigned long row2 = ( clust.first > element ? (element-1) : (clust.first-1) );
                if( row1 == row2 ) continue;

                file<<(element-1)<<","<< cor(series[row1],cachedMean[row1],cachedSd[row1],
                                             series[row2],cachedMean[row2],cachedSd[row2], nColumns ) <<endl;
            }
            file.close();
        }
    }

    return(0);
}
