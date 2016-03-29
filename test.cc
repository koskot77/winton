#include <stdio.h>
#include "utils.cc" 

using namespace std;

// compile: g++ -Wl,--no-as-needed -g -Wall -std=c++11 -o test test.cc -lpthread

int main(void){

    nCores = 4;

    printf("Reading\n");
    unsigned int maxRows = read_csv("impTrain.csv", 26, 205, false);
    printf(" done reading %d lines\n",maxRows);

    printf("Correlating all with all\n");
    correlate( maxRows );

    printf("Sorting\n");
    unsigned long long size = maxRows*(maxRows-1)/2;
    splitQuickSort( correlations, indices, size);

    printf("Clustering\n");
    UnionFind uf(maxRows);
    const double cutoffDistance = 0.7;
    cluster( uf, cutoffDistance );
    printf(" found %d clusters with cutoff distance %f\n",uf.nClusters(),cutoffDistance);

    printf("Saving\n");
    for(auto &clust : uf.clusters() ){
        ostringstream str;
        str<<"tmp/cl"<<(clust.first-1)<<".txt";
        ofstream file(str.str());
        for(auto element : clust.second){
            unsigned long row1 = ( clust.first > element ? (clust.first-1) : (element-1) );
            unsigned long row2 = ( clust.first > element ? (element-1) : (clust.first-1) );
            if( row1 == row2 ) continue;
            file<<(element-1)<<" "<< cor(series[row1],cache[row1].first,cache[row1].second,
                                         series[row2],cache[row2].first,cache[row2].second, length ) <<endl;
        }
        file.close();
    }

    return(0);
}
