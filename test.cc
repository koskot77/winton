#include <stdio.h>   // printf, fopen, fclose, getline ...
#include <stdlib.h>  // strtod
#include <string.h>  // bzero
#include <iostream> 
#include <fstream>
#include <sstream>
#include <map>

// all of the files live in the cluscorr R package, but will be used in a standalone mode within this example
extern size_t nCores;
#include "utilities.cc"
#include "quicksort.cc"
#include "correlator.cc"
#include "read_csv.cc"

// Compile (make sure you have the cluscorr R package locally):
//   g++ -Wl,--no-as-needed -g -Wall -std=c++11 -I./clustcorr/src/ -o test test.cc -lpthread
// Run:
//   download the train.csv or test.csv files from the competition
//   impute them with inpute.R
//   create local tmp directory
//   execute ./test
using namespace std;

int main(int argc, char *argv[]){
    if( argc != 3 ){ printf("Specify start and end positions as the two arguments\n"); return 0; }

    // set number of cores
    nCores = 4;

    printf("Reading\n");
    double *buffer = 0;
    unsigned int startPos = atoi(argv[1]), nextToEndPos = atoi(argv[2]); //26 205(145) 
    unsigned int maxRows = read_csv("impTrain.csv", startPos, nextToEndPos, buffer, false);

    unsigned int maxColumns = nextToEndPos-startPos;
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
