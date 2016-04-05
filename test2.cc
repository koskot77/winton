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
//   g++ -Wl,--no-as-needed -g -Wall -std=c++11 -I./clustcorr/src/ -o test2 test2.cc -lpthread
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
    double *buffer1 = 0, *buffer2 = 0;
    unsigned int startPos = atoi(argv[1]), nextToEndPos = atoi(argv[2]); //26 205(145) 
    unsigned int maxRows1 = read_csv("impTrain.csv", startPos, nextToEndPos, buffer1, false);
    unsigned int maxRows2 = read_csv("impTest.csv",  startPos, nextToEndPos, buffer2, false);
    //maxRows1=10000;
    //maxRows2=10000;

    unsigned int maxColumns = nextToEndPos-startPos;
    printf(" done reading %d and %d lines\n",maxRows1,maxRows2);

    int dimensions1[2] = {(int)maxRows1,(int)maxColumns};
    int dimensions2[2] = {(int)maxRows2,(int)maxColumns};
    int grouping[maxRows1+maxRows2];
    bzero(grouping,sizeof(grouping));
    crossCorrelations(dimensions1, buffer1, dimensions2, buffer2, grouping);

    const double (*series1)[maxColumns] = (const double (*)[maxColumns])buffer1;
    const double (*series2)[maxColumns] = (const double (*)[maxColumns])buffer2;

    ofstream out1("group1.csv");
    for(unsigned int row1=0; row1<maxRows1; row1++){
        out1<<grouping[row1];
        if( grouping[row1] ){
            unsigned int row2 = grouping[row1] - 1;
            out1<<","<< cor(series1[row1],cachedMean1[row1],cachedSd1[row1],
                            series2[row2],cachedMean2[row2],cachedSd2[row2], maxColumns );
        }
        out1<<endl;
    }
    out1.close();

    ofstream out2("group2.csv");
    for(unsigned int row2=0; row2<maxRows2; row2++){
        out2<<grouping[maxRows1+row2];
        if( grouping[maxRows1+row2] ){
            unsigned int row1 = grouping[maxRows1+row2] - 1;
            out2<<","<< cor(series1[row1],cachedMean1[row1],cachedSd1[row1],
                            series2[row2],cachedMean2[row2],cachedSd2[row2], maxColumns );
        }
        out2<<endl;
    }
    out2.close();

    return(0);
}
