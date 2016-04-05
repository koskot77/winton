#include <stdio.h>   // printf, fopen, fclose, getline ...
#include <stdlib.h>  // strtod
#include <string.h>  // bzero

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
