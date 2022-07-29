#include<stdio.h>
#include<stdlib.h>

size_t MAX_LINE_LENGTH = 80;

int main(void){
    FILE *fp;
    fp = fopen("P1 Input", "r");

    if (fp == NULL){
        exit(69);
    }

    char * line = NULL;
    ssize_t read;

    int count = 0;
    int last = 0;
    int middle = 0;
    int first = 0;

    int part1 = 0;
    int part2 = 0;
    
    while ((read = getline(&line, &MAX_LINE_LENGTH, fp)) != -1) {
        int current = atoi(line);

        if (count >= 1 && current > first){
            part1++;
        }

        if (count >= 3 && (atoi(line) + first + middle) > (first + middle + last)){
            part2++;
        }

        last = middle;
        middle = first;
        first = current;  
        count++;
    }

    fclose(fp);

	char text[20]; 

    sprintf(text, "%d", part1);

	printf("P1: %s", text);

    sprintf(text, "%d", part2);

	printf("\nP2: %s", text);


    return 0;
}