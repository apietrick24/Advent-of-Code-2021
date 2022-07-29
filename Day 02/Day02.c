#include<stdio.h>
#include<stdlib.h>
#include<string.h>

size_t MAX_LINE_LENGTH = 80;

int main(void){
    FILE *fp;
    fp = fopen("P1 Input", "r");

    char * line = NULL;
    ssize_t read; 

    int horizontal = 0;
    int depth = 0;
    int aim = 0;

    while ((read = getline(&line, &MAX_LINE_LENGTH, fp)) != -1) {
        char* direction = strtok(line, " ");
        int magnitude = atoi(strtok(NULL, " "));

        if(strcmp(direction, "up") == 0){
            aim-=magnitude;
        } else if (strcmp(direction, "down") == 0){
            aim+=magnitude;
        } else if (strcmp(direction, "forward") == 0){
            horizontal+=magnitude;
            depth+=(aim*magnitude);
        }
    }
    
    printf("P1: %d\n", aim*horizontal);
    printf("P2: %d\n", depth*horizontal);

    return 0;
}