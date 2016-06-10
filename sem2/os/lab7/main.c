#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>


int main(int argc, char* argv[]) {
    char* filename = malloc(20 * sizeof(char));

    if(filename == NULL) {
        printf("Could not allocate memory for filename.\n");
        return 1;
    }

    printf("filename: ");
    scanf("%s", filename);
    printf("filename is <%s>\n", filename);

    int f = open(filename, O_RDONLY);

    if(f == -1) {
        printf("Could not open file.\n");
        return 1;
    }

    char key;


    printf("Key: ");
    scanf("%s", &key);
    printf("Key is <%c>.\n", key);



    int pipefd[2];
    if(pipe(pipefd) == -1) {
        printf("Could not open pipe.\n");
        return 1;
    }

    pid_t cpid = fork();
    if(cpid == -1) {
        printf("Could not fork.\n");
        return 1;
    }

    if(cpid == 0) { // child
        printf("I'm child.\n");
    }
    else {
        printf("I'm dad.\n");
//        write(pipefd[1], 

        char buff;
        while(read(f, &buff, 1) > 0)
            printf("read character <%c>. sending it to child\n", buff);

        write(pipefd[1], &buff, 1);
    }



    return 0;
}
