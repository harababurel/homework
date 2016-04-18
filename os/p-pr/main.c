/* The main process reads from standard input numbers as long as the user
 * enters positive numbers. For each N number it will launch a new process that
 * will double the number DN and creates a new process. The new process will
 * establish the Fibonacci number found on the DN position. Eg. for N=4, ND=8
 * the Fibonacci number is 21. The Fibonacci number will be send as a response,
 * using a pipe channel, to the main process. In order to establish the
 * Fibonacci number on a given place the subprocess will use a shell script
 * launched with popen.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>

int main() {
    int x;
    int back2root[2];

    while(scanf("%d", &x)) {
        if(x < 0)
            return 0;

        printf("parent has read x = %d.\n", x);

        pipe(back2root);

        if(fork() == 0) {
            close(back2root[0]);

            int dx = 2 * x;
            printf("child has computed dx = %d.\n", dx);

            if(fork() == 0) {
                printf("grandson is busy generating fibo(%d).\n", dx);

                char *command = malloc(30 * sizeof(char));
                char *nstr = malloc(10 * sizeof(char));
                sprintf(nstr, "%d", dx);

                strcpy(command, "./fibo.sh ");
                strcat(command, nstr);

                FILE *p = popen(command, "r");
                if(p == NULL) {
                    printf("something went wrong when running popen().\n");
                    exit(1);
                }

                free(nstr);
                free(command);

                char c;
                while(fread(&c, 1, 1, p))
                    write(back2root[1], &c, 1);

                fclose(p);
                close(back2root[1]);

                exit(0);
            }

            exit(0);
        }
        else {
            close(back2root[1]);

            printf("parent has read from grandchild: ");

            char c;
            while(read(back2root[0], &c, 1))
                if(c != 10) // no newline please
                    printf("%c", c);
            printf(".\n");

            wait(0);
            wait(0);
        }
    }

    return 0;
}
