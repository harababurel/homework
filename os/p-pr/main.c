#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>

int x;

int main() {

    while(scanf("%d", &x)) {
        if(x < 0)
            return 0;

        printf("x = %d\n", x);

        int pid = fork();
        if(pid == 0) { // child
            int dx = 2 * x;
            printf("child has computed dx = %d\n", dx);

            int fd[2];  // create the communication channel
            pipe(fd);

            write(fd[1], &dx, sizeof(int));

            int pid = fork();
            if(pid == 0) {
                int n;
                read(fd[0], &n, sizeof(int));

                printf("the grandson will be busy generating fibo(%d)\n", n);

                /*
                int a=1, b=1, c;
                for(int i=3; i<=n; i++) {
                    c = a+b;
                    b = a;
                    a = c;
                }*/

                execl("/bin/bash", "/bin/bash", "fibo.sh", "3");

                // ^TODO: make this communicate properly


                //write(fd[1], &c, sizeof(int));
                exit(0);
            }

            wait(0);


            int result;
            read(fd[0], &result, sizeof(int));
            printf("result from grandson is %d", result);

            exit(0);
        }

        wait(0);

    }

    return 0;
}
