/* ...R2...
 * Write a program which creates 2 child processes. The parent sends to its
 * children its own pid through fifos. Each child sends to the other one its
 * own pid and 0 if the difference between its own pid and parent pid is less
 * than 2 and 1 otherwise.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <math.h>

int main() {
    mkfifo("p2a", 0700);
    mkfifo("p2b", 0700);
    mkfifo("a2b", 0700);
    mkfifo("b2a", 0700);

    if(fork() == 0) {
        pid_t parent_pid, a_pid = getpid(), other_pid;

        int p2a = open("p2a", O_RDONLY);
        read(p2a, &parent_pid, sizeof(parent_pid));
        close(p2a);

        int diff = abs(a_pid - parent_pid) >= 2, other_diff;

        int pout = open("a2b", O_WRONLY);
        int pin = open("b2a", O_RDONLY);

        write(pout, &a_pid, sizeof(pid_t));
        write(pout, &diff, sizeof(int));
        close(pout);

        read(pin, &other_pid, sizeof(other_pid));
        read(pin, &other_diff, sizeof(other_diff));
        close(pin);

        printf("A received %d from parent, and (%d, %d) from B.\n",
                parent_pid,
                other_pid,
                other_diff);
        exit(0);
    }
    else if(fork() == 0) {
        pid_t parent_pid, b_pid = getpid(), other_pid;

        int p2b = open("p2b", O_RDONLY);
        read(p2b, &parent_pid, sizeof(parent_pid));
        close(p2b);

        int diff = abs(b_pid - parent_pid) >= 2, other_diff;

        int pin = open("a2b", O_RDONLY);  // these 2 must be reversed
        int pout = open("b2a", O_WRONLY); // (A opens out-in, B opens in-out)

        write(pout, &b_pid, sizeof(pid_t));
        write(pout, &diff, sizeof(int));
        close(pout);

        read(pin, &other_pid, sizeof(other_pid));
        read(pin, &other_diff, sizeof(other_diff));
        close(pin);

        printf("B received %d from parent, and (%d, %d) from A.\n",
                parent_pid,
                other_pid,
                other_diff);
        exit(0);
    }
    else {
        pid_t parent_pid = getpid();

        int p2a = open("p2a", O_WRONLY);
        int p2b = open("p2b", O_WRONLY);

        write(p2a, &parent_pid, sizeof(parent_pid));
        write(p2b, &parent_pid, sizeof(parent_pid));

        close(p2a);
        close(p2b);

        wait(0);
        wait(0);
    }

    return 0;
}
