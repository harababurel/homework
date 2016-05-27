#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

int xs[10][100];
int sum[10];
pthread_t threads[7];
pthread_mutex_t mutexes[10];

// xs[S][0] = cate numere am generat cu ultima cifra S

void *solve(void *arg) {
    while(1) {
        pthread_mutex_lock(&mutexes[5]);
        if(xs[5][0] >= 5) {
            pthread_mutex_unlock(&mutexes[5]);
            return NULL;
        }
        pthread_mutex_unlock(&mutexes[5]);

        int x = rand() % 101;

        pthread_mutex_lock(&mutexes[x%10]);
        xs[x%10][++xs[x%10][0]] = x;
        printf("%d ", x);
        pthread_mutex_unlock(&mutexes[x%10]);
    }

    return NULL;
}

int main() {
    srand((unsigned)time(0)^getpid());

    for(int i=0; i<10; i++)
        pthread_mutex_init(&mutexes[i], NULL);

    for(int i=0; i<7; i++)
        pthread_create(&threads[i], NULL, *solve, NULL);

    for(int i=0; i<7; i++)
        pthread_join(threads[i], NULL);

    printf("\n");

    for(int s=0; s<10; s++) {
        int total = 0;

        printf("SUM[%d]=", s);
        for(int i=1; i<=xs[s][0]; i++) {
            printf("%d", xs[s][i]);

            if(xs[s][0] > 1)
                printf((i==xs[s][0]? "=":"+"));
            total += xs[s][i];
        }

        if(xs[s][0] != 1)
            printf("%d", total);
        printf("\n");
    }

    return 0;
}

