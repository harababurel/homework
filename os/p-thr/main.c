/* Implement a program that creates two threads
 * The threads will print their ID (pthread_self) 10 times and then stop.
 * Ensure that the printed IDs alternate always (ie A, B, A, B, ...).
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

pthread_t T0, T1;
pthread_mutex_t m[2];

void *f(void *p) {
    int id = *(int *) p;

    for(int i=0; i<10; i++) {
        pthread_mutex_lock(&m[id]);
        printf("%ld %d\n", pthread_self(), id);
        pthread_mutex_unlock(&m[1-id]);
    }

    return NULL;
}

int main() {
    pthread_mutex_init(&m[0], NULL);
    pthread_mutex_init(&m[1], NULL);

    int *arg0 = malloc(sizeof(int));
    int *arg1 = malloc(sizeof(int));

    *arg0 = 0;
    *arg1 = 1;

    pthread_mutex_lock(&m[1]);

    pthread_create(&T0, NULL, f, arg0);
    pthread_create(&T1, NULL, f, arg1);

    pthread_join(T0, NULL);
    pthread_join(T1, NULL);

    return 0;
}
