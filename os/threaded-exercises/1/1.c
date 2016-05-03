// https://www.youtube.com/watch?v=GXXE42bkqQk
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

const long long LOOPS = 500000000;
long long sum = 0;

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *counter_function(void *arg) {

    int offset = *(int *) arg;
    for(long long i=0; i<LOOPS; i++) {
        pthread_mutex_lock(&mutex);
        sum += offset;
        pthread_mutex_unlock(&mutex);
    }

    return NULL;
}


int main() {
    int *offsets = malloc(2*sizeof(int));
    *offsets = 1;
    *(offsets+1) = -1;

    //pthread_mutex_init(&mutex, NULL);

    pthread_t id1, id2;
    pthread_create(&id1, NULL, counter_function, offsets);
    pthread_create(&id2, NULL, counter_function, 1+offsets);

    pthread_join(id1, NULL);
    pthread_join(id2, NULL);

    printf("%lld\n", sum);

    return 0;
}
