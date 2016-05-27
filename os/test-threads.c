
/* 8. Sa se scrie un program care creeaza 7 thread-uri. 
 * Fiecare thread va genera numere aleatoare intre 0 si 500,
 * verifica daca numarul contine cifrele 2, 3 si/sau 5,
 * incrementeaza contorul global corespunzator si printeaza
 * valoarea obtinuta. Thread-urile se opresc cand s-au generat
 * cel putin 3 cifre din fiecare categorie.
 * Programul principal afiseaza cele 3 variabile globale si
 * apoi se termina.

   EX: ./prog4

    Numere generate: 2 15 35 50 420 33 70 10 12 54 93 105
    Cifre de 2: 3
    Cifre de 3: 4
    Cifre de 5: 5
 */

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

pthread_t thread_ids[8];
pthread_mutex_t twos_mutex, threes_mutex, fives_mutex;
int twos, threes, fives;

void *f(void *arg) {

    while(true) {
        pthread_mutex_lock(&twos_mutex);
        pthread_mutex_lock(&threes_mutex);
        pthread_mutex_lock(&fives_mutex);
        bool done = (twos >= 3 && threes >= 3 && fives >= 3);
        pthread_mutex_unlock(&fives_mutex);
        pthread_mutex_unlock(&threes_mutex);
        pthread_mutex_unlock(&twos_mutex);

        if(done) {
            printf("Thread finished.\n");
            return NULL;
        }

        int a = rand() % 6;
        int b = rand() % 10;    // x = ''.join([a, b, c])
        int c = rand() % 10;

        printf("Generated =====> %d%d%d\n", a, b, c);

        if(a == 2 || b == 2 || c == 2) {
            pthread_mutex_lock(&twos_mutex);
            twos++;
            printf("Got another 2\n");
            pthread_mutex_unlock(&twos_mutex);
        }
        if(a == 3 || b == 3 || c == 3) {
            pthread_mutex_lock(&threes_mutex);
            threes++;
            printf("Got another 3\n");
            pthread_mutex_unlock(&threes_mutex);
        }
        if(a == 5 || b == 5 || c == 5) {
            pthread_mutex_lock(&fives_mutex);
            fives++;
            printf("Got another 5\n");
            pthread_mutex_unlock(&fives_mutex);
        }
    }

    return NULL;
}

int main() {
    srand((unsigned)time(0));
    pthread_mutex_init(&twos_mutex, NULL);
    pthread_mutex_init(&threes_mutex, NULL);
    pthread_mutex_init(&fives_mutex, NULL);

    for(int i=0; i<7; i++)
        pthread_create(&thread_ids[i], NULL, f, NULL);

    for(int i=0; i<7; i++)
        pthread_join(thread_ids[i], NULL);

    printf("twos:   %d\n", twos);
    printf("threes: %d\n", threes);
    printf("fives:  %d\n", fives);


    return 0;
}

