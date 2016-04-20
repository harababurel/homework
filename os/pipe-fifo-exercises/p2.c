/* Problema 2
 * Sa se implementeze un proces care creeaza un proces fiu cu care comunica
 * prin pipe. Procesul parinte trimite prin pipe procesului fiu un string iar
 * fiul returneaza prin pipe cate vocale are stringul primit. La final
 * verificati dealocarea corecta a memoriei.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>

#define MAX_BUFF_SIZE 256


bool is_vowel(char c) {
    return (strchr("aeiouAEIOU", (int)c));
}

int main() {
    int p2c[2], c2p[2];

    pipe(p2c);
    pipe(c2p);

    if(fork() == 0) {
        close(p2c[1]);
        close(c2p[0]);

        unsigned long buff_size;
        read(p2c[0], &buff_size, sizeof(buff_size));

        int vowels = 0;
        char c;
        for(int i=0; i<buff_size; i++) {
            read(p2c[0], &c, sizeof(char));
            vowels += is_vowel(c);
        }

        write(c2p[1], &vowels, sizeof(vowels));

        close(c2p[1]);
        close(p2c[0]);

    } else {
        close(c2p[1]);
        close(p2c[0]);

        char *s = malloc(MAX_BUFF_SIZE * sizeof(char));
        fgets(s, MAX_BUFF_SIZE, stdin);

        unsigned long buff_size = strlen(s);

        write(p2c[1], &buff_size, sizeof(buff_size));
        for(int i=0; i<buff_size; i++)
            write(p2c[1], &s[i], sizeof(char));

        free(s);

        int vowels;
        read(c2p[0], &vowels, sizeof(vowels));

        printf("there are %d vowels.\n", vowels);

        close(p2c[1]);
        close(c2p[0]);
    }

    return 0;
}
