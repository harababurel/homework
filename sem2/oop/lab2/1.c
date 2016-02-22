#include <stdio.h>
#define nmax 1000005

char prime[nmax];

/*
typedef struct {
    int val[nmax];
    int size;
} vector;
*/

int cmmdc(int a, int b) {
    if(b==0) return a;
    return cmmdc(b, a%b);
}

void sieve(char prime[], int n) {

    for(int i=2; i<=n; i++)
        prime[i] = 1;

    for(int i=2; i<=n; i++)
        for(int j=i*2; j<=n; j+=i)
            prime[j] = 0;
}


void main() {
    printf("(a) Generate all the prime numbers smaller than a given natural number n.\n");
    printf("(b) Given a vector of numbers, find the longest increasing contiguous subsequence.\n");

    char option;
    int n;

    printf("Option: ");
    scanf("%c", &option);

    if(option == 'a') {
        printf("You chose option (a).\n");
        printf("\n");

        printf("n = ");
        scanf("%d", &n);

        sieve(prime, n);

        printf("Prime numbers: ");
        for(int i=2; i<n; i++)
            if(prime[i])
                printf("%d ", i);
        printf("\n");

    }
    else if(option == 'b') {
        printf("You chose option (b).\n");
    }
    else {
        printf("Invalid option.");
    }



    sieve(prime, 10);
}
