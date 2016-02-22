#include <stdio.h>
#define nmax 1000005

char prime[nmax];

typedef struct {
    /* val[i] = the i-th value of the vector
     * dp[i]  = the size of the longest increasing contiguous subsequence that starts at position i
     * size   = number of elements
     */

    int val[nmax];
    int dp[nmax];
    int size;
} vector;

vector v;

int gcd(int a, int b) {
    /* Method recursively computes the greatest common divisor.
     * Input: two positive integers, a and b.
     * Output: an integer equal to the gcd of a and b.
     */

    if(b==0) return a;
    return (b, a%b);
}

void sieve(char prime[], int n) {
    /* Method determines prime numbers in the interval [2, n]
     * using the Sieve of Eratosthenes.
     *
     * Input: an array of chars prime[] (can contain any values, as they will be overwritten)
     *        an integer n
     *
     * Output: the array prime[] will be constructed as follows:
     *         - prime[x] = 1, if x is a prime number
     *         - prime[x] = 0, otherwise
     *         for any x in [2, n].
     */

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
        printf("You chose option (a).\n\nn = ");        // prompt for n
        scanf("%d", &n);                                // read it

        sieve(prime, n);                                // find the primes

        printf("Prime numbers: ");
        for(int i=2; i<n; i++)
            if(prime[i])
                printf("%d ", i);                       // show them
        printf("\n");

    }
    else if(option == 'b') {
        printf("You chose option (b).\n");

        printf("n = ");                                 // prompt for n
        scanf("%d", &v.size);                           // read it

        printf("values (separated by space): ");
        for(int i=1; i<=v.size; i++)
            scanf("%d", &v.val[i]);                     // read the vector

        v.dp[v.size] = 1;                               // dp[n] = 1, as there is only one sequence that starts on the last position
        for(int i=v.size-1; i; --i)
            v.dp[i] = (v.val[i] < v.val[i+1]? v.dp[i+1]:0) + 1;

        int best_dp = 0, start = 0;
        for(int i=1; i<=v.size; i++)
            if(v.dp[i] > best_dp) {
                best_dp = v.dp[i];
                start = i;
            }

        printf("Longest increasing contiguous subsequence: ");
        for(int i=0; i<best_dp; ++i)
            printf("%d ", v.val[start+i]);
        printf("\n");

    }
    else printf("Invalid option.\n");

}
