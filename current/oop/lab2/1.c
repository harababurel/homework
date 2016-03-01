#include <stdio.h>
#include <stdbool.h>
#define nmax 10000

typedef struct {
    /* val[i] = the i-th value of the vector
     * dp[i]  = the size of the longest increasing contiguous subsequence that starts at position i
     * size   = number of elements
     */

    int val[nmax];
    int dp[nmax];
    int size;
} vector;

void print_vector(vector v) {
    /* Method prints the values of a vector, separated by spaces.
     */

    for(int i=1; i<=v.size; i++)
        printf("%d ", v.val[i]);
    printf("\n");
}

vector read_vector() {
    /* Method reads the values of a vector, and returns the constructed vector.
     */

    vector ret;

    scanf("%d", &ret.size);
    printf("values (separated by space): ");

    for(int i=1; i<=ret.size; i++)
        scanf("%d", &ret.val[i]);

    return ret;
}

vector sieve(int n) {
    /* Method determines and returns a vector containing all prime numbers
     * in the interval [2, n], using the Sieve of Eratosthenes.
     *
     * Input: an integer n.
     *
     * Output: a vector containing prime numbers less than n.
     */

    bool prime[nmax];

    for(int i=2; i<=n; i++)
        prime[i] = true;

    for(int i=2; i<=n; i++)
        for(int j=i*2; j<=n; j+=i)
            prime[j] = false;

    vector ret;
    ret.size = 0;
    for(int i=2; i<n; i++)
        if(prime[i])
            ret.val[++ret.size] = i;

    return ret;
}

vector get_longest_increasing_subsequence(vector v) {
    /*
     * In order to compute dp[i], two cases may arise:
     *     1. v[i] < v[i+1]
     *        In this case, the i-th and (i+1)-th elements can be used in the same subsequence,
     *        so the i-th is prepended to the best subsequence that starts with i+1.
     *        Recurrence: dp[i] = 1 + dp[i+1]
     *
     *     2. v[i] >= v[i+1]
     *        In this case, the i-th and (i+1)-th elements cannot be used in the same subsequence,
     *        so we create a single-element subsequence, containing the i-th element.
     *        Recurrence: dp[i] = 1
     *
     * Additionally, dp[n] = 1, as there is only one sequence that starts on the last position.
     *
     * Input: a vector to be processed
     * Output: a newly constructed vector that contains the longest increasing contiguous subsequence of the input vector.
     */

    v.dp[v.size] = 1;

    for(int i=v.size-1; i; --i)
        v.dp[i] = (v.val[i] < v.val[i+1]? v.dp[i+1]:0) + 1;

    int best_dp = 0, start = 0;                     // find the size of the longest dp
    for(int i=1; i<=v.size; i++)                    // and the starting position
        if(v.dp[i] > best_dp) {
            best_dp = v.dp[i];
            start = i;
        }

    vector ret;
    ret.size = 0;

    for(int i=0; i<best_dp; ++i)
        ret.val[++ret.size] = v.val[start+i];

    return ret;
}

void option_a() {
    /* Method solves the first subproblem:
     * Generate all the prime numbers smaller than a given natural number n.
     */

    int n;
    scanf("%d", &n);

    printf("Prime numbers: ");
    print_vector(sieve(n));
}

void option_b() {
    /* Method solves the second subproblem:
     * Given a vector of numbers, find the longest increasing contiguous subsequence.
     */

    vector v = read_vector();
    printf("Longest increasing contiguous subsequence: ");
    print_vector(get_longest_increasing_subsequence(v));
}

void main_menu() {
    /* Method displays the user interface.
     */

    printf("(a) Generate all the prime numbers smaller than a given natural number n.\n");
    printf("(b) Given a vector of numbers, find the longest increasing contiguous subsequence.\n");

    char option;

    printf("Option: ");
    scanf("%c", &option);

    if(option == 'a') {
        printf("You chose option (a).\n\nn = ");        // prompt for n
        option_a();
    }
    else if(option == 'b') {
        printf("You chose option (b).\n\nn = ");        // prompt for n
        option_b();
    }
    else printf("Invalid option.\n");
}

void main() {
    main_menu();
}
