"""
(17)
Statement:
    Generate the largest perfect number smaller than a given natural number n.
    If such a number does not exist, a message should be displayed.
    A number is perfect if it is equal to the sum of its divisors, except
    itself.
    E.g. 6 is a perfect number (6=1+2+3).

Idea:
    All even perfect numbers are generated with the formula 2^(p-1) * (2^p - 1)
    if both p and 2^p - 1 are primes.
    It is not known whether any odd perfect numbers exist.

    Therefore we can:
        1. iterate through the first prime numbers (=p)
        2. check whether 2^p - 1 is prime
        3. generate a new perfect number if condition #2 holds

    Repeat the process until we get a perfect number larger than n.
    At this point, the previous number generated is the answer.
"""

from math import sqrt
nmax = 100


def getNextSmallPrime():
    """
    Generator function that yields, one by one,
    all prime numbers up to nmax.
    Uses Eratosthenes' Sieve algorithm.
    """
    prime = [True for i in range(0, nmax)]

    for i in range(2, nmax):
        if not prime[i]:                # if i is a prime
            continue                    # -> its multiples are not
        for j in range(2*i, nmax, i):   # mark them
            prime[j] = False

        yield i                         # and then yield the current prime


def isPrime(x):
    """
    Checks whether or not x is a prime number
    in O(sqrt(x)).
    """
    for i in range(2, int(sqrt(x))+1):  # if there is a divisor of x
        if x % i == 0:                  # then x is not prime
            return False

    return True                         # otherwise, it is


def getNextPerfectNumber():
    """
    Generator function that yields, one by one,
    all even perfect numbers.
    """
    for p in getNextSmallPrime():       # iterate through primes
        if isPrime(2**p-1):             # if 2**p-1 is also prime
            yield 2**(p-1) * (2**p-1)   # then we found a perfect number


def getInput():
    """
    Method that validates user input.
    """
    try:
        return int(input("Please input an integer: "))
    except:
        print("You did not enter a valid integer.")


def main():
    n = None
    while n is None:                    # request user input
        n = getInput()                  # until something valid is provided

    previous = 0
    for x in getNextPerfectNumber():    # iterate all perfect numbers
        if x >= n:                      # stop at the first one greater than
            break                       # or equal to n
        previous = x                    # and store the previous one

    if not previous:                    # output the answer
        print("There is no such number.")
    else:                               # or a corresponding message
        print("The largest perfect number smaller than %i is %i."
              % (n, previous))


if __name__ == '__main__':
    main()
