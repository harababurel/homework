nmax = 10**5

n = int(input())

isPrime = [True for i in range(0, nmax)]
for i in range(2, nmax):
    if isPrime[i]:
        for j in range(i*2, nmax, i):
            isPrime[j] = False

while not isPrime[n+1]:
    n += 1

print(n+1)
