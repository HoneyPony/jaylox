#include <stdio.h>

double fib(double n) {
  if (n <= 1) return n;
  return fib(n - 2) + fib(n - 1);
}

int main() {
	for (double i = 0; i < 40; i = i + 1) {
		printf("%f\n", fib(i));
	}
}