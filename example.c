#include <stdio.h>

int blah(int *args, int count) {
	for(int i = 0; i < count; ++i) {
		printf("%d\n", args[i]);
	}
	return 0;
}

int main() {
	// Needs at least C99
	blah((int[]){ 0, 1, 2 }, 3);
	return 0;
}