#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "3dSqueakyToy.h"	/* for defining frames of ASCII art */

void squeak_wait(double seconds) {
	clock_t start = clock();
	while ((double)(clock() - start) / CLOCKS_PER_SEC < seconds) {};
	return;
}

void wag_tail(int n) {
	double gap = 0.1;
	char sequence[][57] = {
		SQUEAKYTOY_BASELINE,
		SQUEAKYTOY_WAG1,
		SQUEAKYTOY_WAG2,
		SQUEAKYTOY_WAG3
	};
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < 4; ++j) {
			system("clear");
			for (int k = 0; k < 57; ++k) putchar(sequence[j][k]);
			squeak_wait(gap);
		}
	}
}

void dog_woo(int n) {
	double gap = 0.1;
	char sequence[][57] = {
		SQUEAKYTOY_BASELINE,
		SQUEAKYTOY_WOO1,
		SQUEAKYTOY_WOO2,
		SQUEAKYTOY_WOO3,
		SQUEAKYTOY_WOO4,
		SQUEAKYTOY_WOO5,
		SQUEAKYTOY_WOO6,
		SQUEAKYTOY_WOO7,
		SQUEAKYTOY_WOO8,
		SQUEAKYTOY_BASELINE,
		SQUEAKYTOY_BASELINE,
		SQUEAKYTOY_BASELINE,
	};
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < 11; ++j) {
			system("clear");
			for (int k = 0; k < 57; ++k) putchar(sequence[j][k]);
			squeak_wait(gap);
		}
	}
}

void dog_squeak(int n) {
	double gap = 0.25;
	char sequence[][57] = {
		SQUEAKYTOY_BASELINE,
		SQUEAKYTOY_SQUEAK
	};
	for (int i = 0; i < n; ++i) {
		for (int j = 0; j < 2; ++j) {
			system("clear");
			for (int k = 0; k < 57; ++k) putchar(sequence[j][k]);
			squeak_wait(gap);
		}
	}
}



char HELP_3DSQUEAKYTOY[] = "\
3dSqueakyToy \n\
Usage: 3dSqueakyToy TYPE\n\
    Clears the screen and creates a cute ASCII art animation.\n\
    Valid types are: squeak, wag, woo.\n\
";

int main(int argc, char ** argv) {
	int num_cycles = 10;
	if (argc != 2) {
		fprintf(
			stderr,
			"You must provide just a type\nSee 3dSqueakyToy -h for help\n"
		);
		return 1;
	}
	else if (strncmp(argv[1], "-h", 2) == 0) {
		printf("%s", HELP_3DSQUEAKYTOY);
	}
	else if (strncmp(argv[1], "squeak", 6) == 0) {
		dog_squeak(num_cycles);
	}
	else if (strncmp(argv[1], "wag", 3) == 0) {
		wag_tail(num_cycles);
	}
	else if (strncmp(argv[1], "woo", 3) == 0) {
		dog_woo(num_cycles);
	}
	else {
		fprintf(
			stderr,
			"You must provide a valid type\nSee 3dSqueakyToy -h for help\n"
		);
		return 1;
	}
	return 0;
}
