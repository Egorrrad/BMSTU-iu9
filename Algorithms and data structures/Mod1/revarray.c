#include <stdio.h>
#include <stdlib.h>



void revarray1(void *base, size_t nel, size_t width)
{
      char *left, *right;
      left = (char *)base;
      right = (char *)base + (nel - 1) * width;
      int i;
      char p;
      while (left<right){
            i=0;
            while(i<width){
                  p=right[i];
                  right[i]=left[i];
                  left[i]=p;
                  i++;
            }
            left+=width;
            right-=width;
      }
}
#include <stdio.h>
#include <stdlib.h>



void revarray(void *base, size_t nel, size_t width)
{
      char *s;
      s = (char*)base;
      char *left, *right;
      left = (char *)base;
      right = (char *)base + (nel - 1) * width;
      int i;
      char p;
      for (i=0; i<(nel/2)*width; i=i+width){
            p=s[i];
            s[i]=s[(nel-1)*width-i];
            s[(nel-1)*width-i]=p;
      }
}
/*

int main()
{
      // char array[43]="The quick brown fox jumps over the lazy dog";
      int array[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
      int n;
      n = 10;
      // scanf("%d", &n);
      // scanf("%s", array);
      revarray(array, n, sizeof(int));
      for (int i = 0; i < n; i++)
      {
            printf("%d\t", array[i]);
      }
}
*/
// work with numbers, but with literals not
// should to be understand


#include <stdbool.h>
#include <string.h>
extern void revarray(void *base, size_t nel, size_t width);


static void *memdup(const void *mem, size_t size)
{
	void *res = malloc(size);
	if (! res) {
		perror("memdup()");
		abort();
	}

	memcpy(res, mem, size);
	return res;
}


#define NELEM(arr) (sizeof(arr) / sizeof(arr[0]))


#define TEST(type, fmt, neq, input, output) \
	{ \
		type *array = (type *) memdup(input, sizeof(input)); \
		revarray(array, NELEM(input), sizeof(input[0])); \
		bool failed = false; \
		for (size_t i = 0; i < NELEM(output); ++i) { \
			if (neq(array[i], output[i])) { \
				printf("res[%zi]==" fmt "  !=  " #output "[%zi]==" fmt "\n", \
						i, array[i], i, output[i]); \
				failed = true; \
			} \
		} \
		free(array); \
		if (failed) exit(EXIT_FAILURE); \
	}


static const int input_int_even[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static const int output_int_even[] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };

static const int input_int_odd[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
static const int output_int_odd[] = { 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };

static const double input_double[] = {
	3.1415926535897932384626433832795,
	2.7182818284590452353602874713527,
	1.380649e-23,
	6.02214076e+23
};

static const double output_double[] = {
	6.02214076e+23,
	1.380649e-23,
	2.7182818284590452353602874713527,
	3.1415926535897932384626433832795
};

static int input_big[100500];
static int output_big[100500];

static const char input_char[43] = "The quick brown fox jumps over the lazy dog";
static const char output_char[43] = "god yzal eht revo spmuj xof nworb kciuq ehT";

static const int input_output_one[1] = { 100500 };

struct Point {
	double x;
	double y;
};

static const struct Point input_points[] = {
	{ 1.0, 2.0 },
	{ 3.1415926535897932384626433832795, 2.7182818284590452353602874713527 },
	{ 1.380649e-23, 6.02214076e+23 },
	{ 100.0, 500.0 },
};

static const struct Point output_points[] = {
	{ 100.0, 500.0 },
	{ 1.380649e-23, 6.02214076e+23 },
	{ 3.1415926535897932384626433832795, 2.7182818284590452353602874713527 },
	{ 1.0, 2.0 },
};


#define NEQ(x, y) ((x) != (y))
#define MEMNEQ(x, y) (memcmp(&(x), &(y), sizeof(x)) != 0)


int main()
{
	TEST(int, "%d", NEQ, input_int_even, output_int_even);
	TEST(int, "%d", NEQ, input_int_odd, output_int_odd);
	TEST(double, "%f", NEQ, input_double, output_double);

	for (size_t i = 0; i < NELEM(input_big); ++i) {
		input_big[i] = i;
		output_big[NELEM(output_big) - i - 1] = i;
	}
	TEST(int, "%d", NEQ, input_big, output_big);

	TEST(char, "'%c'", NEQ, input_char, output_char);
	TEST(int, "'%d'", NEQ, input_output_one, input_output_one);

	/*
	  Трюк с printf выполнен профессиональными каскадёрами!
	  Не пытайтесь повторить это самостоятельно!
	*/
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat"
	TEST(struct Point, "(%f, %f)", MEMNEQ, input_points, output_points);
#pragma GCC diagnostic pop

	return 0;
}
