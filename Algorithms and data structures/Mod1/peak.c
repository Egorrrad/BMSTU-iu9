#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int array[8] = {1, 2, 3, 6, 4, 6, 2, -5};
// int array[1]={1};

int less(unsigned long i, unsigned long j)
{
	if (array[i] < array[j])
	{
		return 1;
	}
	return 0;
}

unsigned long peak(unsigned long nel,
				   int (*less)(unsigned long i, unsigned long j))
{
	unsigned long k, x = 0;
	if (nel == 1)
	{
		return 0;
	}
	if (nel==2){
		if(less(0,1)==1){
			return 1;
		}
		else{
			return 0;
		}
	}
	else
	{
		for (k = 1; k < nel - 1; k++)
		{
			if ((less(k, k + 1) == 0) && (less(k, k - 1) == 0))
			{
				x = k;
			}
		}
	}
	return x;
}

static unsigned long nel;

#define CHECK_OVERFLOW(i)                                         \
	if (i >= nel)                                                 \
	{                                                             \
		printf("\n\n%s:%d: %s(%lu): "                             \
			   "argument must be less count of elements (%lu)\n", \
			   __FILE__, __LINE__, __func__, (i), nel);           \
		exit(EXIT_FAILURE);                                       \
	}

static int even(unsigned long i)
{
	CHECK_OVERFLOW(i);
	return i % 2 == 0;
}

static int odd(unsigned long i)
{
	CHECK_OVERFLOW(i);
	return i % 2 == 1;
}

static int const_(unsigned long i)
{
	CHECK_OVERFLOW(i);
	return 42;
}

static unsigned long parabola_top;
static long parabola_ratio;

static long parabola(unsigned long i)
{
	unsigned long d = i - parabola_top;
	return 116 - d * d / parabola_ratio;
}

#define GEN_LESS(name)                                       \
	static int less_##name(unsigned long i, unsigned long j) \
	{                                                        \
		return name(i) < name(j);                            \
	}

GEN_LESS(even);
GEN_LESS(odd);
GEN_LESS(const_);
GEN_LESS(parabola);

#define NELEM(arr) (sizeof(arr) / sizeof(arr[0]))

int main()
{
	long y; /* y = f(x) */

#define TEST(expr, expected)                           \
	printf("Checking %s %s... ", #expr, #expected);    \
	y = (expr);                                        \
	if (y == (expected))                               \
		printf("ok\n");                                \
	else                                               \
	{                                                  \
		printf("fail\n\t%s == %ld, %s == %ld\n",       \
			   #expr, y, #expected, (long)(expected)); \
		return 1;                                      \
	}

	printf("even(x) := 1 if x is even\n           0 otherwise\n");
	nel = 1;
	TEST(even(peak(1, less_even)), 1);
	nel = 10;
	TEST(even(peak(10, less_even)), 1);

	printf("odd(x) := 1 if x is odd\n          0 otherwise\n");
	nel = 1;
	TEST(odd(peak(1, less_odd)), 0);
	nel = 2;
	TEST(odd(peak(2, less_odd)), 1);
	nel = 10;
	TEST(odd(peak(10, less_odd)), 1);

	printf("const_(x) := 42 for any x\n");
	nel = 1;
	TEST(const_(peak(1, less_const_)), 42);
	nel = 10;
	TEST(const_(peak(10, less_const_)), 42);

	printf("parabola(x) := 116 - (x - %lu)²/%ld\n",
		   parabola_top, parabola_ratio);
	nel = 15;
	parabola_top = 7;
	parabola_ratio = 1;
	TEST(peak(15, less_parabola), 7);

	printf("parabola(x) := 116 - (x - %lu)²/%ld\n",
		   parabola_top, parabola_ratio);
	nel = 100;
	parabola_top = 50;
	parabola_ratio = 5;
	TEST(parabola(peak(100, less_parabola)), 116);

	return 0;
}
