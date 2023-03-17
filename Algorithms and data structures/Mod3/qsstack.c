#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct
{
      int low;
      int high;
} task;

struct stak
{
      task *data;
      long top;
} stack;

void push(task x)
{
      stack.data[stack.top] = x;
      stack.top += 1;
}

task pop()
{
      task x;
      stack.top -= 1;
      x = stack.data[stack.top];
      return x;
}
int empty()
{
      if (stack.top == 0)
      {
            return 1;
      }
      return 0;
}
void swap(int *p, int i, int j)
{
      int perem;
      perem = p[i];
      p[i] = p[j];
      p[j] = perem;
}
int partition(int *arr, int low, int high)
{
      int r, i, j;
      r = arr[high];
      i = low - 1;
      j = low;
      while (j < high)
      {
            if (arr[j] < r)
            {
                  i += 1;
                  swap(arr, i, j);
            }
            j += 1;
      }
      swap(arr, i + 1, high);
      return i + 1;
}
void quickSort(int *arr, int l, int h)
{
      int low, high, m;
      m = partition(arr, l, h);
      task t;
      if (m - 1 > l)
      {
            t.low=l;
            t.high=m-1;
            push(t);
      }
      if (m + 1 < h)
      {
            t.low=m+1;
            t.high=h;
            push(t);
      }
      while (empty() == 0)
      {
            t=pop();
            high = t.high;
            low = t.low;
            m = partition(arr, low, high);
            if (m - 1 > low)
            {
                  t.low=low;
                  t.high=m-1;
                  push(t);
            }
            if (m + 1 < high)
            {
                  t.low=m+1;
                  t.high=high;
                  push(t);
            }
      }
}

int main()
{
      int n, i;
      scanf("%d", &n);
      int *arr;
      arr = malloc(sizeof(int) * n);
      stack.data = malloc(sizeof(task) * n);
      for (i = 0; i < n; i++)
      {
            scanf("%d", &arr[i]);
      }
      quickSort(arr, 0, n - 1);
      for (i = 0; i < n; i++)
      {
            printf("%d ", arr[i]);
      }
      free(stack.data);
      free(arr);
      return 0;
}

//++++