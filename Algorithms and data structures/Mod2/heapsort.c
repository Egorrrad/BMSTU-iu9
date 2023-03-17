#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>


void swap(char *e1, char *e2, size_t width)
{
	int i=0;
	char per;
      while(i<(int)width){
		per=e1[i];
		e1[i]=e2[i];
		e2[i]=per;
		i+=1;
	  }
}
int compare(const void *a, const void *b)
{
      char **s1, **s2;
      s1=(char**)a;
      s2=(char**)b;
      int c1,c2, lens, lenss;
      c1=c2 = 0;
      int i=0;
      lens=strlen(*s1);
      lenss=strlen(*s2);
      while(i<lens)
      {
            if ((*s1)[i] == 'a')
            {
                  c1 += 1;
            }
            i+=1;
      }
      i=0;
      while(i<lenss)
      {
            if ((*s2)[i] == 'a')
            {
                  c2 += 1;
            }
            i+=1;
      }
      return c1-c2;
}
void heapify(int i, int nel, int width, void *base, int (*compare)(const void *a, const void *b))
{
      int l, r, j;
      int b = 0;
      while (1)
      {
            l = 2 * i + 1;
            r = l + 1;
            j = i;
            if (l < nel && compare((char*)base + i * width , (char*)base + l * width) < 0)
                  i = l;
            if (r < nel && compare((char*)base + i * width, (char*)base + r * width) < 0)
                  i = r;
            if (i == j)
            {
                  break;
            }
            swap((char*)base + i * width, (char*)base + j * width, width);
      }
}

void buildheap(void *base, int n, int width, int (*compare)(const void *a, const void *b))
{
      int i;
      i = (n / 2) - 1;
      while (i >= 0)
      {
            heapify(i, n, width, base, compare);
            i = i - 1;
      }
}

void hsort(void *base, size_t nel, size_t width,
           int (*compare)(const void *a, const void *b))
{
      int i;
      buildheap(base, nel, width, compare);
      i = nel - 1;
      while (i > 0)
      {
            swap((char*)base + i * width, (char*)base, width);
            heapify(0, i, width, base, compare);
            i = i - 1;
      }
}

int main()
{
	char **string;
      int size;
      scanf("%d", &size);
      string = malloc(sizeof(char *) * size);
      for (int i = 0; i < size; i++)
      {
            string[i] = malloc(sizeof(char) * 1001);
            scanf("%s", string[i]);
      }
      
      hsort(string, size, sizeof(char*), compare);
      for (int i = 0; i < size; i++)
      {
            printf("%s\n", string[i]);
      }
      for (int i=0; i<size; i++){
            free(string[i]);
      }
      free(string);
      return 0;
}


//++++++