#include <stdlib.h>
#include <stdio.h>

struct Date
{
    int Day, Month, Year;
} Date;
struct Date *array;
void swap(int i, int j)
{
    struct Date p;
    p = array[i];
    array[i] = array[j];
    array[j] = p;
}
int main()
{
    int n;
    scanf("%d", &n);
    struct Date *days, *mounths, *years;
    days = malloc(sizeof(Date) * (n+1));    // список для ключей для дней от 1 до 31
    mounths = malloc(sizeof(Date) * (n+1)); // список для ключей для месяцев от 1 до 12
    years = malloc(sizeof(Date) * (n+1));   // список для ключей для годов от 1 до 61
    array = malloc(sizeof(Date) * n);
    for (int i = 0; i < n; i++)
    {
        scanf("%d %d %d", &array[i].Year, &array[i].Month, &array[i].Day);
    }
    // sort for days
    int num, count, c;
    count=1;
    c = 1;
    while (count < 32)
    {
        for (int k = 0; k < n; k++)
        {
            num = array[k].Day;
            if (num == count)
            {
                days[c] = array[k];
                c += 1;
            }
        }
        count+=1;
    }
    // sort for mounths
    count=1;
    c=1;
    while (count < 13)
    {
        for (int k = 1; k <= n; k++)
        {
            num = days[k].Month;
            if (num == count)
            {
                mounths[c] = days[k];
                c += 1;
            }
        }
        count+=1;
    }
    // sort for years
    count=1;
    c=1;
    while (count < 62)
    {
        for (int k = 1; k <= n; k++)
        {
            num = mounths[k].Year-1969;
            if (num == count)
            {
                years[c] = mounths[k];
                c += 1;
            }
        }
        count+=1;
    }
    for (int i = 1; i <= n; i++)
    {
        printf("%04d %02d %02d\n", years[i].Year, years[i].Month, years[i].Day);
    }
    free(array);
    free(days);
    free(mounths);
    free(years);
    return 0;
}

//++++++

/*
printf("\n");
    for (i = 1; i <=n; i++)
    {
        printf("%04d %02d %02d\n", days[i].Year, days[i].Month, days[i].Day);
    }
*/

