#include <stdio.h>

struct Data {
    int x;
    int y;
};

int process(int a, int b, int* arr, struct Data* d) {
    int result;
    if (a > b) {
        result = arr[0] * arr[1] + d->x;
    } else {
        result = arr[0] / arr[1] - d->y;
    }
    
    return result;
}

int main() {
    int array[2] = {10, 5};
    struct Data data = {3, 2};
    
    printf("Result: %d\n", process(8, 5, array, &data));
    int x = 1;
    float y = 1.0;
    if (x > 0) {
        goto my_label;
    }
    my_label:
    return 0;
}