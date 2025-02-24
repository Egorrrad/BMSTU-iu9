#include <iostream>
#include <vector>
#include <omp.h>
#include <cmath>

using namespace std;

int main(int argc, char* argv[]) {
    // Проверяем, передали ли параметр количества потоков
    if (argc < 2) {
        cout << "Использование: " << argv[0] << " <количество потоков>" << endl;
    }

    // Парсим количество потоков из аргументов командной строки
    int num_threads = atoi(argv[1]);
    if (num_threads <= 0) {
        cout << "Количество потоков должно быть больше 0" << endl;
    }

    // Устанавливаем количество потоков для OpenMP
    omp_set_num_threads(num_threads);

    const int N = 2 << 9; // Размерность матрицы и вектора

    // a — матрица размера N x N, где все элементы равны 1, а диагональные элементы — 2
    vector<vector<float>> a(N, vector<float>(N, 1));

    #pragma omp parallel for
    for (int i = 0; i < N; ++i) {
        a[i][i] = 2;
    }

    // b — вектор размерности N, каждый элемент которого равен N + 1
    vector<float> b(N, N + 1);

    // x — вектор решения
    vector<float> x(N, 0);

    const float epsilon = 0.000001;
    float tau = 0.1 / N;

    double start_time = omp_get_wtime(); // Замер времени начала выполнения

    float old_crit = 0;
    int i = 0;
    bool flag = false;

    while (true) {
        i++;

        // Вычисляем Ax - b
        vector<float> Ax_minus_b(N, 0);
        #pragma omp parallel for
        for (int i = 0; i < N; ++i) {
            float sum = 0;
            for (int j = 0; j < N; ++j) {
                sum += a[i][j] * x[j];
            }
            Ax_minus_b[i] = sum - b[i];
        }

        // Вычисляем tau * (Ax - b)
        vector<float> tax_minus_b(N);
        #pragma omp parallel for
        for (int i = 0; i < N; ++i) {
            tax_minus_b[i] = tau * Ax_minus_b[i];
        }

        // Вычисляем норму (Ax - b) и норму b
        float norm_Ax_minus_b = 0;
        float norm_b = 0;
        #pragma omp parallel for reduction(+:norm_Ax_minus_b, norm_b)
        for (int i = 0; i < N; ++i) {
            norm_Ax_minus_b += Ax_minus_b[i] * Ax_minus_b[i];
            norm_b += b[i] * b[i];
        }
        norm_Ax_minus_b = sqrt(norm_Ax_minus_b);
        norm_b = sqrt(norm_b);

        float crit = norm_Ax_minus_b / norm_b;

        #pragma omp single
        {
            if (crit < epsilon || crit == old_crit) {
                flag = true;
            } else {
                old_crit = crit;
                for (int i = 0; i < N; ++i) {
                    x[i] -= tax_minus_b[i];
                }
            }
        }

        if (flag) {
            break;
        }
    }

    double end_time = omp_get_wtime(); // Замер времени окончания выполнения
    double execution_time = end_time - start_time;

    cout << "Запуск для N = " << N << endl;
    cout << "Количество потоков: " << num_threads << endl;
    cout << "Время работы: " << execution_time << " секунд" << endl;

    return 0;
}