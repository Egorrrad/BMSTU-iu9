import time
from mpi4py import MPI
import numpy as np
from numpy.linalg import norm

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

N = 2 ** 13  # размерность матрицы и вектора
split = size  # число частей, на которые будет делиться матрица

# a — матрица размера N x N, где все элементы равны 1, а диагональные элементы — 2
a = np.zeros((N, N), dtype=np.single)
for i in range(N):
    for j in range(N):
        if i == j:
            a[i, j] = 2
        else:
            a[i, j] = 1

# b — вектор размерности N, каждый элемент которого равен N + 1
b = np.zeros(N, dtype=np.single)
for i in range(N):
    b[i] = N + 1

x = np.zeros(N, dtype=np.single)

epsilon = 0.000001


def mult_matrix_by_vector(m, v):
    v = v[:, None]
    # создаем буфер для вычислений
    part_a = np.empty(shape=(N // split, N), dtype=np.single)
    # принимаем свой кусок
    comm.Scatter(m, part_a, root=0)
    part_a = part_a @ v
    # выделяем место под результат
    res = None
    if rank == 0:
        res = np.empty(shape=(N, 1), dtype=np.single)
    # отдаем свой кусок
    comm.Gather(part_a, res, root=0)

    # возвращаем то, что в нулевом процессе
    return comm.bcast(res, root=0).T[0]


def main():
    global x

    old_crit = 0
    i = 0
    tau = 0.1 / N
    # метод простой итерации
    while True:
        i += 1
        Ax_minus_b = mult_matrix_by_vector(a, x) - b
        tax_minus_b = tau * Ax_minus_b

        flag = False
        if rank == 0:
            crit = norm(Ax_minus_b) / norm(b)
            if crit < epsilon or crit == old_crit:
                flag = True
            else:
                old_crit = crit
                x = x - tax_minus_b

        if comm.bcast(flag, root=0):
            break

        x = comm.bcast(x, root=0)
    # if rank == 0:
    # print('Вектор x: ', x)


if __name__ == '__main__':
    # измеряем время выполнения
    t = time.time()
    if rank == 0:
        print('Запуск для N =', split)
    main()
    if rank == 0:
        print('Время работы:', time.time() - t)
