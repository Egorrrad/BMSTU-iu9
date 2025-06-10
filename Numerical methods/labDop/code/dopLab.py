import numpy as np
import matplotlib.pyplot as plt

N = 128
n = 7
j = np.arange(N)
x_j = j / N

def f(x):
    return np.exp(np.sin(3 * np.pi * x + 0.2))

f_j = f(x_j)

def bit_reverse(j, n_bits):
    reversed_j = 0
    for bit in range(n_bits):
        if (j >> bit) & 1:
            reversed_j |= 1 << (n_bits - 1 - bit)
    return reversed_j

def manual_fft(values, N, inverse=False):
    n_bits = int(np.log2(N))
    a = np.zeros(N, dtype=complex)
    for i in range(N):
        rev_i = bit_reverse(i, n_bits)
        a[rev_i] = values[i]

    for s in range(1, n_bits + 1):
        m = 1 << s
        m2 = m >> 1
        for k in range(m2):
            if inverse:
                w = np.exp(2j * np.pi * k / m)
            else:
                w = np.exp(-2j * np.pi * k / m)
            for j in range(k, N, m):
                t = w * a[j + m2]
                u = a[j]
                a[j] = u + t
                a[j + m2] = u - t

    if not inverse:
        a /= N
    return a

A_q = manual_fft(f_j, N)

y_j = 0.5 + j / N
f_interp = np.zeros(N, dtype=complex)
for q in range(N):
    phase = np.exp(2j * np.pi * q * y_j)
    f_interp += A_q[q] * phase

f_actual = f(y_j)

error = np.abs(f_interp - f_actual)

print("Сравнение значений тригонометрической интерполяции и истинных значений в точках y_j:")
for i in range(10):
    print(f"y_{i} = {y_j[i]:.4f}: Интерполяция = {f_interp[i].real:.16f}, Истинное = {f_actual[i]:.16f}, Ошибка = {error[i]:.6e}")

# Визуализация
plt.figure(figsize=(10, 7))
# plt.subplot(2, 1, 1)
plt.plot(y_j, f_actual, 'b-', label='Истинная функция f(y_j) = exp(sin(2π y_j))')
plt.plot(y_j, f_interp.real, 'r--', label='Тригонометрическая интерполяция')
plt.xlabel('y_j')
plt.ylabel('f(y_j)')
plt.title('Сравнение тригонометрической интерполяции и истинной функции')
plt.legend()
plt.grid(True)


plt.tight_layout()
plt.show()