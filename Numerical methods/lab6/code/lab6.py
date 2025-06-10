import numpy as np
import matplotlib.pyplot as plt

x = np.array([1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5])
y = np.array([0.16, 0.68, 1.96, 2.79, 3.80, 6.81, 9.50, 15.60, 24.86])

x_a = (x[0] + x[-1]) / 2
x_g = np.sqrt(x[0] * x[-1])
x_h = 2 * x[0] * x[-1] / (x[0] + x[-1])
y_a = (y[0] + y[-1]) / 2
y_g = np.sqrt(y[0] * y[-1])
y_h = 2 * y[0] * y[-1] / (y[0] + y[-1])

print(f"x_a = {x_a:.3f}, x_g = {x_g:.3f}, x_h = {x_h:.3f}")
print(f"y_a = {y_a:.3f}, y_g = {y_g:.3f}, y_h = {y_h:.3f}")

ln_x = np.log(x)
ln_y = np.log(y)
n = len(x)
A = np.sum(ln_x**2)
B = np.sum(ln_x)
D1 = np.sum(ln_x * ln_y)
D2 = np.sum(ln_y)

coeff_matrix = np.array([[n, B], [B, A]])
right_hand_side = np.array([D2, D1])
ln_a, b = np.linalg.solve(coeff_matrix, right_hand_side)
a = np.exp(ln_a)

print(f"a = {a:.3f}, b = {b:.3f}")

def z_2(x, a, b):
    return a * x**b

z_x_a = z_2(x_a, a, b)
z_x_g = z_2(x_g, a, b)
z_x_h = z_2(x_h, a, b)
print(f"z(x_a) = {z_x_a:.3f}, z(x_g) = {z_x_g:.3f}, z(x_h) = {z_x_h:.3f}")

delta = [
    abs(z_x_a - y_a),
    abs(z_x_g - y_g),
    abs(z_x_a - y_g),
    abs(z_x_g - y_a),
    abs(z_x_h - y_a),
    abs(z_x_a - y_h),
    abs(z_x_h - y_h),
    abs(z_x_h - y_g),
    abs(z_x_g - y_h)
]
for i, d in enumerate(delta, 1):
    print(f"δ_{i} = {d:.3f}")
xi_h = min(delta)
xi_h_idx = delta.index(xi_h) + 1
print(f"Наименьшее значение δ_i = {xi_h:.3f} для функции z_{xi_h_idx}")

y_pred = z_2(x, a, b)
Delta = np.sqrt(np.sum((y - y_pred)**2) / n)
print(f"Среднеквадратичное отклонение Δ = {Delta:.6f}")

plt.scatter(x, y, color='blue', label='Табличные данные')
x_smooth = np.linspace(min(x), max(x), 200)
y_smooth = z_2(x_smooth, a, b)
plt.plot(x_smooth, y_smooth, color='red', label=f'z_2(x) = {a:.3f}x^{b:.3f}')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.show()