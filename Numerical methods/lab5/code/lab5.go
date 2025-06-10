package main

import (
	"fmt"
	"math"
)

type Vector []float64

func Function3(x Vector) float64 {
	return math.Exp(x[0]) + math.Pow(x[0]+x[1], 2)
}

func Function(x Vector) float64 {
	return 7*math.Pow(x[0], 2) + 2*x[0]*x[1] + 5*math.Pow(x[1], 2) + x[0] - 10*x[1]
}

func Gradient(x Vector) Vector {
	dfdx0 := 14*x[0] + 2*x[1] + 1
	dfdx1 := 2*x[0] + 10*x[1] - 10
	return Vector{dfdx0, dfdx1}
}

func Gradient3(x Vector) Vector {
	dfdx0 := math.Exp(x[0]) + 2*(x[0]+x[1])
	dfdx1 := 2 * (x[0] + x[1])
	return Vector{dfdx0, dfdx1}
}

func GradNormInf(g Vector) float64 {
	return math.Max(math.Abs(g[0]), math.Abs(g[1]))
}

func parabolaMethod(f func(float64) float64, a, b, eps float64) float64 {
	const maxIter = 100
	x1, x2, x3 := a, (a+b)/2, b
	f1, f2, f3 := f(x1), f(x2), f(x3)

	for i := 0; i < maxIter; i++ {
		// строим параболу через 3 точки
		A := (f3 - (x3*(f2-f1)+x2*f1-x1*f2)/(x2-x1)) / (x3*x3 - x3*(x1+x2) + x1*x2)
		B := (f2 - f1 - A*(x2*x2-x1*x1)) / (x2 - x1)
		// находим ее вершину
		xv := -B / (2 * A)

		// если вершина параболы близка к предыдущей точке x2, то выходим
		if math.Abs(x2-xv) < eps {
			return xv
		}

		if xv < x2 {
			// если xv слева от x2, то мы сдвигаем правый край
			x3, f3 = x2, f2
			x2, f2 = xv, f(xv)
		} else {
			// если xv справа от x2, то сдвигаем левый край
			x1, f1 = x2, f2
			x2, f2 = xv, f(xv)
		}
	}
	return (a + b) / 2
}

func SteepestDescent(x0 Vector, eps float64, maxIter int) (Vector, []float64, int) {
	x := make(Vector, len(x0))
	copy(x, x0)
	history := []float64{Function(x)}

	for k := 0; k < maxIter; k++ {
		grad := Gradient(x)

		// условие остановки
		if GradNormInf(grad) < eps {
			return x, history, k + 1
		}

		d := make(Vector, len(grad))
		for i := range d {
			// берём направление наискорейшего убывания
			d[i] = -grad[i]
		}

		// строим функцию phi(t) = f(x + t * d)
		phi := func(t float64) float64 {
			newX := make(Vector, len(x))
			for i := range newX {
				newX[i] = x[i] + t*d[i]
			}
			return Function(newX)
		}
		t := parabolaMethod(phi, 0, 1, 1e-5)

		for i := range x {
			x[i] += t * d[i]
		}
		history = append(history, Function(x))
	}
	return x, history, maxIter
}

func main() {
	x0 := Vector{1.0, 1.0}
	eps := 1e-3
	maxIter := 1000

	solution, _, iterations := SteepestDescent(x0, eps, maxIter)

	// Вывод результатов
	fmt.Printf("Начальная точка: x = [%.6f, %.6f] \n", x0[0], x0[1])
	fmt.Printf("Численное решение: x = [%.6f, %.6f]\n", solution[0], solution[1])
	fmt.Printf("Значение функции (численное): f(x) = %.6f\n", Function(solution))
	fmt.Printf("Количество итераций: %d\n", iterations)
}
