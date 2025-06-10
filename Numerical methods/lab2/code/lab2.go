package main

import (
	"fmt"
	"math"
)

func f(x float64) float64 {
	// Вариант 4
	return 2 * x * math.Cos(x/2)
}

// Генерация точек на отрезке [a, b] с шагом h
func generatePoints(a, b float64, n int) []float64 {
	h := (b - a) / float64(n)
	points := make([]float64, n+1)
	for i := 0; i <= n; i++ {
		points[i] = a + float64(i)*h
	}
	return points
}

// Метод прямоугольников
func rectangleMethod(a, b float64, n int) float64 {
	points := generatePoints(a, b, n)
	h := (b - a) / float64(n)
	sum := 0.0

	for i := 0; i < n; i++ {
		x := points[i] + h/2
		sum += f(x)
	}

	return sum * h
}

// Метод трапеций
func trapezoidMethod(a, b float64, n int) float64 {
	points := generatePoints(a, b, n)
	h := (b - a) / float64(n)
	sum := (f(a) + f(b)) / 2

	for i := 1; i < n; i++ {
		sum += f(points[i])
	}

	return sum * h
}

// Метод Симпсона
func simpsonMethod(a, b float64, n int) float64 {
	if n%2 != 0 {
		n++
	}

	points := generatePoints(a, b, n)
	h := (b - a) / float64(n)
	sum := f(a) + f(b)

	for i := 1; i < n; i++ {
		x := points[i]
		if i%2 == 0 {
			sum += 2 * f(x)
		} else {
			sum += 4 * f(x)
		}
	}

	return sum * h / 3
}

func applyMethod(method func(a, b float64, n int) float64, a, b float64, n int, epsilon float64, p int) (int, float64, float64) {
	methodResult := method(a, b, n)
	methodResult2 := method(a, b, 2*n)
	methodError := math.Abs(methodResult-methodResult2) / (math.Pow(2, float64(p)) - 1)

	for methodError > epsilon {
		n *= 2
		methodResult = method(a, b, n)
		methodResult2 = method(a, b, 2*n)
		methodError = math.Abs(methodResult-methodResult2) / (math.Pow(2, float64(p)) - 1)
	}
	return n, methodResult, methodError
}

func main() {
	a := 0.0
	b := math.Pi
	epsilon := 0.001
	pSimpson := 4
	pRect := 2
	pTrap := 2

	// Вычисление методом прямоугольников
	nRect := 10
	var rectResult, rectError float64
	nRect, rectResult, rectError = applyMethod(rectangleMethod, a, b, nRect, epsilon, pRect)

	// Вычисление методом трапеций
	nTrap := 10
	var trapResult, trapError float64
	nTrap, trapResult, trapError = applyMethod(trapezoidMethod, a, b, nTrap, epsilon, pTrap)

	// Метод Симпсона
	nSimpson := 10
	var simpsonResult, simpsonError float64
	nSimpson, simpsonResult, simpsonError = applyMethod(simpsonMethod, a, b, nSimpson, epsilon, pSimpson)

	// Вывод результатов
	fmt.Printf("\n%-20s%-16s%-16s%-16s\n", "Параметр", "rect", "trapec", "Simpson")
	fmt.Println("---------------------------------------------------------------")
	fmt.Printf("%-20s%-16d%-16d%-16d\n", "n", nRect, nTrap, nSimpson)
	fmt.Printf("%-20s%-16.5f%-16.5f%-16.5f\n", "I(n)", rectResult, trapResult, simpsonResult)
	fmt.Printf("%-20s%-16.6f%-16.6f%-16.6f\n", "R", rectError, trapError, simpsonError)
	fmt.Printf("%-20s%-16.5f%-16.5f%-16.5f\n", "I(n)+R", rectResult+rectError, trapResult+trapError, simpsonResult+simpsonError)
}
