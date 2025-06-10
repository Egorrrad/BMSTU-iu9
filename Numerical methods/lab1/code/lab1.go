package main

import (
	"fmt"
	"math"
)

type CubSpline struct {
	x, y       []float64
	a, b, c, d []float64
}

func NewCubSpline(x, y []float64, h float64) *CubSpline {
	n := len(x)
	if n != len(y) || n < 2 {
		panic("Invalid input data")
	}

	a := make([]float64, n-1)
	b := make([]float64, n)
	c := make([]float64, n-1)
	d := make([]float64, n)

	b[0] = 1.0
	b[n-1] = 1.0

	for i := 1; i < n-1; i++ {
		a[i-1] = h
		b[i] = 4 * h
		c[i] = h
		d[i] = 3 * ((y[i+1]-y[i])/h - (y[i]-y[i-1])/h)
	}

	cCoeffs := progonka(a, b, c, d)

	spline := &CubSpline{
		x: x,
		y: y,
		a: make([]float64, n-1),
		b: make([]float64, n-1),
		c: cCoeffs,
		d: make([]float64, n-1),
	}

	for i := 0; i < n-1; i++ {
		spline.a[i] = y[i]
		if i < n-2 {
			spline.b[i] = (y[i+1]-y[i])/h - h*(2*cCoeffs[i]+cCoeffs[i+1])/3
			spline.d[i] = (cCoeffs[i+1] - cCoeffs[i]) / (3 * h)
		} else {
			spline.b[i] = (y[i+1]-y[i])/h - 2*h*cCoeffs[i]/3
			spline.d[i] = -cCoeffs[i] / (3 * h)
		}
	}

	return spline
}

func (s *CubSpline) Interpolate(xVal float64) float64 {
	// Поиск интервала
	i := 0
	for i < len(s.x)-1 && xVal > s.x[i+1] {
		i++
	}

	dx := xVal - s.x[i]
	return s.a[i] + s.b[i]*dx + s.c[i]*dx*dx + s.d[i]*dx*dx*dx
}

func generateNodesWithStep(f func(float64) float64, a, b float64, n int) ([]float64, []float64, []float64, float64) {
	h := (b - a) / float64(n)
	x := make([]float64, n)
	y := make([]float64, n)
	xPrime := make([]float64, n)

	for i := 0; i < n; i++ {
		x[i] = a + float64(i)*h
		y[i] = f(x[i])
		xPrime[i] = a + (float64(i)-0.5)*h
	}

	return x, y, xPrime, h
}

func main() {
	// Задаем функцию
	// Вариант 4
	f := func(x float64) float64 {
		return 2 * x * math.Cos(x/2)
	}

	// Задаем отрезок
	a, b := 0.0, math.Pi

	// Задаем шаг интерполяции
	n := 32

	// Генерация узлов с заданным шагом
	x, y, xPrime, h := generateNodesWithStep(f, a, b, n)

	// Построение сплайна
	spline := NewCubSpline(x, y, h)

	fmt.Println("Коэффициенты сплайна:")
	fmt.Printf("%-15s %16s %16s %16s %16s\n", "Интервал", "a", "b", "c", "d")
	fmt.Println("--------------------------------------------------------------------------------")

	for i := 0; i < len(x)-1; i++ {
		fmt.Printf("[%.10f, %.10f] %16.10f %16.10f %16.10f %16.10f\n",
			x[i], x[i+1], spline.a[i], spline.b[i], spline.c[i], spline.d[i])
	}

	fmt.Println("Таблица значений:")
	fmt.Printf("%2s %16s %16s %16s %16s\n", "i", "x_i`", "f(x_i`)", "S(x_i`)", "Погрешность")
	fmt.Println("--------------------------------------------------------------------------------")

	for i := 0; i < len(x); i++ {
		fxPrime := f(xPrime[i])
		sxPrime := spline.Interpolate(xPrime[i])
		errAbs := math.Abs(fxPrime - sxPrime)
		fmt.Printf("%2d %16.10f %16.10f %16.10f %16.10f\n",
			i, xPrime[i], fxPrime, sxPrime, errAbs)
	}

	var point float64
	fmt.Print("Введите произвольную точку для вычисления значений функции и сплайна: ")
	fmt.Scan(&point)

	originalValue := f(point)
	splineValue := spline.Interpolate(point)
	errAbs := math.Abs(originalValue - splineValue)

	fmt.Printf("Значение оригинальной функции в точке %.10f: %.10f\n", point, originalValue)
	fmt.Printf("Значение сплайна в точке %.10f: %.10f\n", point, splineValue)
	fmt.Printf("Погрешность в точке %.10f: %.10f\n", point, errAbs)
}
