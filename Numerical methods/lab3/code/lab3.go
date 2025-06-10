package main

import (
	"fmt"
	"math"
)

// вариант 4: y” - y' = 2(1 - x)
func fd(x float64, y []float64) []float64 {
	// y[0] = y, y[1] = y'
	// u' = v
	// v' = v + 2(1 - x)
	return []float64{
		y[1],               // u' = v
		y[1] + 2.0*(1.0-x), // v' = v + 2(1 - x)
	}
}

// точное решение (вариант 4: y = e^x + x^2)
func exactSolution(x float64) float64 {
	return math.Exp(x) + x*x
}

func rungeKutta4(x0, xEnd float64, y0 []float64, eps float64, startH float64) ([]float64, []float64, []float64, []float64, []float64) {
	var xPoints, yPoints, exactPoints, runge, hArr []float64
	x := x0
	y := make([]float64, len(y0))
	copy(y, y0)
	h := startH
	k := 0.9

	hArr = append(hArr, h)
	xPoints = append(xPoints, x)
	yPoints = append(yPoints, y[0])
	exactPoints = append(exactPoints, exactSolution(x))
	runge = append(runge, 0.0)

	for x < xEnd {
		if x+h > xEnd {
			h = xEnd - x
		}

		y1 := stepRK4(x, y, h)
		y2 := stepRK4(x+h, y1, h)
		y2h := stepRK4(x+h, y, 2*h)

		err := math.Abs(y2[0]-y2h[0]) / 15.0
		hOpt := h * math.Pow(eps/err, 1.0/5.0)

		if err < eps {
			xPoints = append(xPoints, x+h)
			yPoints = append(yPoints, y1[0])
			exactPoints = append(exactPoints, exactSolution(x+h))
			runge = append(runge, err)
			hArr = append(hArr, h)

			if x+2*h <= xEnd {
				xPoints = append(xPoints, x+2*h)
				yPoints = append(yPoints, y2[0])
				exactPoints = append(exactPoints, exactSolution(x+2*h))
				runge = append(runge, err)
				hArr = append(hArr, h)
			}

			x += 2 * h
			y = y2
			h = math.Min(k*hOpt, xEnd-x)
		} else {
			h = k * hOpt
		}
	}

	return xPoints, yPoints, exactPoints, runge, hArr
}

func stepRK4(x float64, y []float64, h float64) []float64 {
	k1 := fd(x, y)

	k2y := make([]float64, len(y))
	for i := range y {
		k2y[i] = y[i] + h/2*k1[i]
	}
	k2 := fd(x+h/2, k2y)

	k3y := make([]float64, len(y))
	for i := range y {
		k3y[i] = y[i] + h/2*k2[i]
	}
	k3 := fd(x+h/2, k3y)

	k4y := make([]float64, len(y))
	for i := range y {
		k4y[i] = y[i] + h*k3[i]
	}
	k4 := fd(x+h, k4y)

	yNew := make([]float64, len(y))
	for i := range y {
		yNew[i] = y[i] + h/6*(k1[i]+2*k2[i]+2*k3[i]+k4[i])
	}

	return yNew
}

func main() {
	// Начальные условия (вариант 4)
	x0 := 0.0
	xEnd := 1.0
	y0 := []float64{1.0, 1.0} // y(0) = 1, y'(0) = 1
	eps := 0.001
	h := 1.0

	xPoints, yPoints, exactPoints, runge, hArr := rungeKutta4(x0, xEnd, y0, eps, h)

	fmt.Println("Автоматический подбор шага")
	fmt.Printf("%-10s %-20s %-20s %-12s %-12s %-6s\n", "x", "Приближенное y(x)", "Точное y(x)", "Ошибка", "Погрешность", "Шаг")
	for i := range xPoints {
		err := math.Abs(yPoints[i] - exactPoints[i])
		fmt.Printf("%-10.5f %-20.7f %-20.7f %-12.7f %-12.7f %-6.5f\n",
			xPoints[i],
			yPoints[i],
			exactPoints[i],
			err,
			runge[i],
			hArr[i])
	}

	constH(h)
}
