package main

import (
	"fmt"
	"math"
)

func rungeKutta4ConstH(x0, xEnd float64, y0 []float64, eps float64, startH float64) ([][]float64, [][]float64, [][]float64, [][]float64, [][]float64) {
	var xPointsArr, yPointsArr, exactPointsArr, rungeArr, hArrArr [][]float64
	var xPoints, yPoints, exactPoints, runge, hArr []float64
	h := startH

	for {
		if len(xPoints) != 0 {
			xPointsArr = append(xPointsArr, xPoints)
			yPointsArr = append(yPointsArr, yPoints)
			exactPointsArr = append(exactPointsArr, exactPoints)
			hArrArr = append(hArrArr, hArr)
			rungeArr = append(rungeArr, runge)
		}

		xPoints = []float64{}
		yPoints = []float64{}
		exactPoints = []float64{}
		hArr = []float64{}
		runge = []float64{}

		localMax := 0.0

		x := x0
		y := make([]float64, len(y0))
		copy(y, y0)

		hArr = append(hArr, h)
		xPoints = append(xPoints, x)
		yPoints = append(yPoints, y[0])
		exactPoints = append(exactPoints, exactSolution(x))
		runge = append(runge, 0.0)

		for x < xEnd {

			y1 := stepRK4(x, y, h)
			y2 := stepRK4(x+h, y1, h)
			y2h := stepRK4(x+h, y, 2*h)

			err := math.Abs(y2[0]-y2h[0]) / 15.0
			localMax = math.Max(err, localMax)

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
		}

		if localMax < eps {
			xPointsArr = append(xPointsArr, xPoints)
			yPointsArr = append(yPointsArr, yPoints)
			exactPointsArr = append(exactPointsArr, exactPoints)
			hArrArr = append(hArrArr, hArr)
			rungeArr = append(rungeArr, runge)
			break
		} else {
			h = h / 2
		}
	}

	return xPointsArr, yPointsArr, exactPointsArr, rungeArr, hArrArr
}

func constH(startH float64) {
	// Начальные условия (вариант 4)
	x0 := 0.0
	xEnd := 1.0
	y0 := []float64{1.0, 1.0} // y(0) = 1, y'(0) = 1
	eps := 0.001

	xPoints, yPoints, exactPoints, runge, hArr := rungeKutta4ConstH(x0, xEnd, y0, eps, startH)
	l := len(xPoints)

	fmt.Println("Константный шаг")
	for j := 0; j < l; j++ {
		fmt.Println("--------------------------------------------------------------------------------")
		fmt.Printf("Проход %d c шагом %0.7f\n", j, hArr[j][0])
		if j == l-1 {
			fmt.Println("Нужный шаг найден!!!!!")
		}

		resX, resY, exP, rungRes, hArrRes := xPoints[j], yPoints[j], exactPoints[j], runge[j], hArr[j]

		fmt.Printf("%-10s %-20s %-20s %-12s %-12s %-6s\n", "x", "Приближенное y(x)", "Точное y(x)", "Ошибка", "Погрешность", "Шаг")

		for i := range resX {
			err := math.Abs(resY[i] - exP[i])
			fmt.Printf("%-10.5f %-20.7f %-20.7f %-12.7f %-12.7f %-6.5f\n",
				resX[i],
				resY[i],
				exP[i],
				err,
				rungRes[i],
				hArrRes[i])
		}
	}
}
