package main

import (
	"fmt"
)

func forward(a, b, c, d []float64) ([]float64, []float64) {
	n := len(d)
	alpha := make([]float64, n-1)
	beta := make([]float64, n)

	alpha[0] = c[0] / b[0]
	beta[0] = d[0] / b[0]

	for i := 1; i < n-1; i++ {
		alpha[i] = c[i] / (b[i] - a[i-1]*alpha[i-1])
	}

	for i := 1; i < n; i++ {
		beta[i] = (d[i] - a[i-1]*beta[i-1]) / (b[i] - a[i-1]*alpha[i-1])
	}

	return alpha, beta
}

func backward(alpha, beta []float64) []float64 {
	n := len(beta)
	x := make([]float64, n)
	x[n-1] = beta[n-1]

	for i := n - 2; i >= 0; i-- {
		x[i] = beta[i] - alpha[i]*x[i+1]
	}

	return x
}

func progonka(a, b, c, d []float64) []float64 {
	alpha, beta := forward(a, b, c, d)
	x := backward(alpha, beta)
	return x
}
