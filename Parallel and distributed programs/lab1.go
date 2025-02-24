package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

// Функция для создания случайной квадратной матрицы размерности n
func generateMatrix(n int) [][]int {
	matrix := make([][]int, n)
	for i := range matrix {
		matrix[i] = make([]int, n)
		for j := range matrix[i] {
			matrix[i][j] = rand.Intn(10)
		}
	}
	return matrix
}

// Перемножение матриц построчно
func multiplyRowWise(A, B [][]int) [][]int {
	n := len(A)
	result := make([][]int, n)
	for i := range result {
		result[i] = make([]int, n)
	}
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			for k := 0; k < n; k++ {
				result[i][j] += A[i][k] * B[k][j]
			}
		}
	}
	return result
}

// Перемножение матриц по столбцам
func multiplyColumnWise(A, B [][]int) [][]int {
	n := len(A)
	result := make([][]int, n)
	for i := range result {
		result[i] = make([]int, n)
	}
	for j := 0; j < n; j++ {
		for i := 0; i < n; i++ {
			for k := 0; k < n; k++ {
				result[i][j] += A[i][k] * B[k][j]
			}
		}
	}
	return result
}

// Параллельное перемножение матриц
func multiplyParallel(A, B, C [][]int, startRow, endRow int, wg *sync.WaitGroup) {
	defer wg.Done()

	n := len(A)
	for i := startRow; i < endRow; i++ {
		for j := 0; j < n; j++ {
			for k := 0; k < n; k++ {
				C[i][j] += A[i][k] * B[k][j]
			}
		}
	}
}

// Функция для распараллеливания перемножения матриц
func parallelMatrixMultiplication(A, B [][]int, numThreads int) [][]int {
	n := len(A)
	result := make([][]int, n)
	for i := range result {
		result[i] = make([]int, n)
	}

	var wg sync.WaitGroup
	rowsPerThread := n / numThreads

	for i := 0; i < numThreads; i++ {
		startRow := i * rowsPerThread
		endRow := startRow + rowsPerThread

		// Последний поток берет все оставшиеся строки, если n не делится нацело
		if i == numThreads-1 {
			endRow = n
		}

		wg.Add(1)
		go multiplyParallel(A, B, result, startRow, endRow, &wg)
	}

	wg.Wait() // Ждем завершения всех горутин
	return result
}

// Функция для вывода матрицы в удобочитаемом виде
func printMatrix(matrix [][]int) {
	for _, row := range matrix {
		for _, val := range row {
			fmt.Printf("%4d ", val)
		}
		fmt.Println()
	}
}

// Функция для удобного форматирования времени выполнения
func printDuration(duration time.Duration) {
	minutes := int(duration.Minutes())
	seconds := int(duration.Seconds()) % 60
	milliseconds := int(duration.Milliseconds()) % 1000
	fmt.Printf("%d минут(ы) %d секунд(ы) %d миллисекунд(ы)\n", minutes, seconds, milliseconds)
}

// Проверка, одинаковы ли две матрицы
func compareMatrices(A, B [][]int) bool {
	for i := range A {
		for j := range A[i] {
			if A[i][j] != B[i][j] {
				return false
			}
		}
	}
	return true
}

func main1() {
	// Определяем размерность матрицы
	n := 2500
	numThreads := 8 // Количество потоков

	// Генерируем две случайные матрицы
	A := generateMatrix(n)
	B := generateMatrix(n)

	fmt.Printf("При n = %d\n", n)

	// Замер времени для построчного перемножения
	start := time.Now()
	resultRow := multiplyRowWise(A, B)
	durationRowWise := time.Since(start)
	fmt.Printf("Время выполнения построчного перемножения:")
	printDuration(durationRowWise)

	// Замер времени для перемножения по столбцам
	start = time.Now()
	resultColumn := multiplyColumnWise(A, B)
	//printMatrix(resultColumn)
	durationColumnWise := time.Since(start)
	fmt.Printf("Время выполнения перемножения по столбцам: ")
	printDuration(durationColumnWise)

	// Проверяем, одинаковы ли матрицы
	if compareMatrices(resultRow, resultColumn) {
		fmt.Println("Результаты совпадают.")
	} else {
		fmt.Println("Результаты различаются.")
	}

	// Параллельное перемножение матриц
	start = time.Now()
	resultParallel := parallelMatrixMultiplication(A, B, numThreads)
	durationParallel := time.Since(start)
	fmt.Printf("Время выполнения параллельного перемножения (%d потоков): ", numThreads)
	printDuration(durationParallel)

	// Сравнение результатов
	if compareMatrices(resultRow, resultParallel) {
		fmt.Println("Результаты совпадают.")
	} else {
		fmt.Println("Результаты различаются.")
	}

}
