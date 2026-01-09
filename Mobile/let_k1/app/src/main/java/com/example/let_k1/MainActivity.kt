package com.example.let_k1

import android.os.Bundle
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import org.ejml.simple.SimpleMatrix
import org.json.JSONArray
import java.util.Locale
import kotlin.math.abs
import kotlin.math.min
import kotlin.math.sqrt

class MainActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val matrixInput = findViewById<EditText>(R.id.matrixInput)
        val startButton = findViewById<Button>(R.id.startButton)
        val resultTextView = findViewById<TextView>(R.id.resultTextView)

        matrixInput.setText("[[1,2,3],[4,5,6]]")

        startButton.setOnClickListener {
            val json = matrixInput.text.toString()
            try {
                val matrix = parseJsonMatrix(json)

                val customResult = computeCustomSVD(matrix)
                val libraryResult = computeLibrarySVD(matrix)

                resultTextView.text = String.format(
                    Locale.getDefault(),
                    "=== РУЧНОЙ РАСЧЕТ ===\n%s\n\n=== БИБЛИОТЕЧНЫЙ РАСЧЕТ ===\n%s",
                    customResult,
                    libraryResult
                )

            } catch (e: Exception) {
                Toast.makeText(this, "Ошибка: ${e.message}", Toast.LENGTH_LONG).show()
                resultTextView.text = "Ошибка: ${e.message}"
            }
        }
    }

    private fun computeCustomSVD(matrix: Array<DoubleArray>): String {
        return computeSVD(matrix)
    }

    private fun parseJsonMatrix(json: String): Array<DoubleArray> {
        val jsonArray = JSONArray(json)
        val rows = jsonArray.length()
        val cols = jsonArray.getJSONArray(0).length()
        val matrix = Array(rows) { DoubleArray(cols) }

        for (i in 0 until rows) {
            val row = jsonArray.getJSONArray(i)
            for (j in 0 until cols) {
                matrix[i][j] = row.getDouble(j)
            }
        }
        return matrix
    }

    private fun matrixToString(matrix: SimpleMatrix): String {
        val sb = StringBuilder()
        for (i in 0 until matrix.numRows()) {
            for (j in 0 until matrix.numCols()) {
                sb.append(String.format(Locale.getDefault(), "%10.6f", matrix[i, j]))
            }
            sb.append("\n")
        }
        return sb.toString()
    }

    private fun transpose(matrix: Array<DoubleArray>): Array<DoubleArray> {
        val rows = matrix.size
        val cols = matrix[0].size
        return Array(cols) { col -> DoubleArray(rows) { row -> matrix[row][col] } }
    }

    private fun multiplyMatrices(a: Array<DoubleArray>, b: Array<DoubleArray>): Array<DoubleArray> {
        val rowsA = a.size
        val colsA = a[0].size
        val rowsB = b.size
        val colsB = b[0].size

        if (colsA != rowsB) throw IllegalArgumentException("Несовместимые размеры матриц")

        val result = Array(rowsA) { DoubleArray(colsB) }
        for (i in 0 until rowsA) {
            for (j in 0 until colsB) {
                var sum = 0.0
                for (k in 0 until colsA) {
                    sum += a[i][k] * b[k][j]
                }
                result[i][j] = sum
            }
        }
        return result
    }

    private fun computeTrace(matrix: Array<DoubleArray>): Double {
        var sum = 0.0
        for (i in matrix.indices) {
            sum += matrix[i][i]
        }
        return sum
    }

    private fun subtractScalarMultiplyIdentity(matrix: Array<DoubleArray>, scalar: Double): Array<DoubleArray> {
        val n = matrix.size
        return Array(n) { i ->
            DoubleArray(n) { j ->
                val value = if (i == j) matrix[i][j] - scalar else matrix[i][j]
                if (abs(value) < 1e-10) 0.0 else value
            }
        }
    }

    private fun danilevskyMethod(matrix: Array<DoubleArray>): DoubleArray {
        val n = matrix.size
        var currentMatrix = matrix.map { it.copyOf() }.toTypedArray()
        val coefficients = DoubleArray(n + 1)
        coefficients[0] = 1.0

        for (k in 1..n) {
            val trace = computeTrace(currentMatrix)
            val pk = trace / k
            coefficients[k] = -pk

            if (k < n) {
                val tempMatrix = subtractScalarMultiplyIdentity(currentMatrix, pk)
                currentMatrix = multiplyMatrices(matrix, tempMatrix)
            }
        }
        return coefficients
    }

    private fun evaluatePolynomial(coefficients: DoubleArray, x: Double): Double {
        var result = 0.0
        for (c in coefficients) {
            result = result * x + c
        }
        return result
    }

    private fun polynomialDerivative(coefficients: DoubleArray): DoubleArray {
        val n = coefficients.size - 1
        if (n <= 0) return doubleArrayOf()
        val derivative = DoubleArray(n)
        for (i in 0 until n) {
            derivative[i] = coefficients[i] * (n - i)
        }
        return derivative
    }

    private fun newtonMethod(
        coefficients: DoubleArray,
        initialGuess: Double,
        maxIterations: Int = 100,
        tolerance: Double = 1e-8
    ): Double {
        var x = initialGuess
        val derivativeCoeffs = polynomialDerivative(coefficients)

        for (i in 0 until maxIterations) {
            val fx = evaluatePolynomial(coefficients, x)
            val dfx = evaluatePolynomial(derivativeCoeffs, x)

            if (abs(dfx) < 1e-12) {
                throw RuntimeException("Производная близка к нулю при x = $x")
            }

            val nextX = x - fx / dfx
            if (abs(nextX - x) < tolerance) {
                return nextX
            }
            x = nextX
        }
        throw RuntimeException("Не сошелся за $maxIterations итераций")
    }

    private fun findAllEigenvalues(coefficients: DoubleArray): DoubleArray {
        val n = coefficients.size - 1
        var currentCoeffs = coefficients.copyOf()
        val eigenvalues = DoubleArray(n)
        var initialGuess = 1.0
        var i = 0

        while (i < n) {
            try {
                val root = newtonMethod(currentCoeffs, initialGuess)
                eigenvalues[i] = root
                currentCoeffs = deflatePolynomial(currentCoeffs, root)
                initialGuess += 2.0
                i++
            } catch (e: Exception) {
                initialGuess = -initialGuess
            }
        }
        return eigenvalues
    }

    private fun deflatePolynomial(coefficients: DoubleArray, root: Double): DoubleArray {
        val n = coefficients.size - 1
        val newCoeffs = DoubleArray(n)
        var remainder = 0.0

        for (i in 0 until n) {
            newCoeffs[i] = coefficients[i] + remainder
            remainder = newCoeffs[i] * root
        }
        return newCoeffs
    }

    private fun normalize(vector: DoubleArray): DoubleArray {
        var sum = 0.0
        for (value in vector) {
            sum += value * value
        }
        val norm = sqrt(sum)
        return if (norm < 1e-10) vector else vector.map { it / norm }.toDoubleArray()
    }

    private fun findEigenvector(matrix: Array<DoubleArray>, eigenvalue: Double): DoubleArray {
        val n = matrix.size
        val augmented = Array(n) { DoubleArray(n) }

        for (i in 0 until n) {
            for (j in 0 until n) {
                augmented[i][j] = if (i == j) matrix[i][j] - eigenvalue else matrix[i][j]
            }
        }

        for (col in 0 until n - 1) {
            var maxRow = col
            for (row in col + 1 until n) {
                if (abs(augmented[row][col]) > abs(augmented[maxRow][col])) {
                    maxRow = row
                }
            }

            if (maxRow != col) {
                val temp = augmented[col]
                augmented[col] = augmented[maxRow]
                augmented[maxRow] = temp
            }

            for (row in col + 1 until n) {
                val factor = augmented[row][col] / augmented[col][col]
                for (j in col until n) {
                    augmented[row][j] -= factor * augmented[col][j]
                }
            }
        }

        val eigenvector = DoubleArray(n)
        eigenvector[n - 1] = 1.0

        for (i in n - 2 downTo 0) {
            var sum = 0.0
            for (j in i + 1 until n) {
                sum += augmented[i][j] * eigenvector[j]
            }
            eigenvector[i] = -sum / augmented[i][i]
        }

        return normalize(eigenvector)
    }

    private fun orthogonalize(matrix: Array<DoubleArray>) {
        val m = matrix.size
        val n = matrix[0].size

        for (i in 0 until n) {
            for (j in 0 until i) {
                var dot = 0.0
                for (k in 0 until m) {
                    dot += matrix[k][i] * matrix[k][j]
                }
                for (k in 0 until m) {
                    matrix[k][i] -= dot * matrix[k][j]
                }
            }

            var norm = 0.0
            for (k in 0 until m) {
                norm += matrix[k][i] * matrix[k][i]
            }
            norm = sqrt(norm)

            if (norm > 1e-10) {
                for (k in 0 until m) {
                    matrix[k][i] /= norm
                }
            }
        }
    }

    private fun computeLibrarySVD(matrix: Array<DoubleArray>): String {
        val m = matrix.size
        val n = matrix[0].size
        val simpleMatrix = SimpleMatrix(m, n)

        for (i in 0 until m) {
            for (j in 0 until n) {
                simpleMatrix.set(i, j, matrix[i][j])
            }
        }

        val svd = simpleMatrix.svd()

        val U = svd.u
        val W = svd.w
        val V = svd.v

        return buildString {
            append("Матрица U:\n${matrixToString(U)}\n")
            append("\nМатрица Σ:\n${matrixToString(W)}\n")
            append("\nМатрица Vᵀ:\n${matrixToString(V.transpose())}\n")

            val reconstructed = U.mult(W).mult(V.transpose())
            append("\nПроверка реконструкции (A = UΣVᵀ):\n")
            append("Исходная A:\n")
            append(matrixToString(simpleMatrix))
            append("\nВосстановленная A:\n")
            append(matrixToString(reconstructed))

            append("\nСингулярные значения:\n")
            for (i in 0 until min(m, n)) {
                append(String.format(Locale.getDefault(), "σ%d = %.6f\n", i + 1, W[i, i]))
            }
        }
    }

    private fun computeSVD(A: Array<DoubleArray>): String {
        val m = A.size
        val n = A[0].size

        val AT = transpose(A)
        val ATA = multiplyMatrices(AT, A)

        val charPoly = danilevskyMethod(ATA)
        val eigenvalues = findAllEigenvalues(charPoly)

        val sortedEigenvalues = eigenvalues.sortedArrayDescending()
        val singularValues = sortedEigenvalues.map {
            if (it < 0) {
                if (abs(it) < 1e-10) 0.0
                else throw RuntimeException("Отрицательное собственное значение: $it")
            } else {
                sqrt(it)
            }
        }.toDoubleArray()

        val V = Array(n) { DoubleArray(n) }
        for (i in 0 until n) {
            try {
                val eigenvector = findEigenvector(ATA, sortedEigenvalues[i])
                for (j in 0 until n) {
                    V[j][i] = eigenvector[j]
                }
            } catch (e: Exception) {
                for (j in 0 until n) {
                    V[j][i] = if (j == i) 1.0 else 0.0
                }
            }
        }

        val U = Array(m) { DoubleArray(m) }
        var rank = 0

        for (i in 0 until min(m, n)) {
            if (singularValues[i] > 1e-10) {
                rank++
                val vi = DoubleArray(n) { j -> V[j][i] }
                val uTemp = DoubleArray(m)

                for (row in 0 until m) {
                    var sum = 0.0
                    for (col in 0 until n) {
                        sum += A[row][col] * vi[col]
                    }
                    uTemp[row] = sum / singularValues[i]
                }

                var norm = 0.0
                for (value in uTemp) {
                    norm += value * value
                }
                norm = sqrt(norm)

                for (row in 0 until m) {
                    U[row][i] = if (norm > 1e-10) uTemp[row] / norm else 0.0
                }
            }
        }

        for (i in rank until m) {
            for (j in 0 until m) {
                U[j][i] = if (i == j) 1.0 else 0.0
            }
        }

        orthogonalize(U)

        return buildString {
            append("Сингулярные значения:\n")
            for (i in 0 until min(m, n)) {
                append(String.format(Locale.getDefault(), "σ%d = %.6f\n", i + 1, singularValues[i]))
            }

            append("\nМатрица U (${m}x${m}):\n")
            for (i in 0 until m) {
                append("[")
                for (j in 0 until m) {
                    append(String.format(Locale.getDefault(), "%10.6f", U[i][j]))
                    if (j < m - 1) append(", ")
                }
                append("]\n")
            }

            append("\nМатрица Σ (${m}x${n}):\n")
            for (i in 0 until m) {
                append("[")
                for (j in 0 until n) {
                    val value = if (i == j && i < singularValues.size) singularValues[i] else 0.0
                    append(String.format(Locale.getDefault(), "%10.6f", value))
                    if (j < n - 1) append(", ")
                }
                append("]\n")
            }

            append("\nМатрица Vᵀ (${n}x${n}):\n")
            for (i in 0 until n) {
                append("[")
                for (j in 0 until n) {
                    append(String.format(Locale.getDefault(), "%10.6f", V[j][i]))
                    if (j < n - 1) append(", ")
                }
                append("]\n")
            }
        }
    }
}