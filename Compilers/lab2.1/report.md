% Лабораторная работа № 2.1. Синтаксические деревья
% 18 февраля 2025 г.
% 

# Цель работы
Целью данной работы является изучение представления синтаксических деревьев 
в памяти компилятора и приобретение навыков преобразования синтаксических деревьев.

# Индивидуальный вариант
Вместо каждой именованной константы, имя которой заканчивается на _, подставить её значение.

# Реализация

Демонстрационная программа:

```go
package main

import "fmt"

const A_ = 42
const B_ = "Hello"
const C = 50
const D_ = 10
const N_ = 5

func main() {
	fmt.Println(A_)
	fmt.Println(B_)
	fmt.Println(C)
	fmt.Println(A_ + D_)
	fmt.Println(A_)
	fmt.Println(sunmD(A_, C))
	for i := 0; i < N_; i++ {
		fmt.Println(i)
	}
}

func sunmD(a, b int) int {
	return D_ + a + b
}

```

Программа, осуществляющая преобразование синтаксического дерева:

```go
package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"strings"
)

func main() {
	// Создаём набор токенов и парсим исходный файл demo.go
	fset := token.NewFileSet()
	node, err := parser.ParseFile(fset, "demo.go", nil, parser.ParseComments)
	if err != nil {
		panic(err)
	}

	// Карта для хранения значений констант, имена которых оканчиваются на "_"
	constValues := make(map[string]ast.Expr)
	// Карта для отслеживания использованных констант
	usedConsts := make(map[string]bool)

	// Обход AST-дерева: находим константы, имена которых заканчиваются на "_"
	ast.Inspect(node, func(n ast.Node) bool {
		if decl, ok := n.(*ast.GenDecl); ok && decl.Tok == token.CONST {
			for _, spec := range decl.Specs {
				if vs, ok := spec.(*ast.ValueSpec); ok {
					for i, name := range vs.Names {
						if strings.HasSuffix(name.Name, "_") && i < len(vs.Values) {
							constValues[name.Name] = vs.Values[i]
						}
					}
				}
			}
		}
		return true
	})

	// Обход AST-дерева: замена использования констант их значениями
	ast.Inspect(node, func(n ast.Node) bool {
		switch x := n.(type) {
		// Выражение-оператор
		case *ast.ExprStmt:
			replaceExpr(&x.X, constValues, usedConsts)

		// Оператор присваивания
		case *ast.AssignStmt:
			for i := range x.Rhs {
				replaceExpr(&x.Rhs[i], constValues, usedConsts)
			}

		// Оператор return
		case *ast.ReturnStmt:
			for i := range x.Results {
				replaceExpr(&x.Results[i], constValues, usedConsts)
			}

		// Вызов функции
		case *ast.CallExpr:
			for i := range x.Args {
				replaceExpr(&x.Args[i], constValues, usedConsts)
			}

		// Бинарное выражение
		case *ast.BinaryExpr:
			replaceExpr(&x.X, constValues, usedConsts)
			replaceExpr(&x.Y, constValues, usedConsts)

		// Унарное выражение
		case *ast.UnaryExpr:
			replaceExpr(&x.X, constValues, usedConsts)
		}
		return true
	})

	// Удаление неиспользуемых констант из AST
	var decls []ast.Decl
	for _, d := range node.Decls {
		if gd, ok := d.(*ast.GenDecl); ok && gd.Tok == token.CONST {
			var specs []ast.Spec
			for _, s := range gd.Specs {
				vs, ok := s.(*ast.ValueSpec)
				if !ok {
					specs = append(specs, s)
					continue
				}
				var names []*ast.Ident
				for _, n := range vs.Names {
					if !usedConsts[n.Name] {
						names = append(names, n)
					}
				}
				if len(names) > 0 {
					vs.Names = names
					specs = append(specs, vs)
				}
			}
			if len(specs) > 0 {
				gd.Specs = specs
				decls = append(decls, gd)
			}
		} else {
			decls = append(decls, d)
		}
	}
	node.Decls = decls

	// Форматирование и вывод изменённого кода
	var buf bytes.Buffer
	if err := format.Node(&buf, fset, node); err != nil {
		panic(err)
	}
	fmt.Println(buf.String())
}

// Функция заменяет идентификаторы, соответствующие константам, их значениями
func replaceExpr(e *ast.Expr, constValues map[string]ast.Expr, usedConsts map[string]bool) {
	if id, ok := (*e).(*ast.Ident); ok {
		if val, exists := constValues[id.Name]; exists {
			// Создаём копию узла с обнулёнными позициями
			switch v := val.(type) {
			case *ast.BasicLit:
				*e = &ast.BasicLit{Kind: v.Kind, Value: v.Value}
			default:
				*e = val
			}
			usedConsts[id.Name] = true
		}
	}
}
```

# Тестирование

Результат трансформации демонстрационной программы:

```go
package main

import "fmt"

const C = 50

func main() {
	fmt.Println(42)
	fmt.Println("Hello")
	fmt.Println(C)
	fmt.Println(42 + 10)
	fmt.Println(42)
	fmt.Println(sunmD(42, C))
	for i := 0; i < 5; i++ {
		fmt.Println(i)
	}
}

func sunmD(a, b int) int {
	return 10 + a + b
}

```

# Вывод
В ходе выполнения лабораторной работы были изучены принципы работы с синтаксическими деревьями (AST) 
в языке Go. На практике была реализована программа, выполняющая преобразование исходного кода путём 
замены всех констант, имена которых оканчиваются на _, их значениями. 
Также было произведено удаление неиспользуемых объявлений.
Программа успешно обработала все случаи использования констант с  
в демонстрационном примере и заменила их на значения констант.
