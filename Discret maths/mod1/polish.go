package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// "(-(-38)3)"
func polish(s string) int {
	if len(s) == 0 {
		return 0
	}
	res := 0
	for s[0] == ' ' {
		s = s[1:]
	}
	for s[len(s)-1] == ' ' {
		s = s[:len(s)-1]
	}
	if len(s) == 1 {
		res, _ = strconv.Atoi(s)
		return res
	}
	znak := ' '
	e1 := ""
	e2 := ""
	if s[0] == '(' {
		for i := 1; i < len(s)-1; i++ {
			if s[i] == '+' || s[i] == '-' || s[i] == '*' {
				znak = int32(s[i])
				continue
			} else if s[i] == '(' {
				if e1 == "" {
					e1 += string(s[i])
					i++
					count := 1
					for count != 0 {
						if s[i] == '(' {
							count++
						} else if s[i] == ')' {
							count--
						}
						e1 += string(s[i])
						i++
					}
					i -= 1

				} else {
					e2 += string(s[i])
					i++
					count := 1
					for count != 0 {
						if s[i] == '(' {
							count++
						} else if s[i] == ')' {
							count--
						}
						e2 += string(s[i])
						i++
					}
					i -= 1
				}

			} else {
				if s[i] == ' ' {
					continue
				} else if e1 == "" {
					e1 += string(s[i])
				} else {
					e2 += string(s[i])

				}
			}

		}
	}
	if znak == '*' {
		res = polish(e1) * polish(e2)
	} else if znak == '+' {
		res = polish(e1) + polish(e2)
	} else if znak == '-' {
		res = polish(e1) - polish(e2)
	}
	/*
		fmt.Println(string(znak))
		fmt.Println(e1)
		fmt.Println(e2)
		fmt.Println(res)


	*/
	return res
}

func main564() {

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	expr := scanner.Text()

	//fmt.Println(expr)
	//expr := "(+ (- (- 3 2 ) 5 ) (+ 0 4 ) )" // -8
	res := polish(expr)
	fmt.Println(res)
	//fmt.Println(int('1') - 48)

}

//++++++
