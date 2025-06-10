package ssa

import (
	"OptimalCodeGen/lab4/ast"
	"OptimalCodeGen/lab4/cfg"
	"fmt"
	"os"
	"sort"
	"strings"
)

// SSA представляет граф SSA, содержащий блоки и версии переменных.
type SSA struct {
	CFG      *cfg.CFG
	Versions map[string]int
	Blocks   []*SSABlock
	Defsites map[string][]*SSABlock
}

// SSABlock описывает базовый блок в SSA с инструкциями и связями.
type SSABlock struct {
	Name         string
	Instructions []SSAInstruction
	Next         *SSABlock
	Branch       *SSABlock
	Incoming     []*SSABlock
}

// SSAInstruction — интерфейс для инструкций SSA (присваивание, условие, phi).
type SSAInstruction interface {
	String() string
}

// SSAAssign — инструкция присваивания
type SSAAssign struct {
	Dest string
	Expr ast.Expr
}

func (a SSAAssign) String() string {
	return fmt.Sprintf("%s <- %s", a.Dest, a.Expr.String())
}

// SSACond — инструкция условия
type SSACond struct {
	Expr ast.Expr
}

func (c SSACond) String() string {
	return fmt.Sprintf("cond %s", c.Expr.String())
}

// SSAPhi — phi-функция для слияния версий переменных
type SSAPhi struct {
	Dest   string
	Values []PhiValue
}

type PhiValue struct {
	Value string
	Block string
}

func (p SSAPhi) String() string {
	var vals []string
	for _, v := range p.Values {
		vals = append(vals, fmt.Sprintf("[%s, %s]", v.Value, v.Block))
	}
	return fmt.Sprintf("%s = φ(%s)", p.Dest, strings.Join(vals, ", "))
}

// Convert преобразует CFG в SSA, добавляя phi-функции и переименовывая переменные.
func Convert(inputCFG *cfg.CFG) *SSA {
	ssa := &SSA{
		CFG:      inputCFG,
		Versions: make(map[string]int),
		Blocks:   make([]*SSABlock, 0, len(inputCFG.Blocks)),
		Defsites: make(map[string][]*SSABlock),
	}

	// Копируем блоки и инструкции из CFG в SSA.
	for _, cfgBlock := range inputCFG.Blocks {
		ssaBlock := ssa.getOrCreateBlock(cfgBlock.Name)
		ssaBlock.Incoming = make([]*SSABlock, 0, len(cfgBlock.Incoming))
		for _, inc := range cfgBlock.Incoming {
			ssaBlock.Incoming = append(ssaBlock.Incoming, ssa.getOrCreateBlock(inc.Name))
		}
		if cfgBlock.Next != nil {
			ssaBlock.Next = ssa.getOrCreateBlock(cfgBlock.Next.Name)
		}
		if cfgBlock.Branch != nil {
			ssaBlock.Branch = ssa.getOrCreateBlock(cfgBlock.Branch.Name)
		}
		for _, inst := range cfgBlock.Instructions {
			switch i := inst.(type) {
			case cfg.AssignInst:
				ssaBlock.Instructions = append(ssaBlock.Instructions, SSAAssign{
					Dest: i.Dest,
					Expr: i.Expr,
				})
			case cfg.CondInst:
				ssaBlock.Instructions = append(ssaBlock.Instructions, SSACond{
					Expr: i.Expr,
				})
			default:
				fmt.Printf("Warning: unknown instruction type %T in block %s\n", inst, cfgBlock.Name)
			}
		}
	}

	ssa.placePhiFunctions()
	ssa.renameVariables()
	return ssa
}

// placePhiFunctions добавляет phi-функции в блоки на границе доминирования.
func (s *SSA) placePhiFunctions() {
	// Собираем места определений переменных.
	for _, block := range s.CFG.Blocks {
		for _, inst := range block.Instructions {
			if assign, ok := inst.(cfg.AssignInst); ok {
				s.Defsites[assign.Dest] = append(s.Defsites[assign.Dest], s.getOrCreateBlock(block.Name))
			}
		}
	}

	// Для каждой переменной, определённой в нескольких блоках, добавляем phi-функции.
	for varName, defBlocks := range s.Defsites {
		if len(defBlocks) <= 1 {
			continue
		}

		worklist := make(map[*SSABlock]bool)
		visited := make(map[*SSABlock]bool)
		for _, block := range defBlocks {
			worklist[block] = true
		}

		for len(worklist) > 0 {
			var current *SSABlock
			for b := range worklist {
				current = b
				delete(worklist, b)
				break
			}

			frontier, err := s.findDominanceFrontier(current.Name)
			if err != nil {
				fmt.Printf("Warning: %v\n", err)
				continue
			}

			for _, frontierBlock := range frontier {
				if !s.hasPhiForVar(frontierBlock, varName) && !visited[frontierBlock] {
					// Сортируем входящие блоки для предсказуемого порядка в phi
					incoming := make([]*SSABlock, len(frontierBlock.Incoming))
					copy(incoming, frontierBlock.Incoming)
					for i := 0; i < len(incoming)-1; i++ {
						for j := i + 1; j < len(incoming); j++ {
							if incoming[i].Name > incoming[j].Name {
								incoming[i], incoming[j] = incoming[j], incoming[i]
							}
						}
					}
					phi := SSAPhi{
						Dest:   varName,
						Values: make([]PhiValue, len(incoming)),
					}
					for i, inc := range incoming {
						phi.Values[i] = PhiValue{
							Value: varName,
							Block: inc.Name,
						}
					}
					frontierBlock.Instructions = append([]SSAInstruction{phi}, frontierBlock.Instructions...)
					visited[frontierBlock] = true
					if !worklist[frontierBlock] {
						worklist[frontierBlock] = true
					}
				}
			}
		}
	}
}

// renameVariables переименовывает переменные, присваивая уникальные версии
func (s *SSA) renameVariables() {
	stack := make(map[string][]string)
	visited := make(map[*SSABlock]bool)

	// Фаза 1: Обрабатываем все блоки, не являющиеся точками слияния с phi-функциями.
	// Обрабатываем блоки с <= 1 входящим ребром или без phi-функций, чтобы определить версии переменных.
	for _, block := range s.Blocks {
		// Пропускаем блоки с phi-функциями или с > 1 входящим ребром
		hasPhi := false
		for _, inst := range block.Instructions {
			if _, ok := inst.(SSAPhi); ok {
				hasPhi = true
				break
			}
		}
		if hasPhi || len(block.Incoming) > 1 {
			continue
		}
		if visited[block] {
			continue
		}
		visited[block] = true
		fmt.Printf("Renaming block %s\n", block.Name)

		newInstructions := make([]SSAInstruction, 0, len(block.Instructions))
		for _, inst := range block.Instructions {
			switch instr := inst.(type) {
			case SSAPhi:
				newName := s.newVersion(instr.Dest)
				fmt.Printf("  Phi: %s -> %s\n", instr.Dest, newName)
				stack[instr.Dest] = append(stack[instr.Dest], newName)
				instr.Dest = newName
				newInstructions = append(newInstructions, instr)
			case SSAAssign:
				newExpr := s.renameExpr(instr.Expr, stack)
				newName := s.newVersion(instr.Dest)
				fmt.Printf("  Assign: %s -> %s\n", instr.Dest, newName)
				stack[instr.Dest] = append(stack[instr.Dest], newName)
				newInstructions = append(newInstructions, SSAAssign{
					Dest: newName,
					Expr: newExpr,
				})
			case SSACond:
				newExpr := s.renameExpr(instr.Expr, stack)
				newInstructions = append(newInstructions, SSACond{
					Expr: newExpr,
				})
			}
		}
		block.Instructions = newInstructions

		// Обновляем phi-функции в преемниках, используя последние версии переменных.
		for _, succ := range s.successors(block) {
			// Проходим по всем инструкциям в преемнике
			for i, inst := range succ.Instructions {
				// Проверяем, является ли инструкция φ-функцией
				if phi, ok := inst.(SSAPhi); ok {
					for j, val := range phi.Values {
						if val.Block == block.Name {
							varName := val.Value // Имя переменной до переименования
							if versions, exists := stack[varName]; exists && len(versions) > 0 {
								phi.Values[j].Value = versions[len(versions)-1] // Берём последнюю версию
								fmt.Printf("  Updated phi in %s: %s = %s from %s\n", succ.Name, phi.Dest, phi.Values[j].Value, block.Name)
							} else {
								phi.Values[j].Value = varName + ".0"
								fmt.Printf("  Set default phi in %s: %s = %s from %s\n", succ.Name, phi.Dest, phi.Values[j].Value, block.Name)
							}
						}
					}
					// Обновляем φ-функцию в списке инструкций преемника
					succ.Instructions[i] = phi
				}
			}
		}
	}

	// Фаза 2: Обрабатываем оставшиеся блоки (точки слияния с phi-функциями).
	// Выводим все блоки в порядке по имени
	sortedBlocks := make([]*SSABlock, len(s.Blocks))
	copy(sortedBlocks, s.Blocks)
	sort.Slice(sortedBlocks, func(i, j int) bool {
		return sortedBlocks[i].Name < sortedBlocks[j].Name
	})

	for _, block := range sortedBlocks {
		if !visited[block] {
			visited[block] = true
			fmt.Printf("Renaming block %s\n", block.Name)

			newInstructions := make([]SSAInstruction, 0, len(block.Instructions))
			for _, inst := range block.Instructions {
				switch instr := inst.(type) {
				case SSAPhi:
					newName := s.newVersion(instr.Dest)
					fmt.Printf("  Phi: %s -> %s\n", instr.Dest, newName)
					stack[instr.Dest] = append(stack[instr.Dest], newName)
					instr.Dest = newName
					newInstructions = append(newInstructions, instr)
				case SSAAssign:
					newExpr := s.renameExpr(instr.Expr, stack)
					newName := s.newVersion(instr.Dest)
					fmt.Printf("  Assign: %s -> %s\n", instr.Dest, newName)
					stack[instr.Dest] = append(stack[instr.Dest], newName)
					newInstructions = append(newInstructions, SSAAssign{
						Dest: newName,
						Expr: newExpr,
					})
				case SSACond:
					newExpr := s.renameExpr(instr.Expr, stack)
					newInstructions = append(newInstructions, SSACond{
						Expr: newExpr,
					})
				}
			}
			block.Instructions = newInstructions
		}

		// Выводим содержимое блока в консоль.
		fmt.Printf("BLOCK %s:\n", block.Name)
		for _, inst := range block.Instructions {
			fmt.Printf("    %s\n", inst.String())
		}
		if block.Next != nil && block.Branch != nil {
			for _, inst := range block.Instructions {
				if _, ok := inst.(SSACond); ok {
					fmt.Printf("    if (!%s) go to %s else go to %s\n", inst.(SSACond).Expr.String(), block.Next.Name, block.Branch.Name)
					break
				}
			}
		} else if block.Next != nil {
			fmt.Printf("    go to %s\n", block.Next.Name)
		}
	}
}

// renameExpr заменяет переменные в выражении на их текущие версии из стека.
func (s *SSA) renameExpr(expr ast.Expr, stack map[string][]string) ast.Expr {
	switch e := expr.(type) {
	case ast.Variable:
		if versions, exists := stack[e.Name]; exists && len(versions) > 0 {
			fmt.Printf("    Renaming var %s to %s\n", e.Name, versions[len(versions)-1])
			return ast.Variable{Name: versions[len(versions)-1]}
		}
		return e
	case ast.BinaryOp:
		return ast.BinaryOp{
			Left:  s.renameExpr(e.Left, stack),
			Op:    e.Op,
			Right: s.renameExpr(e.Right, stack),
		}
	case ast.Number:
		return e
	default:
		return expr
	}
}

// newVersion создаёт новую версию переменной
func (s *SSA) newVersion(varName string) string {
	s.Versions[varName]++
	return fmt.Sprintf("%s.%d", varName, s.Versions[varName])
}

// getOrCreateBlock получает или создаёт блок по имени.
func (s *SSA) getOrCreateBlock(name string) *SSABlock {
	for _, b := range s.Blocks {
		if b.Name == name {
			return b
		}
	}
	b := &SSABlock{Name: name}
	s.Blocks = append(s.Blocks, b)
	return b
}

// findDominanceFrontier возвращает границу доминирования для блока.
func (s *SSA) findDominanceFrontier(blockName string) ([]*SSABlock, error) {
	for _, cfgBlock := range s.CFG.Blocks {
		if cfgBlock.Name == blockName {
			var frontier []*SSABlock
			for _, df := range cfgBlock.DomFrontier {
				frontier = append(frontier, s.getOrCreateBlock(df.Name))
			}
			return frontier, nil
		}
	}
	return nil, fmt.Errorf("block %s not found", blockName)
}

// hasPhiForVar проверяет, есть ли phi-функция для переменной в блоке.
func (s *SSA) hasPhiForVar(block *SSABlock, varName string) bool {
	for _, inst := range block.Instructions {
		if phi, ok := inst.(SSAPhi); ok {
			if strings.Split(phi.Dest, ".")[0] == varName {
				return true
			}
		}
	}
	return false
}

// successors возвращает преемников блока (Next и Branch).
func (s *SSA) successors(block *SSABlock) []*SSABlock {
	var succs []*SSABlock
	if block.Next != nil {
		succs = append(succs, block.Next)
	}
	if block.Branch != nil {
		succs = append(succs, block.Branch)
	}
	return succs
}

// VisualizeSSA создаёт файл dot для визуализации SSA-графа.
func VisualizeSSA(ssa *SSA, filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("failed to create file %s: %v", filename, err)
	}
	defer file.Close()

	_, err = file.WriteString("digraph G {\n")
	if err != nil {
		return err
	}
	_, err = file.WriteString("  node [shape=box nojustify=false];\n")
	if err != nil {
		return err
	}

	// Сортируем блоки по имени
	sortedBlocks := make([]*SSABlock, len(ssa.Blocks))
	copy(sortedBlocks, ssa.Blocks)
	sort.Slice(sortedBlocks, func(i, j int) bool {
		return sortedBlocks[i].Name < sortedBlocks[j].Name
	})

	for i, block := range sortedBlocks {
		label := fmt.Sprintf("%s\\l", block.Name)
		for _, inst := range block.Instructions {
			label += fmt.Sprintf("    %s\\l", inst.String())
		}
		if block.Next != nil && block.Branch != nil {
			for _, inst := range block.Instructions {
				if _, ok := inst.(SSACond); ok {
					label += fmt.Sprintf("    if (!%s) go to %s else go to %s\\l", inst.(SSACond).Expr.String(), block.Next.Name, block.Branch.Name)
					break
				}
			}
		} else if block.Next != nil {
			label += fmt.Sprintf("    go to %s\\l", block.Next.Name)
		}
		_, err = file.WriteString(fmt.Sprintf("  %d [label=\"%s\"];\n", i, label))
		if err != nil {
			return err
		}
	}

	// Добавляем рёбра с метками true/false для условных переходов.
	for i, block := range sortedBlocks {
		if block.Next != nil {
			for j, b := range sortedBlocks {
				if b == block.Next {
					_, err = file.WriteString(fmt.Sprintf("  %d -> %d [label=\"true\"];\n", i, j))
					if err != nil {
						return err
					}
					break
				}
			}
		}
		if block.Branch != nil {
			for j, b := range sortedBlocks {
				if b == block.Branch {
					_, err = file.WriteString(fmt.Sprintf("  %d -> %d [label=\"false\"];\n", i, j))
					if err != nil {
						return err
					}
					break
				}
			}
		}
	}

	_, err = file.WriteString("}\n")
	return err
}
