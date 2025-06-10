package cfg

import (
	"OptimalCodeGen/lab4/ast"
	"fmt"
	"os"
)

// BasicBlock представляет базовый блок CFG с инструкциями и связями.
type BasicBlock struct {
	Name         string
	Instructions []Instruction
	Next         *BasicBlock
	Branch       *BasicBlock
	Incoming     []*BasicBlock
	Dominators   []*BasicBlock
	DomFrontier  []*BasicBlock
}

// Instruction — интерфейс для инструкций CFG
type Instruction interface {
	String() string
}

// AssignInst — инструкция присваивания
type AssignInst struct {
	Dest string
	Expr ast.Expr
}

func (a AssignInst) String() string {
	return fmt.Sprintf("%s <- %s", a.Dest, a.Expr.String())
}

// CondInst — инструкция условия
type CondInst struct {
	Expr ast.Expr
}

func (c CondInst) String() string {
	return fmt.Sprintf("cond %s", c.Expr.String())
}

// CFG представляет граф потока управления.
type CFG struct {
	Entry  *BasicBlock
	Blocks []*BasicBlock
}

// BuildCFG строит CFG из AST, создавая блоки и связи.
func BuildCFG(block *ast.Block) *CFG {
	cfg := &CFG{}
	entry := &BasicBlock{Name: "BLOCK 0"}
	cfg.Entry = entry
	cfg.Blocks = append(cfg.Blocks, entry)

	current := entry
	tempCounter := 0
	for _, stmt := range block.Statements {
		switch s := stmt.(type) {
		case ast.Assignment:
			fmt.Printf("Adding assignment: %s to block %s\n", s.String(), current.Name)
			expr, temps := generateTempInstructions(s.Expr, &tempCounter)
			current.Instructions = append(current.Instructions, temps...)
			current.Instructions = append(current.Instructions, AssignInst{
				Dest: s.Name,
				Expr: expr,
			})

		case ast.IfStmt:
			fmt.Printf("Processing if statement in block %s\n", current.Name)
			condBlock := &BasicBlock{Name: fmt.Sprintf("BLOCK %d", len(cfg.Blocks))}
			cfg.Blocks = append(cfg.Blocks, condBlock)
			current.Next = condBlock
			condBlock.Incoming = append(condBlock.Incoming, current)
			fmt.Printf("Created cond block %s, linked from %s\n", condBlock.Name, current.Name)

			expr, temps := generateTempInstructions(s.Cond, &tempCounter)
			condBlock.Instructions = append(condBlock.Instructions, temps...)
			condBlock.Instructions = append(condBlock.Instructions, CondInst{Expr: expr})
			fmt.Printf("Added condition: %s to block %s\n", s.Cond.String(), condBlock.Name)

			thenBlock := &BasicBlock{Name: fmt.Sprintf("BLOCK %d", len(cfg.Blocks))}
			cfg.Blocks = append(cfg.Blocks, thenBlock)
			condBlock.Branch = thenBlock
			thenBlock.Incoming = append(thenBlock.Incoming, condBlock)
			fmt.Printf("Created then block %s, linked from %s\n", thenBlock.Name, condBlock.Name)

			mergeBlock := &BasicBlock{Name: fmt.Sprintf("BLOCK %d", len(cfg.Blocks))}
			cfg.Blocks = append(cfg.Blocks, mergeBlock)
			fmt.Printf("Created merge block %s\n", mergeBlock.Name)

			for _, stmt := range s.Then.Statements {
				if assign, ok := stmt.(ast.Assignment); ok {
					fmt.Printf("Adding assignment: %s to block %s\n", assign.String(), thenBlock.Name)
					expr, temps := generateTempInstructions(assign.Expr, &tempCounter)
					thenBlock.Instructions = append(thenBlock.Instructions, temps...)
					thenBlock.Instructions = append(thenBlock.Instructions, AssignInst{
						Dest: assign.Name,
						Expr: expr,
					})
				}
			}
			thenBlock.Next = mergeBlock
			mergeBlock.Incoming = append(mergeBlock.Incoming, thenBlock)
			fmt.Printf("Linked then block %s to merge %s\n", thenBlock.Name, mergeBlock.Name)

			var elseBlock *BasicBlock
			if s.Else != nil {
				elseBlock = &BasicBlock{Name: fmt.Sprintf("BLOCK %d", len(cfg.Blocks))}
				cfg.Blocks = append(cfg.Blocks, elseBlock)
				condBlock.Next = elseBlock
				elseBlock.Incoming = append(elseBlock.Incoming, condBlock)
				fmt.Printf("Created else block %s, linked from %s\n", elseBlock.Name, condBlock.Name)

				for _, stmt := range s.Else.Statements {
					if assign, ok := stmt.(ast.Assignment); ok {
						fmt.Printf("Adding assignment: %s to block %s\n", assign.String(), elseBlock.Name)
						expr, temps := generateTempInstructions(assign.Expr, &tempCounter)
						elseBlock.Instructions = append(elseBlock.Instructions, temps...)
						elseBlock.Instructions = append(thenBlock.Instructions, AssignInst{
							Dest: assign.Name,
							Expr: expr,
						})
					}
				}
				elseBlock.Next = mergeBlock
				mergeBlock.Incoming = append(mergeBlock.Incoming, elseBlock)
				fmt.Printf("Linked else block %s to merge %s\n", elseBlock.Name, mergeBlock.Name)
			} else {
				condBlock.Next = mergeBlock
				mergeBlock.Incoming = append(mergeBlock.Incoming, condBlock)
				fmt.Printf("Linked cond block %s to merge %s (no else)\n", condBlock.Name, mergeBlock.Name)
			}

			current = mergeBlock
			fmt.Printf("Set current block to %s\n", current.Name)
		}
	}

	fmt.Printf("Finished building CFG with %d blocks\n", len(cfg.Blocks))
	fmt.Println("Computing dominance")
	cfg.computeDominance()
	fmt.Println("Dominance computation completed")
	return cfg
}

// generateTempInstructions разбивает выражение и создает временные переменные.
func generateTempInstructions(expr ast.Expr, tempCounter *int) (ast.Expr, []Instruction) {
	switch e := expr.(type) {
	case ast.BinaryOp:
		left, leftTemps := generateTempInstructions(e.Left, tempCounter)
		right, rightTemps := generateTempInstructions(e.Right, tempCounter)
		tempVar := fmt.Sprintf("tmp_%d_%d", len(leftTemps)+len(rightTemps), *tempCounter)
		*tempCounter++
		instr := AssignInst{
			Dest: tempVar,
			Expr: ast.BinaryOp{Left: left, Op: e.Op, Right: right},
		}
		return ast.Variable{Name: tempVar}, append(append(leftTemps, rightTemps...), instr)
	default:
		return expr, nil
	}
}

// computeDominance вычисляет доминаторы и границы доминирования для всех блоков.
func (c *CFG) computeDominance() {
	for _, block := range c.Blocks {
		block.Dominators = make([]*BasicBlock, 0, len(c.Blocks))
		block.DomFrontier = nil
	}

	entry := c.Entry
	entry.Dominators = []*BasicBlock{entry}

	// Итеративно вычисляем доминаторы до сходимости.
	changed := true
	iteration := 0
	maxIterations := 1000
	for changed && iteration < maxIterations {
		changed = false
		iteration++
		for _, block := range c.Blocks {
			if block == entry {
				continue
			}

			var newDoms []*BasicBlock
			if len(block.Incoming) > 0 {
				newDoms = make([]*BasicBlock, len(block.Incoming[0].Dominators))
				copy(newDoms, block.Incoming[0].Dominators)
				for _, pred := range block.Incoming[1:] {
					if len(pred.Dominators) == 0 {
						newDoms = []*BasicBlock{block}
						continue
					}
					newDoms = intersect(newDoms, pred.Dominators)
				}
			} else {
				newDoms = []*BasicBlock{block}
			}

			newDoms = append(newDoms, block)

			if !equal(block.Dominators, newDoms) {
				block.Dominators = newDoms
				changed = true
			}
		}
	}

	// Вычисляем границы доминирования для блоков с несколькими входящими рёбрами.
	for _, block := range c.Blocks {
		if len(block.Incoming) > 1 {
			for _, pred := range block.Incoming {
				runner := pred
				for runner != nil && !contains(runner.Dominators, block) {
					if contains(runner.DomFrontier, block) {
						break
					}
					runner.DomFrontier = append(runner.DomFrontier, block)
					if len(runner.Dominators) == 0 {
						break
					}
					runner = runner.Dominators[len(runner.Dominators)-1]
				}
			}
		}
	}

	fmt.Printf("Dominance computed after %d iterations\n", iteration)
}

// intersect возвращает пересечение двух списков блоков.
func intersect(a, b []*BasicBlock) []*BasicBlock {
	m := make(map[*BasicBlock]bool)
	for _, v := range a {
		m[v] = true
	}

	var result []*BasicBlock
	for _, v := range b {
		if m[v] {
			result = append(result, v)
		}
	}
	return result
}

// equal проверяет равенство двух списков блоков.
func equal(a, b []*BasicBlock) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// contains проверяет, содержится ли блок в списке.
func contains(slice []*BasicBlock, item *BasicBlock) bool {
	for _, v := range slice {
		if v == item {
			return true
		}
	}
	return false
}

// VisualizeCFG создаёт файл dot для визуализации CFG-графа.
func VisualizeCFG(cfg *CFG, filename string) error {
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

	for i, block := range cfg.Blocks {
		label := fmt.Sprintf("%s\\l", block.Name)
		for _, inst := range block.Instructions {
			label += fmt.Sprintf("    %s\\l", inst.String())
		}
		if block.Next != nil && block.Branch != nil {
			label += fmt.Sprintf("    if (!%s) go to %s else go to %s\\l", block.Instructions[len(block.Instructions)-1].(CondInst).Expr.String(), block.Next.Name, block.Branch.Name)
		} else if block.Next != nil {
			label += fmt.Sprintf("    go to %s\\l", block.Next.Name)
		}
		_, err = file.WriteString(fmt.Sprintf("  %d [label=\"%s\"];\n", i, label))
		if err != nil {
			return err
		}
	}

	// Добавляем рёбра с метками true/false для условных переходов.
	for i, block := range cfg.Blocks {
		if block.Next != nil {
			for j, b := range cfg.Blocks {
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
			for j, b := range cfg.Blocks {
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
