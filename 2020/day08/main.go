package main

import (
    "bufio"
    "fmt"
    "os"
)

type Instruction struct {
    op string
    arg int
}


func parseInput() []Instruction {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    instructions := []Instruction{}
    for scanner.Scan() {
	op, arg := "", 0 
	fmt.Sscanf(scanner.Text(), "%s %d", &op, &arg)
	instruction := Instruction{op: op, arg: arg}
	instructions = append(instructions, instruction)
    }
    return instructions
}

func run(instructions []Instruction) (int, bool) {
    visited := map[int]bool{}
    acc := 0
    pc := 0
    for pc < len(instructions){
	if visited[pc] {
	    return acc, false
	} else {
	    visited[pc] = true
	}
	switch instructions[pc].op {
	case "acc":
	    acc += instructions[pc].arg
	    pc++
	case "jmp":
	    pc += instructions[pc].arg
	case "nop":
	    pc++
	}
    }
    return acc, true
}

func part1() int {
    instructions := parseInput()
    acc, _ := run(instructions)
    return acc
}

func part2() int {
    instructions := parseInput()
    inverse := map[string]string {
	"jmp": "nop",
	"nop": "jmp",
    }
    for i, instruction := range instructions {
	if instruction.op == "nop" || instruction.op == "jmp" {
	    instructions[i].op = inverse[instructions[i].op]
	    acc, terminated := run(instructions)
	    instructions[i].op = inverse[instructions[i].op]
	    if terminated {
		return acc
	    }
	}
    }
    return -1
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
