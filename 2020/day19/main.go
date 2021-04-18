package main

import (
    "fmt"
    "io/ioutil"
    "regexp"
    "strings"
)

func parseInput() (map[string]string, []string){
    data, _ := ioutil.ReadFile("input")
    parts := strings.Split(string(data), "\n\n")
    rules := map[string]string{}
    messages := []string{}
    for _, line := range strings.Split(parts[0], "\n") {
	tokens := strings.Split(line, ": ")
	rules[tokens[0]] = tokens[1]
    }
    for _, line := range strings.Split(parts[1], "\n") {
	messages = append(messages, line)
    }
    return rules, messages
}

func resolveRule(id string, rules map[string]string) string {
    if rules[id][0] == '"' {
	return string(rules[id][1])
    }
    regex := "("
    optionsRegex := []string{}
    for _, option := range strings.Split(rules[id], "|") {
	optionRegex := ""
	for _, nextId := range strings.Fields(option) {
	    optionRegex +=  resolveRule(nextId, rules)
	}
	optionsRegex = append(optionsRegex, optionRegex)
    }
    regex += strings.Join(optionsRegex, "|")
    regex += ")"
    return regex
}

func part1() int {
    rules, messages := parseInput()
    reStr := resolveRule("0", rules)
    re := regexp.MustCompile("^" + reStr + "$")
    matches := 0
    for _, msg := range messages {
	if re.MatchString(msg) {
	    matches++
	}
    }
    return matches
}

func part2() int {
    rules, messages := parseInput()
    // Prevent loop by inserting the first 10 iterations of
    // 8: 42 | 42 8
    // 11: 42 31 | 42 11 31
    for i:=1; i<=10; i++ {
	rules["8"] += " |" + strings.Repeat(" 42", i)
	rules["11"] += " |" + strings.Repeat(" 42", i) + strings.Repeat(" 31", i)
    }
    reStr := resolveRule("0", rules)
    re := regexp.MustCompile("^" + reStr + "$")
    matches := 0
    for _, msg := range messages {
	if re.MatchString(msg) {
	    matches++
	}
    }
    return matches
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
