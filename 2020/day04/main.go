package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strings"
    "strconv"
)

type fieldValidator func(string) bool

var passwordValidator = map[string]fieldValidator {
    "byr": func(value string) bool {
	num := stoi(value)
	return num >= 1920 && num <= 2002
    },
    "iyr": func(value string) bool {
	num := stoi(value)
	return num >= 2010 && num <= 2020
    },
    "eyr": func(value string) bool {
	num := stoi(value)
	return num >= 2020 && num <= 2030
    },
    "hgt": func(value string) bool {
	if strings.HasSuffix(value, "cm") {
	    num := stoi(value[:len(value)-2])
	    return num >= 150 && num <= 193
	}else if strings.HasSuffix(value, "in") {
	    num := stoi(value[:len(value)-2])
	    return num >= 59 && num <= 76
	} else {
	    return false
	}
    },
    "hcl": func(value string) bool {
	matched, _ := regexp.MatchString(`^#[\da-f]{6}$`, value)
	return matched
    },
    "ecl": func(value string) bool {
	switch value {
	case "amb", "blu", "brn", "gry", "grn", "hzl", "oth":
	    return true
	}
	return false
    },
    "pid": func(value string) bool {
	matched, _ := regexp.MatchString(`^\d{9}$`, value)
	return matched
    },
    "cid": func(value string) bool {
	return true
    },
}


type Passport struct {
    fields map[string]string
}

func (p *Passport) init() {
    p.fields["byr"] = ""
    p.fields["iyr"] = ""
    p.fields["eyr"] = ""
    p.fields["hgt"] = ""
    p.fields["hcl"] = ""
    p.fields["ecl"] = ""
    p.fields["pid"] = ""
    p.fields["cid"] = ""
}

func (p Passport) valid() bool {
    for key, value := range p.fields {
	if value == "" && key != "cid" {
	    return false
	}
    }
    return true
}

func (p Passport) strictValid() bool {
    for key, value := range p.fields {
	if !passwordValidator[key](value) {
	    return false
	}
    }
    return true
}

func stoi(s string) int {
    i, err := strconv.Atoi(s)
    if err != nil {
	return -1
    } else {
	return i
    }
}

func parseInput() []Passport {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    passports := []Passport{}
    passport := Passport{fields: map[string]string{}}
    passport.init()
    for scanner.Scan() {
	if len(scanner.Text()) == 0 {
	    passports = append(passports, passport)
	    passport = Passport{fields: map[string]string{}}
	    passport.init()
	} else {
	    for _, token := range strings.Fields(scanner.Text()) {
		words := strings.Split(token, ":")
		passport.fields[words[0]] = words[1]
	    }
	}
    }
    passports = append(passports, passport)
    return passports
}


func part1() int {
    passports := parseInput()
    valid := 0
    for _, p := range passports {
	if p.valid() {
	    valid++
	}
    }
    return valid
}

func part2() int {
    passports := parseInput()
    valid := 0
    for _, p := range passports {
	if p.strictValid() {
	    valid++
	}
    }
    return valid
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
