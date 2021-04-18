package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "sort"
)

type Recipe struct {
    ingredients []string
    allergens []string
}

func parseInput() []Recipe {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    allergens := []string{}
    ingredients := []string{}
    recipes := []Recipe{}
    for scanner.Scan() {
	tokens := strings.Split(scanner.Text(), " (contains ")
	ingredients = strings.Split(tokens[0], " ")
	tokens[1] = strings.ReplaceAll(tokens[1], ")", "")
	allergens = strings.Split(tokens[1], ", ")
	recipes = append(recipes, Recipe{ingredients, allergens})
    }
    return recipes
}

func contains(s []string, str string) bool {
    for _, v := range s {
	if v == str {
	    return true
	}
    }
    return false
}

func difference(a []string, b []string) []string {
    c := []string{}

    for _, bb := range b {
	if !contains(a, bb) {
	    c = append(c, bb)
	}
    }
    return c
}

func removeAllergen(s []string, item string) []string {
    index := 0
    for _, i := range s {
        if i != item {
            s[index] = i
            index++
        }
    }
    return s[:index]
}

func possibleAllergensForIngredients(recipes []Recipe) map[string][]string {
    m := map[string][]string{}
    for _, recipe := range recipes {
	for _, ingredient := range recipe.ingredients {
	    if m[ingredient] == nil {
		m[ingredient] = []string{}
	    }
	    for _, allergen := range recipe.allergens {
		m[ingredient] = append(m[ingredient], allergen)
	    }
	}
    }
    return m
}

func eliminateAllergens(
    recipes []Recipe,
    possibleAllergens map[string][]string,
) map[string][]string {
    for i, recipe := range recipes {
	for j, recipe2 := range recipes {
	    if j == i {
		continue
	    }
	    unsetAllergen := difference(recipe.ingredients, recipe2.ingredients)
	    for _, allergen := range recipe.allergens {
		for _, ingredient := range unsetAllergen {
		    possibleAllergens[ingredient] = removeAllergen(
			possibleAllergens[ingredient],
			allergen,
		    )
		}
	    }
	}
    }
    return possibleAllergens
}

func part1() int {
    recipes := parseInput()
    possibleAllergens := possibleAllergensForIngredients(recipes)
    possibleAllergens = eliminateAllergens(recipes, possibleAllergens)
    count := 0
    for ingredient, allergens := range possibleAllergens {
	if len(allergens) == 0 {
	    for _, recipe := range recipes {
		if contains(recipe.ingredients, ingredient) {
		    count++
		}
	    }
	}
    }
    return count
}

func part2() string {
    recipes := parseInput()
    possibleAllergens := possibleAllergensForIngredients(recipes)
    possibleAllergens = eliminateAllergens(recipes, possibleAllergens)
    s := []string{}
    for ingredient, allergens := range possibleAllergens {
	if len(allergens) > 0 {
	    s = append(s, ingredient)
	}
    }
    sort.Slice(s, func(i, j int) bool { return s[i] < s[j] })
    return strings.Join(s[:], ",")
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %s\n", part2())
}
