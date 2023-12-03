#!/bin/bash

input="../input/day01.txt"

# Replace match with (match)digit(match)
# Example
# eightwo => eight8eighttwo2two
# Then use solution of part 1
# Source https://news.ycombinator.com/item?id=38483271
digitize() {
    sed -E 's/(one)/\11\1/g; s/(two)/\12\1/g; s/(three)/\13\1/g; s/(four)/\14\1/g; s/(five)/\15\1/g; s/(six)/\16\1/g; s/(seven)/\17\1/g; s/(eight)/\18\1/g; s/(nine)/\19\1/g;' < $1
}

addDigits() {
    sum=0
    while read -r line; do
        digit="${line:0:1}${line: -1}"
        sum=$((sum + digit))
    done < $1
    echo "$sum"
}

part1() {
    addDigits <(tr -dc "0-9,\n" < "$input")
}

part2() {
    addDigits <(digitize "$input" | tr -dc "0-9,\n")
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"