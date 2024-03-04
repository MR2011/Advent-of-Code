#!/bin/bash

input="../input/day07.txt"

declare -a hands=()

type() {
    local c0=$1
    local c1=$2
    local jokers=$3
    if (( (c0 + jokers) == 5 )); then
        echo "7"
    elif (( (c0 + jokers) == 4 )); then
        echo "6"
    elif (( (c0 + jokers) == 3 && c1 == 2 )); then
        echo "5"
    elif (( (c0 + jokers) == 3 )); then
        echo "4"
    elif (( c0 == 2 && jokers == 0 && c1 == 2 )); then
        echo "3"
    elif (( c0 == 2 || jokers == 1 )); then
        echo "2"
    else
        echo "1"
    fi
}

# translate characters to allow sorting
translate() {
    local jk
    jk=$([[ -n "$2" ]] && echo "1" || echo "B")
    echo "$1" | tr 'A' 'E' | tr 'K' 'D' | tr 'Q' 'C' | tr 'J' "$jk" | tr 'T' 'A'
}

evaluate() {
    local counts c0 c1 t translated
    while read -r hand bid; do
        counts=$(echo -n "$hand" | grep -o . | sort| uniq -c | awk '{printf "%d", $1}' | grep -o . | sort -r | tr -d '\n')
        t="$(type "${counts:0:1}" "${counts:1:1}" "0")"
        translated="$(translate "$hand")"
        hands+=("$t $translated $bid")
    done < "$1"
}

evaluate_with_jokers() {
    local counts jokers c0 c1 t translated
    while read -r hand bid; do
        counts=$(echo -n "$hand" | tr -d 'J' | grep -o . | sort| uniq -c | awk '{printf "%d", $1}' | grep -o . | sort -r | tr -d '\n')
        jokers=$(grep -o 'J' <<< "$hand" | wc -l)
        t="$(type "${counts:0:1}" "${counts:1:1}" "$jokers")"
        translated="$(translate "$hand" true)"
        hands+=("$t $translated $bid")
    done < "$1"
}

calc_total_winning() {
    local total=0
    local i=1
    local sorted_hands
    sorted_hands=$(printf "%s\n" "${hands[@]}" | sort -k1,1 -k2,2)
    while read -r t translated bid; do
        ((total+=i*bid))
        ((i++))
    done <<< "$sorted_hands"
    echo "$total"
}


part1() {
    evaluate $input
    calc_total_winning
}

part2() {
    evaluate_with_jokers $input
    calc_total_winning
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"