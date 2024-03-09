#!/bin/bash

input="../input/day08.txt"

declare -A network
declare -a currents
directions=""

pattern="([[:alnum:]]+)\s*=\s*\(([^,]+),\s*([^)]+)\)"


parse_input() {
    network=()
    currents=()
    directions=""
    while read -r line; do
        if [[ $line =~ $pattern ]]; then
            current="${BASH_REMATCH[1]}"
            left="${BASH_REMATCH[2]}"
            right="${BASH_REMATCH[3]}"
            network["$current"]="$left $right"
            if [[ $current == *A ]]; then
                currents+=("$current")
            fi
        elif [[ -n "$line" ]]; then
            directions="$line"
        fi
    done < "$1"
}

part1() {
    parse_input "$input"
    current="AAA"
    read -r left right <<< "${network["$current"]}"
    total_steps=0
    step=0
    while [[ $current != "ZZZ" ]]; do
        d="${directions:$step:1}"
        read -r left right <<< "${network["$current"]}"
        if [[ "$d" == "R" ]]; then
            current="$right"
        else
            current="$left"
        fi
        ((step++))
        ((step%=${#directions}))
        ((total_steps++))
    done
    echo "$total_steps"
}

gcd() {
    local a=$1
    local b=$2
    while [[ $b -ne 0 ]]; do
        local temp=$b
        b=$(( a % b ))
        a=$temp
    done
    echo "$a"
}

lcm() {
    local a=$1
    local b=$2
    local gcd_result
    gcd_result=$(gcd "$a" "$b")
    local lcm_result=$(( a * b / gcd_result ))
    echo $lcm_result
}

part2() {
    parse_input "$input"
    read -r left right <<< "${network["$current"]}"
    total_steps=1
    step=0
    lengths=()
    declare -a new_currents=()
    while [ ${#lengths[@]} -lt ${#currents[@]} ]; do
        new_currents=()
        d="${directions:$step:1}"
        for ((i=0; i<${#currents[@]}; i++)); do
            current="${currents[i]}"
            read -r left right <<< "${network["$current"]}"
            if [[ "$d" == "R" ]]; then
                new_currents+=("$right")
                if [[ $right == *Z ]]; then
                    lengths+=("$total_steps")
                fi
            else
                new_currents+=("$left")
                if [[ $left == *Z ]]; then
                    lengths+=("$total_steps")
                fi
            fi
        done
        ((step++))
        ((step%=${#directions}))
        ((total_steps++))
        currents=("${new_currents[@]}")
    done
    lcm_result=${lengths[0]}

    for (( i=1; i<${#lengths[@]}; i++ )); do
        lcm_result=$(lcm "$lcm_result" "${lengths[i]}")
    done
    echo "$lcm_result"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"