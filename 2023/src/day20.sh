#!/bin/bash

input="../input/day20.txt"

declare -A flipflops
declare -A conjunctions
declare -a broadcaster
declare -A states

gcd() {
  local a=$1
  local b=$2
  while [ "$b" -ne 0 ]; do
    local temp=$b
    b=$((a % b))
    a=$temp
  done
  echo $a
}

lcm() {
  local a=$1
  local b=$2
  local gcd_value=$(gcd "$a" "$b")
  echo $(( (a * b) / gcd_value ))
}

lcm_array() {
  local numbers=("$@")
  local lcm_result="${numbers[0]}"
  for i in "${!numbers[@]}"; do
    if [ "$i" -gt 0 ]; then
      lcm_result=$(lcm "$lcm_result" "${numbers[$i]}")
    fi
  done
  echo $lcm_result
}

parse_input() {
    while read -r line; do
        line="${line//[-,]/}"
        IFS='>' read -r source destination <<< "$line"
        source="${source::-1}" 
        destination="${destination/,/}"
        destination="${destination:1}"
        if [[ $source == "%"* ]]; then
            source="${source:1}" 
            flipflops["$source"]="$destination"
        elif [[ $source == "&"* ]]; then
            source="${source:1}" 
            conjunctions["$source"]="$destination"
        elif [[ $source == "broadcaster" ]]; then
            source="${source:1}" 
            broadcaster+=("$destination")
        fi
    done < "$1"
}

init() {
    for f in "${!flipflops[@]}"; do
        states["$f"]="0"
        for dst in ${flipflops["$f"]}; do
            if [[ -v conjunctions["$dst"] ]]; then
                states["$dst;$f"]="0"
            fi
        done
    done
}

part1() {
    parse_input "$input"
    init
    declare -a queue
    local lows=0
    local highs=0

    for ((i=0; i<1000; i++)); do
        queue=("broadcaster;0;btn")
        while [[ "${#queue[@]}" -gt 0 ]]; do
            IFS=";" read -r dst pulse src <<< "${queue[0]}"
            queue=("${queue[@]:1}")

            (( pulse == 0 ? lows++ : highs++ ))

            if [[ "$dst" == "broadcaster" ]]; then
                for b in ${broadcaster[@]}; do
                    queue+=("$b;$pulse;$dst")
                done
            elif [[ -v flipflops["$dst"] ]]; then
                if [[ "$pulse" == "0" ]]; then
                    (( states["$dst"] = (states["$dst"] + 1) % 2 ))
                    for f in ${flipflops["$dst"]}; do
                        queue+=("$f;${states["$dst"]};$dst")
                    done
                fi
            elif [[ -v conjunctions["$dst"] ]]; then
                    states["$dst;$src"]="$pulse"
                    local new_pulse="0"
                    for key in ${!states[@]}; do
                        if [[ "$key" == "$dst;"* && "${states["$key"]}" == "0" ]]; then
                            new_pulse="1"
                            break
                        fi
                    done
                    for c in ${conjunctions["$dst"]}; do
                        queue+=("$c;$new_pulse;$dst")
                    done
            fi
        done
    done
    echo $((lows*highs))
}

part2() {
    parse_input "$input"
    init
    declare -a queue
    local lows=0
    local highs=0
    declare -a offsets

    for ((i=0; i<1000000000; i++)); do
        queue=("broadcaster;0;btn")
        while [[ "${#queue[@]}" -gt 0 ]]; do
            if [[ "${#offsets[@]}" -eq 4 ]]; then
                echo $(lcm_array "${offsets[@]}")
                return
            fi
            IFS=";" read -r dst pulse src <<< "${queue[0]}"
            queue=("${queue[@]:1}")

            (( pulse == 0 ? lows++ : highs++ ))

            if [[ ("$src" == "qz" || "$src" == "cq" || "$src" == "tt" || "$src" == "jx") && "$pulse" == "1" ]]; then
                ((j=i+1))
                offsets+=("$j")
            fi

            if [[ "$dst" == "broadcaster" ]]; then
                for b in ${broadcaster[@]}; do
                    queue+=("$b;$pulse;$dst")
                done
            elif [[ -v flipflops["$dst"] ]]; then
                if [[ "$pulse" == "0" ]]; then
                    (( states["$dst"] = (states["$dst"] + 1) % 2 ))
                    for f in ${flipflops["$dst"]}; do
                        queue+=("$f;${states["$dst"]};$dst")
                    done
                fi
            elif [[ -v conjunctions["$dst"] ]]; then
                    states["$dst;$src"]="$pulse"
                    local new_pulse="0"
                    for key in ${!states[@]}; do
                        if [[ "$key" == "$dst;"* && "${states["$key"]}" == "0" ]]; then
                            new_pulse="1"
                            break
                        fi
                    done
                    for c in ${conjunctions["$dst"]}; do
                        queue+=("$c;$new_pulse;$dst")
                    done
            fi
        done
    done
    echo $((lows*highs))
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"