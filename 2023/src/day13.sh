#!/bin/bash

input="../input/day13.txt"

find_reflection_line() {
    local -n list="$1"
    local diff="$2"
    for ((n=1; n<${#list}; n++)); do
        d=0
        for row in "${list[@]}"; do
            left="${row:0:n}"
            right="${row:n}"
            len_left="${#left}"
            len_right="${#right}"
            if (( len_left < len_right )); then
                right="${right:0:len_left}"
            elif (( len_left > len_right )); then
                ((offset=len_left-len_right))
                left="${left:offset}"
            fi
            right_rev=$(echo $right | rev)
            l=$(echo "ibase=2;$left" | bc)
            r=$(echo "ibase=2;$right_rev" | bc)
            ((xor=l^r))
            set_bits=$(echo "obase=2; $xor" | bc | tr -cd '1' | wc -c)
            ((d+=set_bits))
        done
        if [[ "$d" == "$diff" ]]; then
            echo "$n"
            return
        fi
    done
}

solve() {
    diff="$1"
    rows=()
    columns=()
    lefts=0
    tops=0
    while read -r line; do
        if [[ -z "$line" ]]; then
            vertical=$(find_reflection_line rows "$diff")
            ((lefts+=vertical))
            horizontal=$(find_reflection_line columns "$diff")
            ((tops+=horizontal))
            rows=()
            columns=()
        else
            l=""
            for ((col=0; col<${#line}; col++)); do
                if [[ "${line:$col:1}" == "#" ]]; then
                    c="1"
                else
                    c="0"
                fi
                l="$l$c"
                columns["$col"]+="$c"
            done
            rows+=("$l")
        fi
    done < "$input"
    ((result=lefts+100*tops))
    echo "$result"
}

part1() {
    solve "0"
}

part2() {
    solve "1"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"