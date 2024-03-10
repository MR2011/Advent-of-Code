#!/bin/bash

input="../input/day09.txt"

calc_diffs() {
    declare -a diffs=()
    local h="$*"
    N=$(echo "${h[@]}" | wc -w)
    for ((i=1; i<N; i++)); do
        ((j=i+1))
        current=$(echo "$h" | cut -d " " -f "$i")
        next=$(echo "$h" | cut -d " " -f "$j")
        ((diff=next-current))
        diffs+=("$diff")
    done
    echo "${diffs[@]}"
}

prediction_p1() {
    local history="$1"
    declare -a p
    local diffs
    if [[ $history =~ ^[0[:space:]]*$ ]]; then
        echo "${history[*]} 0"
    else
        diffs=$(calc_diffs "${history[@]}")
        last_before=$(echo "$history" | rev | cut -d ' ' -f 1 | rev)
        p=("$(prediction_p1 "${diffs[@]}")")
        last_after=$(echo "${p[@]}" | rev | cut -d ' ' -f 1 | rev)
        ((diff=last_before-last_after))
        ((new_last=last_after+last_before))

        echo "${history[*]} $new_last"
    fi
}

prediction_p2() {
    local history="$1"
    declare -a p
    local diffs
    if [[ $history =~ ^[0[:space:]]*$ ]]; then
        echo "${history[*]} 0"
    else
        diffs=$(calc_diffs "${history[@]}")
        first_before=$(echo "$history" | cut -d ' ' -f 1 )
        p=("$(prediction_p2 "${diffs[@]}")")
        first_after=$(echo "${p[@]}" | cut -d ' ' -f 1 )
        ((new_first=first_before-first_after))
        echo "$new_first ${history[*]}" 
    fi
}

part1() {
    sum=0
    while read -r history; do
        p="$(prediction_p1 "${history[@]}")"
        last=$(echo "$p" | rev | cut -d ' ' -f 1 | rev)
        ((sum+=last))
    done < "$input"
    echo "$sum"
}

part2() {
    while read -r history; do
        p="$(prediction_p2 "${history[@]}")"
        first=$(echo "$p" | cut -d ' ' -f 1 )
        ((sum+=first))
    done < "$input"
    echo "$sum"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"