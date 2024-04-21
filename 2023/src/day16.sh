#!/bin/bash

input="../input/day16.txt"

declare -A grid

create_grid() {
    row=0
    columns=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            grid["$row $col"]=${line:$col:1}
            columns=${#line}
        done
        ((row++))
    done < "$1"
    grid["rows"]="$row"
    grid["columns"]="$columns"
}

energized_tiles() {
    declare -A visited
    declare -a queue
    queue+=("$1")
    size="${grid["rows"]}"

    while [[ ${#queue[@]} -gt 0 ]]; do
        current="${queue[0]}"
        queue=("${queue[@]:1}")
        read -r row col d <<< "$current"
        
        case "$d" in
            R) ((col++)) ;;
            L) ((col--)) ;;
            U) ((row--)) ;;
            D) ((row++)) ;;
        esac
        
        if [[ "$row" -lt 0 || "$col" -lt 0 || "$row" -ge "$size" || "$col" -ge "$size" ]]; then
            continue
        fi
        
        if [[ "${visited["$row $col $d"]}" == true ]]; then
            continue
        fi

        visited["$row $col $d"]=true
        tile="${grid["$row $col"]}"
        
        case "$tile$d" in
            "-L"|"-R"|"|U"|"|D"|"."*)
                queue+=("$row $col $d")
                ;;
            "|L"|"|R")
                queue+=("$row $col U")
                queue+=("$row $col D")
                ;;
            "-U"|"-D")
                queue+=("$row $col L")
                queue+=("$row $col R")
                ;;
            "/L"|"\R")
                queue+=("$row $col D")
                ;;
            "/R"|"\L")
                queue+=("$row $col U")
                ;;
            "/U"|"\D")
                queue+=("$row $col R")
                ;;
            "/D"|"\U")
                queue+=("$row $col L")
                ;;
        esac
    done

    declare -A filtered
    for key in "${!visited[@]}"; do
        read -r row col d <<< "$key"
        filtered["$row $col"]=true
    done
    echo "${#filtered[@]}"
}

part1() {
    create_grid "$input"
    energized_tiles "0 -1 R"
}

part2() {
    create_grid "$input"
    declare -a energized
    size="${grid["rows"]}"

    for ((i=0; i<size; i++)); do
        energized+=($(energized_tiles "-1 $i D"))
        energized+=($(energized_tiles "$size $i U"))
        energized+=($(energized_tiles "$i -1 R"))
        energized+=($(energized_tiles "$i $size L"))
    done
    
    max=${energized[0]}
    for e in "${energized[@]}" ; do
        ((e > max)) && max=$e
    done
    echo $max
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"