#!/bin/bash

input="../input/day10.txt"

declare -A grid
declare -A loop
start="3 4"

dir_to_offset() {
    local dir="$1"
    case "$1" in
        "N") echo "-1 0";;
        "W") echo "0 -1";;
        "S") echo "1 0";;
        "E") echo "0 1";;
    esac
}

next_dir() {
    local pipe="$1"
    local dir="$2"
    case "$pipe $dir" in
        "| N") echo "N";;
        "| S") echo "S";;
        "- W") echo "W";;
        "- E") echo "E";;
        "L S") echo "E";;
        "L W") echo "N";;
        "J S") echo "W";;
        "J E") echo "N";;
        "7 N") echo "W";;
        "7 E") echo "S";;
        "F N") echo "E";;
        "F W") echo "S";;
    esac
}

create_grid() {
    row=0
    columns=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            grid["$row $col"]=${line:$col:1}
            if [[ ${line:$col:1} == "S" ]]; then
                start="$row $col"
            fi
            columns=${#line}
        done
        ((row++))
    done < "$1"
    grid["rows"]="$row"
    grid["columns"]="$columns"
}

traverse() {
    local steps=0
    local current="$start"
    local d="S"

    # lucky guess
    grid["$start"]="|"
    
    while true; do
        p="${grid[$current]}"
        d=$(next_dir "$p" "$d") 
        loop["$current"]=true
        read -r cx cy <<< "$current"
        read -r dx dy <<< "$(dir_to_offset "$d")"
        (( nx = cx + dx ))
        (( ny = cy + dy ))
        current="$nx $ny"
        ((steps++))
        if [[ "$current" == "$start" ]]; then
            break
        fi
    done
    ((result=steps/2))
    echo "$result"
}


part1() {
    create_grid $input
    traverse
}

part2() {
    create_grid $input
    traverse > /dev/null
    enclosed=0
    for ((i=0; i<=grid["rows"]; i++)); do
        for ((j=0; j<=grid["columns"]; j++)); do
            if [[ "${loop["$i $j"]}" == true ]]; then
                continue
            fi
            count=0
            for (( k=j-1; k>=0; k--)); do
                if [[ "${loop["$i $k"]}" != true ]]; then
                    continue
                fi
                cell=${grid["$i $k"]}
                if [[ "$cell" == "|" || "$cell" == "J" || "$cell" == "L" || "$cell" == "S" ]]; then
                    ((count++))
                fi
            done
            if (( (count % 2) != 0 )); then
                ((enclosed++))
            fi
        done
    done
    echo "$enclosed"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"