#!/bin/bash

input="../input/day03.txt"

create_grid() {
    declare -A grid
    row=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            grid["$row,$col"]=${line:$col:1}
        done
        ((row++))
    done < "$1"
    read -r rows columns <<< "$(echo "${!grid[@]}" | tr ' ' '\n' | awk 'BEGIN{FS=","}{print $1, $2}' | sort -n -k1,1 -k2,2| tail -n 1)"
    grid["rows"]="$rows"
    grid["columns"]="$columns"
    echo "${grid[@]@K}"
}

is_digit() { 
    [[ "$1" =~ [0-9] ]];
}

is_part_number() {
    local -n grid_ref=$1
    row=$2
    start=$3
    end=$4
    for ((r=row-1; r<=row+1; r++)); do
        if (( r < 0 )); then
            continue
        fi
        for ((c=start-1; c<=end+1; c++)); do
            if (( c < 0 || c > grid_ref["columns"])); then
                continue
            fi
            cell=${grid_ref[$r,$c]}
            if ! is_digit "$cell" && [[ "$cell" != "." ]]; then
                return 0
            fi
        done
    done
    return 1
}

set_gear_ratio_nums() {
    local -n grid_ref=$1
    local -n gears_ref=$2
    row=$3
    start=$4
    end=$5
    for ((r=i-1; r<=i+1; r++)); do
        if (( r < 0 )); then
            continue
        fi
        for ((c=start-1; c<=end+1; c++)); do
            if (( c < 0 || c > grid["columns"])); then
                continue
            fi
            if [[ ${grid_ref[$r,$c]} == "*" ]]; then
                if [ -z "${gears_ref["$r,$c"]}" ]; then
                    gears_ref["$r,$c"]="$number"
                else
                    gears_ref["$r,$c"]+=",$number"
                fi
            fi
        done
    done
}

calculate_gear_ratio_sum() {
    local -n gears_ref=$1
    sum=0
    for key in "${!gears_ref[@]}"; do
        IFS="," read -r num1 num2 <<< "${gears_ref[$key]}"
        if [[ -n "$num1" && -n "$num2" ]]; then
            ((sum+=(num1 * num2)))
        fi
    done
    echo "$sum"
}

part1() {
    declare -A grid="($(create_grid $input))"
    declare -A seen
    sum=0
    for ((i=0; i<=grid["rows"]; i++)); do
        start=-1 end=-1 number=""
        for ((j=0; j<=grid["columns"]; j++)); do
            cell=${grid[$i,$j]}
            if is_digit "$cell"; then
                number+="$cell"
                if (( start == -1 )); then
                    start=$j
                fi
                end=$j
            fi
            if ! is_digit "$cell" || (( end == grid["columns"] )) && [[ -n $number ]]; then
                if is_part_number grid $i $start $end && \
                    [[ ! ${seen["$i,$start,$end"]} ]]; then
                       ((sum+=number)) 
                        seen["$i,$start,$end"]=true
                fi
                start=-1 end=-1 number=""
            fi
        done
    done
    echo "$sum"
}

part2() {
    declare -A grid="($(create_grid $input))"
    declare -A gears
    sum=0
    for ((i=0; i<=grid["rows"]; i++)); do
        start=-1 end=-1 number=""
        for ((j=0; j<=grid["columns"]; j++)); do
            cell=${grid[$i,$j]}
            if is_digit "$cell"; then
                number+="$cell"
                if (( start == -1 )); then
                    start=$j
                fi
                end=$j
            fi
            if ! is_digit "$cell" || (( end == grid["columns"] )) && [[ -n $number ]]; then
                set_gear_ratio_nums grid gears $i $start $end
                start=-1 end=-1 number=""
            fi
        done
    done
    calculate_gear_ratio_sum gears
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"