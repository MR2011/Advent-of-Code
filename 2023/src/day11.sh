#!/bin/bash

input="../input/day11.txt"

declare -A grid
declare -A galaxies
declare -A empty_rows
declare -A empty_cols

create_grid() {
    row=0
    columns=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            grid["$row $col"]=${line:$col:1}
            if [[ ${line:$col:1} == "#" ]]; then
                galaxies["$row $col"]=true
            fi
            columns=${#line}
        done
        if [[ $line != *#* ]]; then
            empty_rows["$row"]=true
        fi
        ((row++))
    done < "$1"
    grid["rows"]="$row"
    grid["columns"]="$columns"
    for ((c=0; c<columns; c++)); do
        empty=true
        for ((r=0; r<row; r++)); do
            if [[ "${grid["$r $c"]}" == "#" ]]; then
                empty=false
                break
            fi
        done
        if [[ $empty == true ]]; then
            empty_cols["$c"]=true
        fi
    done
}

solve() {
    expansion="$1"
    sum=0
    for g1 in "${!galaxies[@]}"; do
        for g2 in "${!galaxies[@]}"; do
            read -r r1 c1 <<< "$g1"
            read -r r2 c2 <<< "$g2"
            (( r_min = r1 < r2 ? r1 : r2 ))
            (( r_max = r1 > r2 ? r1 : r2 ))
            er=0
            ec=0
            for ((r=r_min; r<=r_max; r++)); do
                if [[ "${empty_rows["$r"]}" == true ]]; then
                    ((er++))
                fi
            done
            (( c_min = c1 < c2 ? c1 : c2 ))
            (( c_max = c1 > c2 ? c1 : c2 ))
            for ((c=c_min; c<=c_max; c++)); do
                if [[ "${empty_cols["$c"]}" == true ]]; then
                    ((ec++))
                fi
            done
            (( sum += r_max-r_min+c_max-c_min+ec*expansion+er*expansion ))
        done
    done
    ((sum/=2))
    echo "$sum"
}

part1() {
    create_grid "$input"
    solve 1
}

part2() {
    create_grid "$input"
    solve 999999 
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"