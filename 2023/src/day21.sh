#!/bin/bash
input="../input/day21.txt"

declare -A grid
declare -a DIRECTIONS=("0,1" "1,0" "0,-1" "-1,0")
start=""

create_grid() {
    row=0
    columns=0
    while read -r line; do
        for ((col=0; col<${#line}; col++)); do
            if [[ "${line:$col:1}" == "S" ]]; then
                start="$row;$col"
                grid["$row;$col"]="."
            else
                grid["$row;$col"]=${line:$col:1}
                columns=${#line}
            fi
        done
        ((row++))
    done < "$input"
    grid["rows"]="$row"
    grid["columns"]="$columns"
}

print_grid() {
    echo ""
    for ((i=0; i<grid["rows"]; i++)); do
        s=""
        for ((j=0; j<grid["columns"]; j++)); do
            s="$s${grid[$i;$j]}"
        done
        echo "$s"
    done
}

solve() {
    local max_steps="$1"
    declare -A visited
    declare -a queue
    queue+=("$start;0")
    local size="${grid["rows"]}"
    local count=0

    while [[ ${#queue[@]} -gt 0 ]]; do
        current="${queue[0]}"
        queue=("${queue[@]:1}")
        IFS=";" read -r row col steps <<< "$current"

        if [[ "${visited["$row;$col"]}" == "true" ]]; then
            continue
        fi
        if [[ "$steps" -gt "$max_steps" || "${grid["$row;$col"]}" != "." ]]; then
            continue
        fi

        visited["$row;$col"]="true"

        if (( steps % 2 == 0)); then
            ((count++))
            grid["$row;$col"]=O
        fi

        ((steps++))
        for (( d=0; d<4; d++ )); do
            IFS="," read dr dc <<< "${DIRECTIONS["$d"]}"
            ((r=row+dr))
            ((c=col+dc))
            if [[ "${grid[$r;$c]}" == "."  && "${visited["$r;$c"]}" != "true" ]]; then
                queue+=("$r;$c;$steps")
            fi
        done
    done

    echo "$count"
}

part1() {
    create_grid "$input"
    solve "64"
}

part2() {
    echo "2"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"