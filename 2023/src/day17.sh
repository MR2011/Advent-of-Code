#!/bin/bash

input="../input/day17.txt"

declare -A grid
declare -a pqueue
declare -a DIRECTIONS=("0,1" "1,0" "0,-1" "-1,0")

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

queue_push() {
    local item="$1"
    pqueue+=("$item")
    IFS=$'\n' pqueue=($(sort -t',' -k1 -n <<<"${pqueue[*]}"))
}

solve() {
    min_dist="$1"
    max_dist="$2"
    declare -A visited
    declare -A costs
    size="${grid["rows"]}"

    queue_push "0,0,0,-1"

    while [[ ${#pqueue[@]} -gt 0 ]]; do
        IFS="," read -r cost row col dir <<< "${pqueue[0]}"
        pqueue=("${pqueue[@]:1}")
        
        if (( row == (size-1) && col == (size-1) )); then
            echo "$cost"
            return
        fi
        
        if [[ "${visited["$row $col $dir"]}" == true ]]; then
            continue
        fi

        visited["$row $col $dir"]=true
        for (( direction=0; direction<4; direction++ )); do
            if (( direction == dir || (direction+2)%4 == dir )); then
                continue
            fi
            cost_inc=0
            for (( distance=1; distance<=max_dist; distance++ )); do
                IFS="," read -r drow dcol <<< "${DIRECTIONS["$direction"]}"
                ((r=row+drow*distance))
                ((c=col+dcol*distance))
                if (( r >= 0 && r < size && c >= 0 && c < size )); then
                    ((cost_inc+=grid["$r $c"]))
                    if (( distance < min_dist )); then
                        continue
                    fi
                    ((new_cost=cost+cost_inc))
                    if [[ -n "${costs["$r $c $direction"]}" && "${costs["$r $c $direction"]}" -lt "$new_cost" ]]; then
                        continue
                    fi
                    costs["$r $c $direction"]="$new_cost"
                    queue_push "$new_cost,$r,$c,$direction"
                fi
            done
        done
        
    done
}

part1() {
    create_grid "$input"
    solve "1" "3"
}

part2() {
    create_grid "$input"
    solve "4" "10"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"