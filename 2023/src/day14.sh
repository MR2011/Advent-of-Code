#!/bin/bash

input="../input/day14.txt"

declare -A grid

calc_hash() {
    s=""
    for ((i=0; i<grid["rows"]; i++)); do
        for ((j=0; j<grid["columns"]; j++)); do
            s="$s${grid[$i $j]}"
        done
    done
    echo -n "$s" | md5sum | awk '{print $1}'
}

print_grid() {
    echo "\n"
    for ((i=0; i<grid["rows"]; i++)); do
        s=""
        for ((j=0; j<grid["columns"]; j++)); do
            s="$s${grid[$i $j]}"
        done
        echo "$s"
    done
}

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

tilt_north() {
    while true; do
        moved=false
        for ((i=1; i<grid["rows"]; i++)); do
            ((north=i-1))
            for ((j=0; j<grid["columns"]; j++)); do
                if [[ "${grid["$i $j"]}" == "O" && "${grid["$north $j"]}" == "." ]]; then
                    grid["$i $j"]="."
                    grid["$north $j"]="O"
                    moved=true
                fi
            done
        done
        if [[ "$moved" == false ]]; then
            break
        fi
    done
}

tilt_south() {
    while true; do
        moved=false
        for ((i=grid["rows"]-2; i>=0; i--)); do
            ((south=i+1))
            for ((j=0; j<grid["columns"]; j++)); do
                if [[ "${grid["$i $j"]}" == "O" && "${grid["$south $j"]}" == "." ]]; then
                    grid["$i $j"]="."
                    grid["$south $j"]="O"
                    moved=true
                fi
            done
        done
        if [[ "$moved" == false ]]; then
            break
        fi
    done
}

tilt_west() {
    while true; do
        moved=false
        for ((i=0; i<grid["rows"]; i++)); do
            for ((j=1; j<grid["columns"]; j++)); do
                ((west=j-1))
                if [[ "${grid["$i $j"]}" == "O" && "${grid["$i $west"]}" == "." ]]; then
                    grid["$i $j"]="."
                    grid["$i $west"]="O"
                    moved=true
                fi
            done
        done
        if [[ "$moved" == false ]]; then
            break
        fi
    done
}

tilt_east() {
    while true; do
        moved=false
        for ((i=0; i<grid["rows"]; i++)); do
            for ((j=grid["columns"]-2; j>=0; j--)); do
                ((east=j+1))
                if [[ "${grid["$i $j"]}" == "O" && "${grid["$i $east"]}" == "." ]]; then
                    grid["$i $j"]="."
                    grid["$i $east"]="O"
                    moved=true
                fi
            done
        done
        if [[ "$moved" == false ]]; then
            break
        fi
    done
}

tilt() {
    while true; do
        moved=false
        for ((i=1; i<=grid["rows"]; i++)); do
            ((north=i-1))
            for ((j=0; j<=grid["columns"]; j++)); do
                if [[ "${grid["$i $j"]}" == "O" && "${grid["$north $j"]}" == "." ]]; then
                    grid["$i $j"]="."
                    grid["$north $j"]="O"
                    moved=true
                fi
            done
        done
        if [[ "$moved" == false ]]; then
            break
        fi
    done
}

tilt_all() {
    declare -A states
    declare -A loads
    for ((n=0; n<1000000; n++)); do
        h=$(calc_hash)
        if [[ -v states["$h"] ]]; then
            index="${states["$h"]}"
            ((cycle_length=n-index))
            ((load_index=index+(1000000000-index)%cycle_length))
            echo "${loads["$load_index"]}"
            return
        fi
        states["$h"]="$n"
        loads["$n"]=$(calc_load)
        tilt_north
        tilt_west
        tilt_south
        tilt_east
    done
}

calc_load() {
    sum=0
    for ((i=0; i<=grid["rows"]; i++)); do
        for ((j=0; j<=grid["columns"]; j++)); do
            if [[ "${grid["$i $j"]}" == "O" ]]; then
                ((sum+=(grid["rows"]-i)))
            fi
        done
    done
    echo "$sum"
}

part1() {
    create_grid "$input"
    tilt_north
    calc_load
}

part2() {
    create_grid "$input"
    tilt_all
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"