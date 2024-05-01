#!/bin/bash

input="../input/day19.txt"

declare -A workflows

parse_workflow() {
    IFS={ read -r key value <<< "$1"
    workflows["$key"]="${value%\}}"
    
}

parse_rating() {
    local sum=0
    cleaned=$(echo "$1" | tr -d '{x=mas}')
    IFS=, read -r x m a s <<< "$cleaned"
    ((sum=x+m+a+s))
    current="in"
    wf="${workflows["in"]}"
    IFS=, read l r <<< "$right"
    while [[ -n "$wf" ]]; do
        if [[ "$current" == *":"* ]]; then
            wf="$current"
        else
            wf="${workflows["$current"]}"
        fi
        IFS=: read -r condition dst <<< "$wf"
        IFS=, read -r dstt dstf <<< "$dst"
        if [[ $((condition)) -eq 1 ]]; then
            if [[ "$dstt" == "R" ]]; then
                echo "0"
                return
            fi
            if [[ "$dstt" == "A" ]]; then
                echo "$sum"
                return
            fi
            current="$dstt"
        else
            if [[ "$dstf" == "R" ]]; then
                echo "0"
                return
            fi
            if [[ "$dstf" == "A" ]]; then
                echo "$sum"
                return
            fi
            current="$dstf"
        fi
    done
}

part1() {
    local mode="workflow"
    local sum=0
    while read -r line; do
        if [[ "$line" == "" ]]; then
            mode="rating"
            continue
        fi
        if [[ "$mode" == "workflow" ]]; then
            parse_workflow "$line"
        elif [[ "$mode" == "rating" ]]; then
            res=$(parse_rating "$line")
            ((sum+=res))
        fi
    done < "$input"
    echo "$sum"
}

part2() {
    echo "2"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"