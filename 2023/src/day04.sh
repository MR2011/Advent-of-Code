#!/bin/bash

input="../input/day04.txt"

solve() {
    total=0
    declare -A counts
    
    while read -r _ card_id numbers; do
        card_id="${card_id::-1}" # remove :
        if [[ ! -v counts["$card_id"] ]]; then
            counts["$card_id"]="1"
        fi
        IFS="|" read -r winning nums <<< "$numbers"
        points=0
        matches=0
        for n in $nums; do
            if [[ " $winning " == *" $n "* ]]; then
                points=$((points == 0 ? 1 : points*2))
                ((matches++))
            fi
        done
        # part2
        for ((c=card_id+1; c<=card_id+matches; c++)); do
            if [[ ! -v counts["$c"] ]]; then
                counts["$c"]="1"
            fi
            ((counts[$c]+=counts[$card_id]))
        done
        ((total+=points))
    done < $input

    sum=$(( $(echo "${counts[@]}" | tr ' ' '+') ))
    echo $(($1 == 1 ? total : sum))
}

echo "Part 1: $(solve 1)"
echo "Part 2: $(solve 2)"