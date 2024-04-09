#!/bin/bash

input="../input/day12.txt"

count_combinations() {
    conditions="$1"
    rules="$2"

    if [[ -z "$rules" ]]; then
        if [[ "$conditions" == *"#"* ]]; then
            echo "0"
        else
            echo "1"
        fi
    elif [[ -z "$conditions" ]]; then
        if [[ -z "$rules" ]]; then
            echo "1"
        else
            echo "0"
        fi
    else
        result=0
        c="${conditions:0:1}"
        if [[ "$c" == "." || "$c" == "?" ]]; then
            combinations=$(count_combinations "${conditions:1}" "$rules")
            ((result+=combinations))
        fi

        if [[ "$c" == "#" || "$c" == "?" ]]; then
            IFS=","
            read -ra rules_arr <<< "$rules"
            r="${rules_arr[0]}"
            if [[ "$r" -le "${#conditions}" && "${conditions:0:$r}" != *"."* && ("$r" -eq "${#conditions}" || "${conditions:$r:1}" != "#")]]; then
                unset "rules_arr[0]"
                rules="${rules_arr[*]}"
                ((r++))
                combinations=$(count_combinations "${conditions:$r}" "$rules")
                ((result+=combinations))
            fi
        fi

        echo "$result"
    fi
}

part1() {
    sum=0
    while read -r line; do
        read -r conditions rules <<< "$line"
        c=$(count_combinations "$conditions" "$rules")
        ((sum+=c))
    done < "$input"
    echo "$sum"
}

part2() {
    sum=0
    while read -r line; do
        read -r conditions rules <<< "$line"
        cc="$conditions?$conditions?$conditions?$conditions?$conditions"
        rr="$rules,$rules,$rules,$rules,$rules"
        c=$(count_combinations "$cc" "$rr")
        ((sum+=c))
    done < "$input"
    echo "$sum"
}
echo "Part 1: $(part1)"
echo "Part 2: $(part2)"