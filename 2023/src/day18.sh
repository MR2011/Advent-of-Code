#!/bin/bash

input="../input/day18.txt"

solve() {
    local perimeter=0
    local shoelace=0
    local x=0
    local y=0
    local parse_color="$1"
    declare -a points

    while read -r line; do
        read -r dir steps color <<< "$line"
        if [[ "$parse_color" == true ]]; then
            steps=$(printf "%d\n" "0x${color:2:5}")
            dir="${color:7:1}"
        fi

        local dx=0
        local dy=0
        case "$dir" in
            R|0) dx=1;;
            L|2) dx=-1;;
            U|3) dy=-1;;
            D|1) dy=1;;
        esac

        ((x+=dx*steps))
        ((y+=dy*steps))
        ((perimeter+=steps))
        points+=("$x,$y")
    done < "$input"

    points2=("${points[@]:1}") 
    for i in "${!points2[@]}"; do
        IFS="," read -r x1 y1 <<< "${points["$i"]}"
        IFS="," read -r x2 y2 <<< "${points2["$i"]}"
        ((shoelace+=(x1*y2-x2*y1)))
    done

    ((result=(shoelace/2)+(perimeter/2) + 1))

    echo "$result"
}

part1() {
    solve false
}

part2() {
    solve true
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"