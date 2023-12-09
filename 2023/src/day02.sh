#!/bin/bash

input="../input/day02.txt"

max_red=12
max_green=13
max_blue=14

is_possible() {
    while IFS=";" read -ra sets; do
        for set in "${sets[@]}"; do
            r=0 g=0 b=0
            IFS="," read -ra pairs <<< "$set"
            for pair in "${pairs[@]}"; do
                [[ $pair =~ ([0-9]+)\ (red|blue|green) ]]
                count=${BASH_REMATCH[1]}
                color=${BASH_REMATCH[2]}
                case "$color" in
                    red) ((r += count)) ;;
                    green) ((g += count)) ;;
                    blue) ((b += count)) ;;
                esac
            done
            (( r > max_red || g > max_green || b > max_blue )) && return 1
        done
    done < "$1"
    return 0
}

is_possible_fewest() {
    while IFS=";" read -ra sets; do
        r=0 g=0 b=0
        for set in "${sets[@]}"; do
            IFS="," read -ra pairs <<< "$set"
            for pair in "${pairs[@]}"; do
                [[ $pair =~ ([0-9]+)\ (red|blue|green) ]]
                count=${BASH_REMATCH[1]}
                color=${BASH_REMATCH[2]}
                case "$color" in
                    red) (( count > r )) && r=$count ;;
                    green) (( count > g )) && g=$count ;;
                    blue) (( count > b )) && b=$count ;;
                esac
            done
        done
    done < "$1"
    echo $(( r * b * g ))
}

part1() {
    sum=0
    while read -r _ game_id sets; do
        game_id="${game_id::-1}" # remove :
        is_possible <(echo "$sets") && (( sum += game_id ))
    done < $input
    echo "$sum"
}

part2() {
    sum=0
    while read -r _ game_id sets; do
        game_id="${game_id::-1}" # remove :
        (( sum += $(is_possible_fewest <(echo "$sets"))))
    done < $input
    echo "$sum"
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"