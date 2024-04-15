#!/bin/bash

input="../input/day15.txt"

calc_hash() {
    s="$1"
    value=0
    for (( i=0; i<${#s}; i++ )); do
        c="${s:$i:1}"
        ascii=$(printf "%d\n" "'$c")
        ((value+=ascii))
        ((value*=17))
        ((value%=256))
    done
    echo "$value"
}

part1() {
    sum=0
    read -r line < "$input"
    IFS=, read -ra fields <<< "$line"
    for field in "${fields[@]}"; do
        h=$(calc_hash "$field")
        ((sum+=h))
    done
    echo "$sum"
}

part2() {
    declare -A boxes
    regex="^([A-Za-z]+)(=|-)([0-9]*)$"

    read -r line < "$input"
    IFS=, read -ra fields<<< "$line"
    for (( i=0; i<256; i++ )); do
        boxes["$i"]=""
    done

    for field in "${fields[@]}"; do
        if [[ $field =~ $regex ]]; then
            label="${BASH_REMATCH[1]}"
            op="${BASH_REMATCH[2]}"
            focal_length="${BASH_REMATCH[3]}"
            h=$(calc_hash "$label")
            s="${boxes["$h"]}"

            if [[ "$op" == "=" ]]; then
                if [[ $s == *"$label"* ]]; then
                    s=$(echo "$s" | sed "s/\($label \)[0-9]\+/\1$focal_length/")
                else
                    s="$s$label $focal_length;"
                fi
            elif [[ "$op" == "-" ]]; then
                s=$(echo "$s" | sed "s/$label [0-9]\+;//")
            fi

            boxes["$h"]="$s"
        fi
    done

    sum=0
    for (( i=0; i<256; i++ )); do
        ((index=i+1))
        s="${boxes["$i"]}"
        if [[ -n "$s" ]]; then
            IFS=";" read -ra fields <<< "$s"
            j=1
            for field in "${fields[@]}"; do
                read -r _ fl <<< "$field"
                ((sum+=(index*j*fl)))
                ((j++))
            done
        fi
    done
    echo "$sum"        
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"