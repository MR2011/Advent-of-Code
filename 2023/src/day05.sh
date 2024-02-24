#!/bin/bash

input="../input/day05.txt"

declare -a ranges seeds_ranges seeds soil fertilizer water light temperature humidity location
declare -A maps
declare -a names=("soil" "fertilizer" "water" "light" "temperature" "humidity" "location")

maps["seeds"]=seeds
maps["seed-to-soil"]=soil
maps["soil-to-fertilizer"]=fertilizer
maps["fertilizer-to-water"]=water
maps["water-to-light"]=light
maps["light-to-temperature"]=temperature
maps["temperature-to-humidity"]=humidity
maps["humidity-to-location"]=location

parse_input() {
    while read -r line; do
        if [[ "${line}" =~ [a-zA-Z] ]]; then
            declare -n map="${maps["$line"]}"
        elif [[ -n $line ]]; then
            map+=("$line")
        fi
    done < <(
       sed -e 's/map://g' \
        -e 's/:/\n/g' < $input
    )
}

lookup() {
    value="$1"
    target="$value"
    shift # pop first argument
    map="$@"
    
    IFS=' ' read -ra triplets <<< "$map"
    for ((i=0; i<${#triplets[@]}; i+=3)); do
        dst="${triplets[i]}"
        src="${triplets[i+1]}"
        range="${triplets[i+2]}"
        if (( value >= src && value < (src + range) )); then
            ((target=dst + value - src))
        fi
    done
    echo "$target"
}

get_location() {
  declare -a locations
  for seed in ${seeds[@]}; do
    res1=$(lookup $seed ${soil[@]})
    res2=$(lookup $res1 ${fertilizer[@]})
    res3=$(lookup $res2 ${water[@]})
    res4=$(lookup $res3 ${light[@]})
    res5=$(lookup $res4 ${temperature[@]})
    res6=$(lookup $res5 ${humidity[@]})
    res7=$(lookup $res6 ${location[@]})
    locations+=("$res7")
  done
  min=$(printf "%s\n" "${locations[@]}" | sort -n | head -n 1)
  echo "$min"
}

minimum() {
  read -r min _ <<< "$1"
  for tuple in "$@"; do
    read -r a _ <<< "$tuple"
    (( a < min )) && min=$a
  done
  echo "$min"
}

create_seeds_ranges() {
  declare -a seeds_array=($seeds)
  for ((i=0; i<${#seeds_array[@]}; i+=2)); do
    seed=${seeds_array[i]}
    offset=${seeds_array[i+1]}
    (( end = seed + offset ))
    seeds_ranges+=("$seed $end")
  done
}

init_ranges() {
    ranges=()
    local -n map="$1"
    for m in "${map[@]}"; do
      ranges+=("$m")
    done
}

get_location_ranges() {
    create_seeds_ranges

    for name in "${names[@]}"; do
        init_ranges "$name"

        declare -a new=()
        while (( ${#seeds_ranges[@]} > 0 )); do
            # pop item
            read -r s e <<<"${seeds_ranges[-1]}"
            unset 'seeds_ranges[-1]'
                  
            overlap=false
            for ((i=0; i<${#ranges[@]}; i++)); do
                read -r dst src range <<< "${ranges[i]}"

                os=$((s > src ? s : src))
                oe=$((e < (src + range) ? e : (src + range)))

                if ((os < oe)); then
                    new+=("$((os - src + dst)) $((oe - src + dst))")
                    if (( os > s )); then
                        seeds_ranges+=("$s $os")
                    fi
                    if (( e > oe )); then
                        seeds_ranges+=("$oe $e")
                    fi
                    overlap=true && break
                fi
                done
                if [[ ! $overlap ]]; then
                    new+=("$s $e")
                fi
            done
        seeds_ranges=("${new[@]}")
    done
    minimum "${seeds_ranges[@]}"
}

part1() {
    parse_input
    get_location
}

part2() {
    parse_input
    get_location_ranges
}

echo "Part 1: $(part1)"
echo "Part 2: $(part2)"