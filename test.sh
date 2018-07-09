#!/bin/bash

COMPILE=./compile.sh

# shouldbe input shouldbeed_output
function shouldbe {
  elf=$(mktemp)
  echo -e "$1" | $COMPILE - "$elf"
  if [ "$?" != "0" ]; then
    echo -e "\e[31m compile failed $1\e[m"
    return 1
  fi

  r=$($elf)
  if [ "$r" != "$2" ]; then
    echo -e "\e[31mExpected $2 but got $r (in case $1)\e[m"
    return 1
  fi
}

function example {
  shouldbe "$(cat "examples/$1")" "$2" && echo -e "\e[32mpass $1\e[m" || echo -e "\e[31mfailed $1\e[m"
}


example "print1.silk" "1"
example "arith.silk" "11"
example "var.silk" "-20"
example "add2.silk" "3"
example "id.silk" ""
example "func_typespec.silk" ""
example "factorial.silk" "120"

