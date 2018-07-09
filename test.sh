#!/bin/bash

COMPILE=./compile.sh

# shouldbe input shouldbeed_output
function shouldbe {
  elf=$(mktemp)
  echo -e "$1" | $COMPILE - "$elf"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    return 1
  fi

  r=$($elf)

  if [ "$(echo -e "$r"|md5sum)" != "$(echo -e "$2"|md5sum)" ]; then
    echo -e "\e[33mexpected:\n\e[m$2\n\e[33moutput:\e[m\n$r\e[m"
    return 1
  fi
}

function example {
  shouldbe "$(cat "examples/$1")" "$2" && echo -e "\e[32mpass $1\e[m" || echo -e "\e[33mfailed $1\e[m"
}


example "print1.silk" "1"
example "arith.silk" "11"
example "var.silk" "-20"
example "add2.silk" "3"
example "id.silk" "true\n1"
example "func_typespec.silk" ""
example "literals.silk" "10\ntrue\nfalse"
example "factorial.silk" "120"

