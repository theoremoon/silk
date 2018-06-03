#!/bin/bash

COMPILE=./compile.sh

# expect input expected_output
function expect {
  elf=$(mktemp)
  echo -e "$1" | $COMPILE - "$elf"
  if [ "$?" != "0" ]; then
    echo -e "\e[31m compile failed $1\e[m"
    exit 1
  fi

  r=$($elf)
  if [ "$r" != "$2" ]; then
    echo -e "\e[31mExpected $2 but got $r (in case $1)\e[m"
    exit 1
  else
    echo -e "\e[32mpass $1\e[m"
  fi
}

function example {
  expect "$(cat "examples/$1")" "$2"
}

expect "def main() {print(100)}" "100"
expect "def main() print(100)" "100"
expect "def main() {hello=10 print(hello)}" "10"
expect "def f() { 100 } def main() {print(f())}" "100"
expect "def f(arg) arg+100 def main() {print(f(10))}" "110"
expect "def f(a, b) { a + b } def main() {print(f(100, 10))}" "110"
expect "def main() { a = if 1 { 1 } else { 2 } print(a) }" "1"
expect "def main() { a = if 0 { 1 } else { 2 } print(a) }" "2"


example "factorial.silk" "120"

