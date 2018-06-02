#!/bin/bash

PROGRAM=./silk

# expect input expected_output
function expect {
  a=$(mktemp)
  echo -e "$1" | timeout 5 $PROGRAM "$a"
  r=$(lli "$a")

  if [ "$r" != "$2" ]; then
    echo -e "\e[31mExpected $2 but got $r (in case $1)\e[m"
    exit 1
  else
    echo -e "\e[32mpass $1\e[m"
  fi
}

function failed {
  a=$(mktemp)
  echo "$1" | timeout 5 $PROGRAM "$a"
  if [ "$?" != "0" ]; then
    echo -e "\e[32mfailed $1\e[m"
  else
    r=$(lli "$a")
    if [ "$?" != "0" ]; then
      echo -e "\e[32mfailed $1\e[m"
    else
      echo -e "\e[31mFailure expected (in case $1)\e[m"
      exit 1
    fi
  fi


}

expect "def main() {print(100)}" "100"
expect "def main() {hello=10 print(hello)}" "10"
expect "def f() { 100 } def main() {print(f())}" "100"
expect "def f(arg) { arg+100 } def main() {print(f(10))}" "110"
expect "def f(a, b) { a + b } def main() {print(f(100, 10))}" "110"
expect "def main() { a = if 1 { 1 } else { 2 } print(a) }" "1"
expect "def main() { a = if 0 { 1 } else { 2 } print(a) }" "2"

failed "abcd"
failed "def mainer { print(100) }"
