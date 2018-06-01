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
  if [ "$?" == "0" ]; then
    echo -e "\e[31mExpected fail\e[m"
    exit 1
  else
    echo -e "\e[32mfailed $1\e[m"
  fi
}

expect "print(100)" "100"
expect "print(20)" "20"
expect "print(-10)" "-10"
expect "print(100 +   200)" "300"
expect "print(100+-200)" "-100"
expect "print(100+200+300+400)" "1000"
expect "print(1+2*3+4)" "11"
expect "print(2*3/6)" "1"
expect "print(2*4/6)" "1"
expect "print(2*3/6)" "1"
expect "print(2*3/8)" "0"
expect "print(4/3-2*1+5)" "4"
expect "print(4/(3-2)*(1+5))" "24"
expect "100\nprint(200+300)" "500"
expect "hello=100\nprint(hello+300)" "400"

failed "abcd"
