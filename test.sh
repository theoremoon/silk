#!/bin/bash

PROGRAM=./silk

# expect input expected_output
function expect {
  a=$(mktemp)
  echo "$1" | timeout 5 $PROGRAM "$a"
  r=$(lli "$a")

  if [ "$r" != "$2" ]; then
    echo -e "\e[31mExpected $2 but got $r\e[m"
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

expect "100" "100"
expect "20" "20"
expect "-10" "-10"
expect "100 +   200" "300"
expect "100+-200" "-100"

failed "abcd"
