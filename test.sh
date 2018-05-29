#!/bin/bash

PROGRAM=./silk

function expect {
  a=$(mktemp)
  $PROGRAM "$a"
  r=$(lli "$a")

  if [ "$r" != "$1" ]; then
    echo -e "\e[31mExpected $1 but got $r\e[m"
    exit 1
  else
    echo -e "\e[32mpass $1\e[m"
  fi
}

expect 100
