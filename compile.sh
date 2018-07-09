PROGRAM=./silk
LIBSRC=./lib.c

function compile {
  lib=$(mktemp --suffix .ll)
  clang -c -S -emit-llvm "$LIBSRC" -o "$lib"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    exit 1
  fi

  ll=$(mktemp --suffix .ll)
  cat "$1" | timeout 5 "$PROGRAM" "$ll"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    exit 1
  fi

  bc=$(mktemp --suffix .bc)
  llvm-link "$ll" "$lib" -S -o "$bc"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    exit 1
  fi

  s=$(mktemp --suffix .s)
  llc "$bc" -o "$s"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    exit 1
  fi

  clang -no-pie "$s" -o "$2"
  if [ "$?" != "0" ]; then
    echo -e "\e[33m compile failed $1\e[m"
    exit 1
  fi
}
function run {
  o=$(mktemp)
  compile $1 $o
  "$o"
}

case $1 in
  "run") run $2;;
  *) compile $@
esac

