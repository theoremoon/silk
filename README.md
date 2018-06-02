# silk

Silk is a practice language based on LLVM which respect to [cotton](https://github.com/eliza0x/cotton/).

## try it

1. `git clone`
2. `opam install omake menhir llvm`
3. `omake`
4. `./silk`

## usage

If you run `./silk` without any parameters, the binary will read silk program from stdin until EOF. And will output LLVM IR to stdout.

Also `./silk` can run with oen parameter that filename to output LLVM bitcode.

## syntax

A simple factorial program from [examples](https://github.com/theoldmoon0602/silk/blob/master/examples/factorial.silk).

```
def fact(n) {
  if n {
    n * fact(n-1)
  }
  else {
    1
  }
}

def main() {
  print(fact(5))
}
```

this program will output `120`.

## features

- program starts with `main` function
- all variables/literals have `int32_t` type
- function returns last evaluated value
- if/else are expression

## future works

- [ ] make blocks as expr
- [ ] specify type
- [ ] boolean/float type
- [ ] create function scope

and so on

- [ ] type inference
- [ ] closure

## Author
theoldmoon0602

## LICENSE

This repository does not have any licenses (any problems?).