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
def fact(n:I32):I32 {
  if n == 0 {
    1
  }
  else {
    n * fact(n-1)
  }
}

def main() {
  print(fact(5))
}
```

this program will output `120`.

## features

- program starts with `main` function
- integer literals have `int32_t` type
- function returns last evaluated value
- if/else are expression
- type inference is available
- fully typed recursive function is available
- mutual recursive function is not available even fully typed

## future works

- [x] make blocks as expr
- [x] specify type
- [ ] boolean/float type
- [x] create function scope

and so on

- [x] type inference
- [ ] closure

## Author
theoldmoon0602

## LICENSE

This repository does not have any licenses (any problems?).
