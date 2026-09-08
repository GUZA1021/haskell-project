# PROJECTV2

A simple **register machine interpreter written in Haskell**. It reads a program from a text file, executes the instructions, and returns the final value stored in the `rv` register.

## Features

* 16 registers: `r0`–`r15`
* Special registers: `rv` and `lnk`
* Arithmetic operations: `add`, `sub`, `mul`, `div`
* Constant operations: `movc`, `addc`, `subc`, `mulc`, `divc`
* Control flow: `jmp`, `jal`, `jif`, `jr`
* Programs loaded from `.txt` files
* Built-in test suite

## Instructions

| Instruction    | Description                           |
| -------------- | ------------------------------------- |
| `mov r0 r1`    | Copy `r0` into `r1`                   |
| `movc r0 10`   | Set `r0` to `10`                      |
| `add r0 r1 r2` | `r0 = r1 + r2`                        |
| `sub r0 r1 r2` | `r0 = r1 - r2`                        |
| `mul r0 r1 r2` | `r0 = r1 * r2`                        |
| `div r0 r1 r2` | `r0 = r1 / r2`                        |
| `addc r0 5`    | Add `5` to `r0`                       |
| `subc r0 5`    | Subtract `5` from `r0`                |
| `mulc r0 5`    | Multiply `r0` by `5`                  |
| `divc r0 5`    | Divide `r0` by `5`                    |
| `jmp 5`        | Jump to instruction `5`               |
| `jal 5`        | Jump and save return address in `lnk` |
| `jif r0 5`     | Jump if `r0` is non-zero              |
| `jr lnk`       | Jump to the address stored in `lnk`   |

## Example

A program file might contain:

```text
movc r2 10
movc r0 0
movc r1 1
add r0 r0 r1
jal 8
subc r2 1
jif r2 3
jmp 12
mov r0 r3
mov r1 r0
mov r3 r1
jr lnk
mov r0 rv
```

## Running

Compile the program with GHC:

```bash
ghc PROJECTV2.hs
```

Then run:

```bash
./PROJECTV2
```

Enter the name of the program file when prompted.

## Testing

The project includes tests for all implemented instructions.

Evaluate:

```haskell
exectestsuite
```

A result of `True` means all tests passed.

## Built With

* **Haskell**
* `Control.Monad.State`
* `Data.Int`
