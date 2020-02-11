# Steganographic BF
A steganographic programming language interepreted from the length of git commit messages.

## How it works
The length of your commit message (without spaces) modulo 8 maps straighforwardly to [BF](https://en.wikipedia.org/wiki/Brainfuck).
A quick translation table:

Commit message length mod 8| BF translation
:------------: | :-----:
0              | <
1              | >
2              | +
3              | -
4              | .
5              | ,
6              | [
7              | ]

## Get Started
The commit messages in this repository contain an example script. To build the interpreter, you need a Haskell compiler.
For example:
```bash
ghc -O2 steg.hs
```
builds the interpreter. Scripts are interpreted by the commit messages of a specified author from the root of a git project. For this repository:
```bash
steg hello
```
should now give an appropriate output.

## Do I have to type the commit messages manually?
There is a basic translation tool from BF which can be built as follows:
```bash
ghc bf2steg.hs
bf2steg $BF_FILE.bf $AUTHOR_NAME
```
which results in a shell script that can populate a repository with empty commits, but appropriate commit messages from a given BF source code. Running `source $AUTHOR_NAME.sh` then should yield the program.

A bundled script to translate from git commit messages to BF for a given author is also provided.
```bash
ghc steg2bf.hs
steg2bf $AUTHOR_NAME
```

With the above translation tools, it should be possible to do all sorts, like write steganographic BF via C using a [C to BF compiler](https://github.com/arthaud/c2bf), or make a steganographic version of [Conway's Game of Life](https://www.linusakesson.net/programming/brainfuck/index.php). A source for quality BF programs can be found [here](http://www.hevanet.com/cristofd/brainfuck/).
