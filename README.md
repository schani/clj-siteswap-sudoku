siteswap-sudoku 1.2
===================

This is a program for generating somewhat Sudoku-like puzzles
involving the juggling pattern notation Siteswap.  For an overview of
what the puzzles are about and a description of an earlier
implementation, visit [my blog](http://schani.wordpress.com/2006/04/09/a-siteswap-puzzle/).

You can download the source or the latest version of this program from
[GitHub](http://github.com/schani/clj-siteswap-sudoku).

Usage
-----

To use siteswap-sudoku you need a Java VM installed.  It's very likely
you have one on your system already.  If not, get it
[from Sun](http://java.sun.com/javase/downloads/index.jsp).

Invoke the program like this

    java -jar siteswap-sudoku.jar

and it will generate a 3x4 puzzle with 5 unknowns, which are the
default values.  Invoke it like this

    java -jar siteswap-sudoku.jar --help

and it will give you a summary of the command line options.  The most
important options are `--rows`, `--cols` and `--unknowns`.  For
example, to generate a 5x6 puzzle with 18 unknowns, do

    java -jar siteswap-sudoku.jar --rows 5 --cols 6 --unknowns 18

If you want to see some progress messages along the way, use
`--verbose`.

An important option is `--complex-rules`: In contrast to the original
puzzle rules, this program does not by default require that all
siteswaps be different.  If you do want all siteswaps to be different,
then use `--complex-rules`.  Note that if you do use that option, the
resulting puzzles might have some solutions where two or more
siteswaps are the same - it's your job to find the one solution where
there are no two identical siteswaps.

Note that the complex rule requirement that all siteswaps be different
does not apply to siteswaps with period 1.  It also does not apply to
any pair of siteswaps with different periods, i.e. 51 is considered
different from 5151.

If you want to generate a puzzle with a non-rectangular shape, use the
`--shape-file` option to specify a file that describes the shape.  A
space in that file stands for an empty square, every other character
for a square that is part of a siteswap.  If such a character is a
digit or a lowercase letter and it falls within the range of allowed
throws, then only that throw is allowed on that square.  As an
example, this is the same shape you would get with the options
`--rows 2 --cols 3`:

    ###
    ###

This is the same shape, but with the throw in the upper left fixed to
a 1:

    1##
    ###

See the `shapes` directory for a few more interesting examples.
