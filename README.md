# ICFP 2014 Contest

The task was to write Pac-M^H^H^H^H^HLambda-Man and Ghost AIs for two different machines.

The Lambda-Man AI was run on a SECD machine, for which I have written a minimal Scheme compiler.

The Ghost AI was run on a persistent state machine with 256 byte memory, for which I have written a macro assembler.
(There were quite strong limitations on this one: only constant address jumps, max. 1024 execution cycle.)

I enjoyed writing these compilers very much, more than writing the actual AI programs, so they are not too complicated.
ICFP was fun even as a one-man team, but next year I'll try to join a team...

## Usage

1. Load the ASDF package icfp2014.asd
2. Load the file lambdaman-ai.lisp to generate /tmp/lambdaman.gcc
3. Load the file ghost-ai.lisp to generate /tmp/ghost0.ghc
   (and /tmp/ghost1.ghx, but that is not part of the submission)

I was using SBCL, but there shouldn't be any problem with any other CL implementations.

# Lambda-Man

## AI design

This is a greedy AI, but also wary of the ghosts.
Every possible direction candidate (walls are not included) is scored by the TRY-POS function.
This finds the nearest non-empty cell, and gives a score based on that, but also looks for
lurking ghosts, negative scores ("danger, ghosts!") have priority.

Scores are in inverse proportion with the distance of a cell,
so a pill right next to lambdaman worth more than a power-pill quite far away.

The maximum distance to look for is now 6 cells away.

Also, the program maintains a list of the last 51 visited positions,
and in case of a draw (e.g. there are only empty cells within 6 moves)
it chooses the one that was visited last (or never in the last 51 moves).

## Implementation notes

The file gcc.lisp contains the compiler.

There is...
- no defmacro
- no tail-call optimization

There are, however, some useful special forms like
- let*
- lambda
- when

The AI source file (lambdaman-ai.lisp) is essentially one call to GCC,
and here you can find a lot of utilities:
- mapcar
- reduce
- remove-if-not
- etc.

# Ghost

## AI design

All directions are scored. Obviously, walls and the reverse direction get a score of 0.
Pills get 70, power pills 80 points - any other cell get 60.

This ghost tracks the 50 last visited positions (though it actually only uses 15 of them,
due to the execution cycle limit). Each visited position is awarded the time to the last
visit, so a cell visited 10 moves ago gets 10 points. Cells that never have been visited
(or have been forgotten) get 50 points.

Then every direction that goes in Lambda-Man's direction, gets 50 points,
and every direction that goes the other way, gets -50 points.
(In fright-mode, the sign of points is reversed.)
It is a simple principle, but works quite well in practice.

Finally, to cover more area, ghosts try to avoid each other, though with low priority.
So for a direction where there are other ghosts, it gets -5 points.

Implementation notes
--------------------

The file ghc.lisp contains the macro assembler.

The idea was to use Lisp functions as instructions,
so I can use macros (and macro-defining macros) to generate constructs like a FOR loop.

Instructions are actually macros, so their arguments are written just as in assembly,
but later processed in two passes to generate line numbers instead of labels,
or replace constants with their values.

On the top level, the GHC macro takes the program, and prepends "GHC-" on every function
name, so e.g. MOV becomes GHC-MOV, etc. Then it expands the macros and does it again,
until everything is expanded and GHC-d.
When the program body is generated, it is run twice, with *PASS* = 1 and *PASS* = 2.
Each instruction is just increasing the instruction counter in pass 1, and the
real code generation occurs in pass 2. The special GHC-LABEL function stores the
label in a hash map in pass 1, and does nothing in pass 2.

There are a few constructs to help write more readable code,
notably some IF and WHEN functions, and a FOR loop.
