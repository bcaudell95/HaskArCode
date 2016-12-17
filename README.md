# HaskArCode
Haskell Arithmetic Coding library

To run a testing application, first run 'make'.  This will
build an executable.  The first time this command is run,
it will have to update and install packaages for Haskell
to use, so it may take a few moments.

The executable built is called arithmetic, so run it with
'./arithmetic'.  The application prompts you to enter a
string to encode and then decode using one of two probaility
models.

If the user prompt is 's {some string consisting of the
characters a,b,c,d}', then the simple probability model
will be used.  This probability model is constant over
the entire encoding/decoding, and assigns 'a' a probability
of .6, 'b' a probability .2, and 'c', 'd' probabilities .1.

The other model is run if the user enters 'c {string
consisting of a,b,c,d,e}'.  This is the complex model,
and is dependent upon the string encoded/decoded before the
current symbol.  In this model, the first character in
the string has uniform probability, and all characters
after it are as follows.  The liklihood of repeating the
most-recent character is .5, and the other four characters
have uniform probability .125.

This library is written to be extremely general.  It
allows for probability models as complex as desired, so
long as they only depend on the previous string elements.
It also allows for any type of data to be encoded or
decoded, so long as it is an orderable data type (one
with a constant ordering of its elements, such as
the integers or alphanumeric characters).  This creates
an extremely generalized coding library.
