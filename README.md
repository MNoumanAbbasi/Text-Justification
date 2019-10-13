# Text-Justification

Text justification function for a monospaced terminal output.

## Overview

Given text is justified by aligning the text to given width (default 15). The alignment is achieved by inserting blanks and hyphenating the words. For hypenating, we use a map `enHyp` that has all possible hypenations for a given word.  

The best possible alignment is returned based on computing and comparing costs for all possible alignments for a given text.  
In case a given text can not be justified, e.g. if given width is less than length of a word in line, the original unjustified text will be returned.  

The type `Token` is used to represent the Words, Blanks and Hypenated Words.  
`line` is simply a list of `Token`, or `[Token]`.

## Example

For given text:  
`text = "He who controls the past controls the future. He who controls the present controls the past."`  
After alignment to a width of 15, we get:

```
He who controls
the  past cont-
rols  the futu-
re. He  who co-
ntrols the pre-
sent   controls
the past.
```
