# Extended technical article

This directory contains the complete technical article, including statistical
derivations, implementation details, worked examples, additional figures, and
appendices. It is retained for readers who want more detail than can fit in a
JOSS software paper.

The authoritative Journal of Open Source Software submission is
`../../paper/paper.md`. The PDF in this directory is an extended manuscript,
not the JOSS-formatted paper.

If this extended article is submitted to, reviewed by, or published in another
venue, that relationship must be disclosed to JOSS during submission. The two
documents should maintain distinct scopes: the JOSS paper describes the
software contribution, while this article may provide fuller statistical and
application detail.

Compile the extended manuscript from this directory with:

```sh
latexmk -pdf partial_association_explorer_full.tex
```
