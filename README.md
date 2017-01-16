updater
=======

Simple binary updater program

* using for now [bsdiff](http://www.daemonology.net/bsdiff/)


MANIFEST
--------

Tar file need to have a MANIFEST file that contains how to apply various files
that it contains.

Format of MANIFEST file:

```
<source-digest> SP <destination-digest> SP <diff-digest> NL
...
<source-digest> SP <destination-digest> SP <diff-digest> NL
```

when patching a file, its digest will be computed and will repeatdly
be compared to the MANIFEST file until no `<source-digest>` match.

Once a `<source-digest>` matches the source we're patching, then the
bsdiff filename associated with the `diff-digest` of the same line will
be applied and check that the resulting file is equal to `destination-digest`.
