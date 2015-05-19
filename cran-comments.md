## Test environments

* local Linux install (Ubuntu 14.04 LTS), R 3.2.0
* win-builder (devel and release)

## R CMD check results

There were no Errors or WARNINGs.

I observed the following NOTEs:

* (win-builder devel only):  No repository set, so cyclic dependency check skipped
* (win-builder devel only):  Maintainer: 'Homer White hwhite0@georgetowncollege.edu'
* (win-builder devel and release):  Possibly mis-spelled words in DESCRIPTION:
    * abd (18:36)
    * mosaicData (18:21)
    
`abd` and `mosaicData` are names of packages.

## Downstream dependencies

There are currently no downstram dependencies for this package.