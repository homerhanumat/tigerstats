## Test environments

* local Linux install (Debian 8.3 Jesse), R 3.2.3
* local OS X install (El Capitan 10.11.3), R 3.2.3
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, no WARNINGs and no NOTEs.


## Downstream dependencies

There are currently no downstream dependencies for this package.

## Reason for Current Submission

Temporarily removing package `abd` from Depends list until it (or other upstream package) fixes this error:

```
* installing *source* package ‘abd’ ...
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** preparing package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘nlme’ 3.1-122 is already loaded, but >= 3.1.123 is required
Error : package ‘car’ could not be loaded
ERROR: lazy loading failed for package ‘abd’
* removing ‘/home/homer/git/abd.Rcheck/abd’
```