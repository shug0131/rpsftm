## Description of Changes

This is a minor version release. The vignette is modified  to conditionally load the package "tableone", or use summary() as an alternative. Tableone package has been archived and so the CRAN checks are failing. 


## Test environments
* travis checks
* appveyor
* win-builder
* windows 7
* ubuntu 

## R CMD check results
There were no ERRORs or WARNINGs. 

1 Note: "tableone" package is in Suggests, but has been archived. This is handled using conditional loading in the vignette file as per section 1.1.3.1 in Writing R Extensions. 

## Downstream dependencies
devtools::revdep_check()  issued No ERRORs or WARNINGs found :)
