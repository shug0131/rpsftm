## Description of Changes

This is a patch release. The outputs from the print and summary methods now finish with a new line.

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
