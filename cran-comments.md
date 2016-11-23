## Description of Changes

This is a minor patch version 1.0.2 to use the '@keywords internal' syntax to the documentation of functions that are not exported; this will lessen the visibility of the functions in the documentation to avoid confusion to users.


## Test environments
* local Ubuntu  R 3.3.1
* travis checks
* win-builder

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE in win_build

Possibly mis-spelled words in DESCRIPTION:
  Tsiatis (7:67)
  confounders (7:343)
  
However this text is correct and unaltered from the pervious version accepted on CRAN.

## Downstream dependencies
New package so not yet any downstream dependencies