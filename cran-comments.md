## Description of Changes

Following an update to the survival package, the print.summary for objects fitted with coxph(), now does not report the R-squared statistic. Also the names of the modified copies of print and summary methods have been changed to include "rpsftm." and a new class created internally, so as to be more explicit and avoid masking the methods from the survival package.

## Test environments
* Windows 7 PC
* travis
* appveyor
* win-builder


## R CMD check results
There were no ERRORs or WARNINGs or Notes. 



