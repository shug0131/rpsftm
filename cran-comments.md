## Description of Changes

This is a minor version release with several enhancements

Defined a class "rand" for rand() output;added a print method that gives summary of rx values by arm; exported rand and print.rand, modified rpsftm to return the rand() object, and the print.rpsftm, and print.summary methods to print the rand() 

added an example to the plot.rpsftm documentation to illustrate how to modify the ggplot2 object. Standardised the underlying data.frame to have lower-case variable names. Modified the call to survfit() in rpsftm to use the same alpha argument (original set for CI around psi), but for the KM curves.

made the returned object inherit from survreg/coxph/survdiff as appopriate. Moved the returned object from the survival objects directly into the returned list rather than nested, with modified formula, terms, and call elements. Modified the print and summary methods. Hence standard methods are availble: anova, extractIAC, logLik, model.frame, model.matrix, predict, residuals, vcov.

added checks on the treat_modifier values: fail if they are all zero, and warn if not all strictly positive.

replaced uniroot with rootSolve::uniroot.all, to deal with multiple roots.

added in methods for residuals.rpsftm and survfit.rpsftm - essentially just calling the "inner" fitted model as needed. This allows cox.zph to be used. 

modified the vignette to improve clarity

change the internal use of  variable names arm and rx, to .arm and .rx and put in checks for these terms, and "time" "status" in the adjusting formula.


## Test environments
* travis checks
* appveyor
* win-builder
* windows 7
* ubuntu 

## R CMD check results
There were no ERRORs or WARNINGs. 


## Downstream dependencies
devtools::revdep_check()  issued No ERRORs or WARNINGs found :)
