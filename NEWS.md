# rpsftm 1.2.6

Version control readiness

# rpsftm 1.2.5

Following an update to the survival package, the print.summary for objects fitted with coxph(), now does not report the R-squared statistic. Also the names of the modified copies of print and summary methods have been changed to include "rpsftm." and a new class created internally, so as to be more explicit and avoid masking the methods from the survival package.


# rpsftm 1.2.4

Small modifications to get 100% coverage of the tests.

# rpsftm 1.2.3

Minor aesthetic corrections to ensure that the print() and summary() output finishes with a new line.

# rpsftm 1.2.2

modified the vignette to conditionally load the package "tableone", or use summary() as an alternative. Tableone package has been archived and so the CRAN checks are failing. 


# rpsftm 1.2.1

Ensuring forward compatibility with testthat package v2.0.0 .

# rpsftm 1.2.0

Defined a class "rand" for rand() output;added a print method that gives summary of rx values by arm; exported rand and print.rand, modified rpsftm to return the rand() object, and the print.rpsftm, and print.summary methods to print the rand() 

added an example to the plot.rpsftm documentation to illustrate how to modify the ggplot2 object. Standardised the underlying data.frame to have lower-case variable names. Modified the call to survfit() in rpsftm to use the same alpha argument (original set for CI around psi), but for the KM curves.

made the returned object inherit from survreg/coxph/survdiff as appopriate. Moved the returned object from the survival objects directly into the returned list rather than nested, with modified formula, terms, and call elements. Modified the print and summary methods. Hence standard methods are availble: anova, extractIAC, logLik, model.frame, model.matrix, predict, residuals, vcov.

added checks on the treat_modifier values: fail if they are all zero, and warn if not all strictly positive.

replaced uniroot with rootSolve::uniroot.all, to deal with multiple roots.

added in methods for residuals.rpsftm and survfit.rpsftm - essentially just calling the "inner" fitted model as needed. This allows cox.zph to be used. 

modified the vignette to improve clarity

change the internal use of  variable names arm and rx, to .arm and .rx and put in checks for these terms, and "time" "status" in the adjusting formula.

# rpsftm 1.1.0

removed rows for "arm" from the print and summary methods for coxph and survreg fits.

clarified the documentation for rpsftm function arguments slightly.

Created a tools directory and put a copy of the image file in it for the Readme file. Left the original in the root directory, for github, which is ignored in the .Rbuildignore file. Following the instructions below.

>It was recently pointed out to us that some README.html files (generated from the corresponding README.md ones) on the CRAN package web pages are incomplete, missing 'local' images not available from the web page and in most cases actually not even shipped with the package.  This clearly should be changed, so we will move to using '--self-contained' for the pandoc conversion to ensure that the README.html files are "complete".
>Of course, this implies that all 'local' images used in README.md are needed in the package sources.
>If the images are also used for vignettes or Rd files, you can put them in the 'vignettes' or 'man/figures' directories.  Otherwise, please put them in the top-level 'tools' directory, or a subdirectory of it.
>The CRAN incoming checks in r-devel were changed to perform the pandoc conversion checks with '--self-contained', and hence will warn about missing images.


Trying to recreate and resolve "bug" in (#2) for NA handling in the rand() function. test_errors.R revised.

added a link to https://codecov.io/ for continuous cloud testing of the coverage properties of the testing suite.  (#3)

added a link to https://ci.appveyor.com/ for continuous building in a windows environment (#3)

edited the .travis.yml to send alerts on commits with the results (#3)


# rpsftm 1.0.2

adding "@keywords internal" to the documentation of functions that are not exported; this will lessen the visibility of the functions in the documentation to avoid confusion to users.

# rpsftm 1.0.1

##Minor Changes

* adding details of all contributors to the DESCRIPTION page.


# rpsftm 1.0.0
This was the first release.
