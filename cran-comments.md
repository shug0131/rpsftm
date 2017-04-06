## Description of Changes

This is a minor version release with several enhancements


removed rows for "arm" from the print and summary methods for coxph and survreg fits.

clarified the documentation for rpsftm function arguments slightly.

Created a tools directory and put a copy of the image file in it for the Readme file. Left the original in the root directory, for github, which is ignored in the .Rbuildignore file. Following the instructions below.

>It was recently pointed out to us that some README.html files (generated from the corresponding README.md ones) on the CRAN package web pages are incomplete, missing 'local' images not available from the web page and in most cases actually not even shipped with the package.  This clearly should be changed, so we will move to using '--self-contained' for the pandoc conversion to ensure that the README.html files are "complete".
>Of course, this implies that all 'local' images used in README.md are needed in the package sources.
>If the images are also used for vignettes or Rd files, you can put them in the 'vignettes' or 'man/figures' directories.  Otherwise, please put them in the top-level 'tools' directory, or a subdirectory of it.
>The CRAN incoming checks in r-devel were changed to perform the pandoc conversion checks with '--self-contained', and hence will warn about missing images.



## Test environments
* travis checks
* appveyor
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