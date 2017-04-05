# rpsftm 1.0.2.9000

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
