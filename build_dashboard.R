devtools::load_all()
devtools::test()

Sys.setenv("_R_CHECK_FORCE_SUGGESTS_"=TRUE)
rcmdcheck::rcmdcheck()

spelling::spell_check_package()

devtools::build_vignettes(clean=FALSE)
devtools::clean_vignettes()



.libPaths("U:/My Documents/R/win-library/4.1")
devtools::build_vignettes()
devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::build()

#Check you've got the right version number
install.packages("../rpsftm_1.2.8.tar.gz",
                 repos = NULL, type = "source")
devtools::build("../rpsftm_1.2.8.tar.gz", binary=TRUE)

#install.packages("V:/STATISTICS/NON STUDY FOLDER/Software/R/R Code Library/cctu_0.9.0.zip",
#                 repos = NULL, type="win.binary")

#devtools::build( binary=TRUE)
# modify news.md, cran-comments.md README.rmd
devtools::build_readme()
urlchecker::url_check()

devtools::check_win_release()
rcmdcheck::rcmdcheck()
devtools::check_win_devel()
devtools::check_rhub()
spelling::spell_check_package()
#check github is totally up to date
devtools::release()
devtools::submit_cran()
#add a tag to github
#do some publicity , edit the website news, CTU stats email list.
#Add the .9000 suffix to the Version field in the DESCRIPTION to
#indicate that this is a development version.
#Create a new heading in NEWS.md and commit the changes.
