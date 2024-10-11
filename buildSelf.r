#
library(devtools)


#make a .Rbuildignore
use_build_ignore(c('drivers', 'sppCompQuick.pdf', 'sppCompQuick.tex', 'buildSelf.r'))
#compiles the Roxygen documentation to R package doumentation.
document()
#run the data-raw code. data-raw code should contain use_data() to export cleaned up data.
#Rscript ./data-raw/inherents.r
source("./data-raw/inherents.r")
#check the structure of your R package so far
check()
#edit README.rmd and this function builds the README.md
build_readme()
#similate installing the package and importing it
load_all()
