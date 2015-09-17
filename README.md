#Introduction
This is the repository for an R package intended to provide tools to analyze and vizualise wind turbine power curves.

#Requirements
1. A working installation of R. This can be obtained from e.g. http://r-project.org.
2. Some experience with using R to explore data and plot results.
3. Basic understanding of how to use command line programs.

#Download
Click on the "Download ZIP" button on the lower right of this page. 

#Installing and using the package
Run the following code in the R console:
```R
# identify where the code was downloaded to 
setwd('~/Downloads/')
try(detach(name = "package:PowerPerformance",unload = TRUE))
library("roxygen2")
roxygenize(package.dir = "PowerPerformance")
system("R CMD build PowerPerformance --no-build-vignettes")
system("R CMD check PowerPerformance")
system("R CMD INSTALL PowerPerformance --preclean --build")
try(file.remove(file.path(getwd(),"PowerPerformance.pdf")))
system("R CMD Rd2pdf PowerPerformance")
# and make the package available
library("PowerPerformance")
```

#Documentation
Documentation is provided in several forms:
* From R. After installation, type ??PowerPerformance at the R console
* As a PDF manual
* As a PDF vignette, which can be found at  https://github.com/AndyClifton/PowerPerformance/vignettes/powercurves.pdf.

#Reporting issues and errors
Please use the issue-tracker at https://github.com/AndyClifton/PowerPerformance/issues to report issues.

#Wiki
Please use the wiki at https://github.com/AndyClifton/PowerPerformance/wiki as you feel fit.

#Comments
This software is provided as is, with no guarantees.

#Recent changes
9.15.2015 First commit.

#Code Maintainers
* [Andy Clifton](mailto:andrew.clifton@nrel.gov) (National Renewable Energy Laboratory)

#Project Contributors
* Rozenn Wagner, Danish Technical University
