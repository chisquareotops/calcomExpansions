
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Calcom Expansions

<!-- badges: start -->
<!-- badges: end -->

An automated version of Don’s Visual Basic calcom expansion code
rewritten in R.

## Installation

Before installing the calcomExpansions package you should ensure that
Java is installed on your computer. Please visit <http://www.java.com>
for information on installing Java on your particular system.

To install the latest version of the `calcomExpansions` package you can
run the following in an R shell:

    R> install.packages("devtools") #if required
    R> devtools::install_github("chisquareotops/calcomExpansions")

Upon installing, some unix-based systems may need to to have Java
reconfigured with R by running the following command in the terminal:

    bash$ sudo R CMD javareconf

<!--
upon installing RJDBC you may need to run "R CMD javareconf" command in the 
#terminal as root to add Java support to R.
&#10;Additionally, two JDBC drivers are required for access to the CALCOM and Pacfin Databases.
These drivers need to be in the current working directory of R when running this expansion code.
The getDrivers() function can/should be run from the current working directory of R before running the expansion code.
-->

## Package Use

Calcom expansions combine information from both the PacFIN and CalCOM
databses and are completed on a yearly basis. The `calcomExpansions`
package provides functions for gathering and preparing the data
neccessary for completing expansions, and several core functions for
executing and exporting species and length expansions (age expansions
coming soon).

A typical use-case for this package will require access to both the
PacFIN and CalCOM databases. If you have not setup database access for
these databases you will need to arrange access to these databases prior
to using this package. Contact XXXX for access to the PacFIN database.
Contact YYYY for access to the CalCOM database and/or PSMFC VPN.

### Species Expansion

The data for species expansions are collected and prepared by the two
helper functions `getPacfinSppData` and `getCalcomSppData`.

The `getPacfinSppData(year, save = F, fromFile = F)` function prepares
an R data.frame from the neccessary PacFIN database calls needed to
perform a species expansion. By default this function connects to the
PacFIN database to prepare its output. If run with `save=True`, then the
output will be saved to a local csv file for replicability and/or
convienience purposes. If a local csv is present on your system,
`getPacfinSppData` can be run with `fromFile=True` to bypass VPN and
database connections.

A typical use will look like:

    R> year = 2019  
    R> pacfinSpp = getPacfinSppData(year)

    Reading PacFIN Species Data From PacFIN Connection...
    PacFIN User: ********
    Password: ************

Similarly the `getCalcomSppData(year, save = F, fromFile = F)` function
prepares an R list from the neccessary Calcom database calls needed to
perform a species expansion. By default this function connects to the
Calcom database to prepare its output. If run with `save=True`, then the
output will be saved to a local .rda file for replicability and/or
convience purposes. If a local .rda is present on your system,
`getCalcomSppData` can be run with `fromFile=True` to bypass VPN and
database connections.

A typical use will look like:

    R> calcomSpp = getCalcomSppData(year)

    Reading CALCOM Species Data From CALCOM Connection...
    CALCOM User: ********
    Password: ****************

Once both the PacFIN and Calcom data have been prepared, the data can be
passed to `estSppComp` function to perform the species composition
expansion as follows.

    R> sppExp = estSppComp(pacfinSpp, calcomSpp)

The
`estSppComp(pacfinData, calcomData, portBorr = portMatrix2, qtrBorr = qtrMatrix, files = T)`
function uses the two optional arguments `portBorr` and `qtrBorr` to
establish port complex and quarter borrowing rules to expand strata in
which no data exists. By default the matricies `portMatrix2` and
`qtrMatrix` are supplied which define the long established borrowing
standards used by Calcom (CITE). For more details about the structure of
these matrices, and the borrowing rules they imply, see the R help pages
for these data structures (i.e. `?portMatrix2' or`?qtrMatrix\` in an R
shell).

If manual adjusting of particular borrows are required (exceptions to
the borrowing rules implied by the given portBorr and qtrBorr arguments)
the
`estSppCompDoc(pacfinData, calcomData, doc = "sppdocYYYY.csv", qtrBorr = qtrMatrix, files = T)`
function may be used to to modify the borrows performed by `estSppComp`.
For more details about editing particular borrows via the expansion
documentation files see the R help page for these data `estSppCompDoc`
(i.e. `?estSppCompDoc` in an R shell)

Once the expansion is complete the results may be exported in several
different formats by the
`exportSpp(exp, human = T, pacfin = T, calcom = F, doc = NULL)`
function. See the R help page for details about the various export
formats available (i.e. `?exportSpp` in an R shell).

    R> exportSpp(sppExp)

Stripping out the commentary from the above commands results in the
following simple example of what a default expansion in 2019entails in
R.

    R> year = 2019  #Vectorized in year. year=2018:2023 would also work. 
    R> pacfinSpp = getPacfinSppData(year)

    Reading PacFIN Species Data From PacFIN Connection...
    PacFIN User: ********
    Password: ************

    R> calcomSpp = getCalcomSppData(year)

    Reading CALCOM Species Data From CALCOM Connection...
    CALCOM User: ********
    Password: ****************

    R> sppExp = estSppComp(pacfinSpp, calcomSpp)
    R> exportSpp(sppExp)

### Length Expansion

    R> year = 2019  #Vectorized in year. year=2018:2023 would also work. 

    R> calcomLen = getCalcomLenData(year)

    Reading CALCOM Length Data From CALCOM Connection...
    CALCOM User: ********
    Password: ****************

    R> lenExp = estLenComp(calcomLen)

### Age Expasion Coming Soon…

<!-- Age Expansion -->
