
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

To install the latest version of the package:

``` r
install.packages("devtools") #if required
devtools::install_github("chisquareotops/calcomExpansions")
```

Upon installing, some unix-based systems may need to to have Java
reconfigured with R by running the following command in the terminal:

``` bash
sudo R CMD javareconf
```

<!--
upon installing RJDBC you may need to run "R CMD javareconf" command in the 
#terminal as root to add Java support to R.
&#10;Additionally, two JDBC drivers are required for access to the CALCOM and Pacfin Databases.
These drivers need to be in the current working directory of R when running this expansion code.
The getDrivers() function can/should be run from the current working directory of R before running the expansion code.
-->

## Package Use

### Species Expansion

### Length Expansion

<!-- Age Expansion -->
