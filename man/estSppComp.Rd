% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sppCompFuncs.r
\name{estSppComp}
\alias{estSppComp}
\title{The core expansion function executing an automated species expansion of Don's
Visual Basic code.}
\usage{
estSppComp(
  pacfinData,
  calcomData,
  portBorr = portMatrix2,
  qtrBorr = qtrMatrix,
  files = T
)
}
\arguments{
\item{pacfinData}{A data.frame as returned by getPacfinSppData.}

\item{calcomData}{A list as returned by getCalcomSppData.}

\item{portBorr}{A matrix to define the priority of port borrowing.
Rownames should indicate the port complex code of the actual stratum to
be filled. The first column contains the first priorty for borrowing
from, second column contains the second priority, ... etc. Elements
should be port complex codes as they appear in the samples. Elements not
given as appearing in the samples will code for Nominal.}

\item{qtrBorr}{A matrix to define the priority of qtr borrowing.
Rownames should define the actual qtr of the stratum to be filled.
The first column contains the the first priority to borrowing from,
second column contains the second priority, ... etc. Elements should be
integers \%in\% 1:4. Elements not given \%in\% 1:4 will code for Nominal.}

\item{files}{A boolean flag to produce verbose error files and/or
expansion output files such as sppdoc.}
}
\value{
a data.frame reminiscent of the calcom.com_lands table.
}
\description{
The core expansion function executing an automated species expansion of Don's
Visual Basic code.
}
