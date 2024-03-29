% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatreat.R
\name{datatreat}
\alias{datatreat}
\title{Organizes pedigree data in a chronological way}
\usage{
datatreat(data = NULL, n.max = 50, unk = 0, save = FALSE)
}
\arguments{
\item{data}{name of the pedigree data frame. Default=NULL.}

\item{n.max}{max number of iteractions to get the chronological order. Default = 50}

\item{unk}{the code of the data missing. Default=0.}

\item{save}{if TRUE, save the genealogy in a .txt file}
}
\value{
list with parental 1, parental 2, and real names of the individuals (key) also saves a txt file with the new chronological pedigree.
}
\description{
This function organizes pedigree data in a chronological way and return 3 lists: i) parental 1 values (numeric); ii) parental 2 values (numeric); iii) real names of the individuals. Also save a .txt file with new pedigree file.
}
\examples{
data(ped.mrode)
datatreat(ped.mrode)

}
\author{
Rodrigo R Amadeu, \email{rramadeu@gmail.com}
}
