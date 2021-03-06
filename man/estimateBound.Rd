% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateBound.R
\name{estimateBound}
\alias{estimateBound}
\title{Estimate a Uniform Distribution for MCMCtree}
\usage{
estimateBound(minAge, maxAge, minProb = 0.025, rightTail = 0.025, phy,
  monoGroups, writeMCMCtree = FALSE, plot = TRUE,
  MCMCtreeName = "bound.tre", pdfOutput = "uniformPlot.pdf")
}
\arguments{
\item{minAge}{vector of minimum age bounds for nodes matching order in monoGroups}

\item{maxAge}{vector of maximum age bounds for nodes matching order in monoGroups}

\item{minProb}{probability of left tail (minimum bound) - default to hard minimum (minProb=0)}

\item{rightTail}{probability of right tail (maximum bound default = 0.975)}

\item{phy}{fully resolved phylogeny in ape format}

\item{monoGroups}{list  with each element containing species that define a node of interest}

\item{writeMCMCtree}{logical whether to write tree in format that is compatible with MCMCTree to file}

\item{plot}{logical specifying whether to plot to PDF}

\item{MCMCtreeName}{MCMCtree.output file name}

\item{pdfOutput}{pdf output file name}
}
\value{
list containing node estimates for each distribution
\itemize{
 \item{"parameters"}{ estimated parameters for each node}
 \item{"apePhy"}{ phylogeny in ape format with node labels showing node distributions}
 \item{"MCMCtree"}{ phylogeny in MCMCtreeR format}
 \item{"nodeLabels"}{ node labels in MCMCtreeR format}
}

If plot=TRUE plot of distributions in file 'pdfOutput' written to current working directory

If writeMCMCtree=TRUE tree in MCMCtree format in file "MCMCtreeName" written to current working directory
}
\description{
Estimate the paramaters of a soft-bounded uniform distribution and output trees for MCMCtree input
}
\examples{
data(apeData)
attach(apeData)
## extract taxon descending from calibrated nodes 8, 10, 11, 13
## these nodes can be visualised using plot.phylo
## and nodelabels from ape
monophyleticGroups <- tipDes(apeData$apeTree, c(8,10,11,13))
minimumTimes <- c("nodeOne"=15, "nodeTwo"=6,
"nodeThree"=8, "nodeFour"=13) / 10
maximumTimes <- c("nodeOne" = 30, "nodeTwo" = 12,
"nodeThree"=12, "nodeFour" = 20) / 10
estimateBound(minAge=minimumTimes, maxAge=maximumTimes, 
monoGroups=monophyleticGroups, phy=apeTree, plot=FALSE)$MCMCtree
}
\author{
Mark Puttick
}
