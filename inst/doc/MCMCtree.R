## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.pos = 'H')

## ------------------------------------------------------------------------
if(!any(rownames(installed.packages()) == "MCMCtreeR")) install.packages("MCMCtreeR")
library(MCMCtreeR, quietly=TRUE, warn.conflicts=FALSE)

## ------------------------------------------------------------------------
data(apeData)
attach(apeData)
names(apeData)
minimumTimes
maximumTimes
monophyleticGroups
apeTree

## ---- fig.align='center', fig.cap='**Figure 1** Plot of the tree with node labelled with numbers', fig.height=4, fig.width=4, dpi=200----
plot(apeTree)
nodelabels()
tipDes(apeTree, 10)

## ------------------------------------------------------------------------
monophyleticGroups.user <- tipDes(apeTree, c(8, 10, 11, 13))
monophyleticGroups.user

## ------------------------------------------------------------------------
skewT_results <- estimateSkewT(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, phy=apeTree, plot=FALSE)
skewT_results$MCMCtree

## ---- fig.align='center', fig.cap='**Figure 2** Skew *t* distributions for all nodes', fig.height=6, fig.width=6, dpi=200----
par(mfrow=c(2,2), family="Palatino")
for(i in 1:4) plotMCMCtree(skewT_results$parameters[i,], method="skewT", title=paste0("node ", i), upperTime=max(maximumTimes))
skewT_results$MCMCtree

## ------------------------------------------------------------------------
# result in tree MCMCtree format
skewT_results$MCMCtree
## not run
## skewT_results <- estimateSkewT(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, phy=apeTree, plot=FALSE, pdfOutput="skewTPlot.pdf", writeMCMCtree=TRUE, MCMCtreeName="skewTInput.tre")

## ------------------------------------------------------------------------
## not run (remove ## to run)
## skewT_results <- estimateSkewT(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, shape=c(9, 10, 8, 10), phy=apeTree, plot=TRUE, pdfOutput="skewTPlot.pdf", writeMCMCtree=TRUE, MCMCtreeName="skewTInput.tre")
## skewT_results$parameters

## ------------------------------------------------------------------------
skewT_results <- estimateSkewT(minAge=minimumTimes[2], maxAge=maximumTimes[2], monoGroups=monophyleticGroups, scale=0.05, estimateShape=TRUE, estimateScale=FALSE, phy=apeTree, plot=FALSE, writeMCMCtree=FALSE)
skewT_results$parameters

## ------------------------------------------------------------------------
skewNormal_results <- estimateSkewNormal(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, addMode=0.05, phy=apeTree, plot=FALSE)
skewNormal_results$parameters

## ---- fig.align='center', fig.cap='**Figure 3** Skew normal distributions for all nodes', fig.height=6, fig.width=6, dpi=200----
par(mfrow=c(2,2), family="Palatino")
for(i in 1:4) plotMCMCtree(skewNormal_results$parameters[i,], method="skewNormal", title=paste0("node ", i), upperTime=max(maximumTimes))

## ---- fig.align='center', fig.cap='**Figure 4** Cauchy distributions for all nodes  (with a given scale)', fig.height=6, fig.width=6, dpi=200----
example_page_50 <- estimateCauchy(minAge=1, maxAge=4.32, monoGroups=monophyleticGroups[[1]],   phy=apeTree,  offset=0.5, minProb=0.025, plot=FALSE)[[1]]
plotMCMCtree(example_page_50, method="cauchy", title=paste0("node ", i), upperTime=max(maximumTimes))

## ---- fig.align='center', fig.cap='**Figure 5** Cauchy distributions for all nodes (with a given shape)', fig.height=6, fig.width=6, dpi=200----
cauchy_results <- estimateCauchy(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups,  offset=0.5, phy=apeTree, plot=FALSE)
cauchy_results$parameters
par(mfrow=c(2,2), family="Times")
for(i in 1:4) plotMCMCtree(cauchy_results$parameters[i,], method="cauchy", title=paste0("node ", i), upperTime=max(maximumTimes))

## ---- fig.align='center', fig.cap='**Figure 6** Cauchy distributions for all nodes (with a given shape) and smaller offset', fig.height=6, fig.width=6, dpi=200----
cauchy_results <- estimateCauchy(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups,  offset=c(0.5, 0.1, 0.1, 0.05), phy=apeTree, plot=FALSE)
cauchy_results$parameters
par(mfrow=c(2,2), family="Times")
for(i in 1:4) plotMCMCtree(cauchy_results$parameters[i,], method="cauchy", title=paste0("node ", i), upperTime=maximumTimes[i])

## ---- fig.align='center', fig.cap='**Figure 7** Uniform distributions for all nodes', fig.height=6, fig.width=6, dpi=200----
uniform_results <- estimateBound(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, phy=apeTree, plot=FALSE)
uniform_results$parameters
par(mfrow=c(2,2), family="Palatino")
for(i in 1:4) plotMCMCtree(uniform_results$parameters[i,], method="bound", title=paste0("node ", i), upperTime=maximumTimes[i]+1)

## ---- fig.align='center', fig.cap='**Figure 8** Gamma distributions for all nodes', fig.height=6, fig.width=6, dpi=200----
gamma_results <- estimateGamma(minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, alpha=188, beta=2690, offset=0.1, phy=apeTree, plot=FALSE)
gamma_results$parameters
par(mfrow=c(2,2), family="Palatino")
for(i in 1:4) plotMCMCtree(gamma_results$parameters[i,], method="gamma", title=paste0("node ", i), upperTime=maximumTimes[i])

## ------------------------------------------------------------------------
upper_results <- estimateUpper(maxAge=maximumTimes, monoGroups=monophyleticGroups, rightTail=0.025, phy=apeTree)
upper_results$parameters

## ------------------------------------------------------------------------
fixed_results <- estimateFixed(minAge=minimumTimes[1], monoGroups=monophyleticGroups[[1]], phy=apeTree)
fixed_results 

## ------------------------------------------------------------------------
each.node.method <- c("skewT", "cauchy", "gamma", "upper")
output.full <- MCMCtreePhy(phy=apeTree, minAge=minimumTimes, maxAge=maximumTimes, monoGroups=monophyleticGroups, method=each.node.method, writeMCMCtree = FALSE)

## ------------------------------------------------------------------------
estimate.alpha <- c(FALSE, FALSE, TRUE, FALSE)
estimate.beta <- c(TRUE, TRUE, FALSE, TRUE)
outputFull <- MCMCtreePhy(phy=apeTree, minAges=minimumTimes, maxAges=maximumTimes, monoGroups=monophyleticGroups, method=each.node.method, estimateAlpha=estimate.alpha, estimateBeta=estimate.beta, alpha=188, beta=100, writeMCMCtree = FALSE)

## ------------------------------------------------------------------------
skewNormal_results_nodeOne <- estimateSkewNormal(minAge=minimumTimes[1], maxAge=maximumTimes[1], monoGroups=monophyleticGroups[[1]], addMode=0.05, phy=apeTree, plot=FALSE, writeMCMCtree = FALSE)
skewNormal_results_nodeOne$apePhy

## ------------------------------------------------------------------------
cauchy_results_nodeTwo <- estimateCauchy(minAge=minimumTimes[2], maxAge=maximumTimes[2], monoGroups=monophyleticGroups[[2]],  offset=0.5, phy=skewNormal_results_nodeOne$apePhy, plot=FALSE, writeMCMCtree = FALSE)
cauchy_results_nodeTwo$apePhy

## ------------------------------------------------------------------------
uniform_results_nodeThree <- estimateBound(minAge=minimumTimes[3], maxAge=maximumTimes[3], monoGroups=monophyleticGroups[[3]], phy=cauchy_results_nodeTwo$apePhy, plot=FALSE, writeMCMCtree = FALSE)
uniform_results_nodeThree$apePhy

## ------------------------------------------------------------------------
## not run
# skewT_results_nodeFour <- estimateSkewT(minAge=minimumTimes[4], maxAge=maximumTimes[4], monoGroups=monophyleticGroups[[4]], scale=0.5, phy=cauchy_results_nodeTwo$apePhy, plot=FALSE, writeMCMCtree = TRUE)
# skewT_results_nodeFour$apePhy

