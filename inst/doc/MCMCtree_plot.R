## ---- echo=FALSE, warning=FALSE, message=FALSE---------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)

## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.pos = 'H')

## ---- echo=FALSE, warning=FALSE, message=FALSE, fig.align='center', fig.cap='', fig.height=6, fig.width=6, fig.path='MCMCtree_plot_fig/plot1'----
par(mfrow=c(1,2))
library(MCMCtreeR)
data(MCMCtree.output)
attach(MCMCtree.output)
phy <- readMCMCtree(MCMCtree.phy, from.file=FALSE)
MCMC.tree.plot(phy, MCMC.chain=MCMCtree.posterior, time.correction=100, plot.type="distributions", cex.age=0.7, cex.labels=0.7, relative.height=0.08, col.tree="grey40", scale.res=c("Eon", "Period"), no.margin=TRUE, label.offset=4, density.col = "#4169E170", density.border.col = "#4169E190",  label.timescale.names=FALSE, tip.color="white",  cex.tips=0.1)
MCMC.tree.plot(phy, MCMC.chain=MCMCtree.posterior, time.correction=100, plot.type="cladogram", cex.age=0.7, cex.labels=0.7, relative.height=0.08, col.tree="#008b00", scale.res=c("Eon", "Period"), no.margin=TRUE, label.offset=4, col.age = "#008b0070", label.timescale.names=FALSE, tip.color="white",  cex.tips=0.1)

## ---- warning=FALSE, message=FALSE---------------------------------------
if(!any(rownames(installed.packages()) == "MCMCtreeR")) install.packages("MCMCtreeR")
library(MCMCtreeR, quietly=TRUE, warn.conflicts=FALSE)
data(MCMCtree.output)
attach(MCMCtree.output)
names(MCMCtree.output)
phy <- readMCMCtree(MCMCtree.phy, from.file=FALSE)

## ---- fig.align='center', fig.cap='**Figure 1** Phylogeny with posterior distributions of age on nodes and timescale', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, cex.tips=0.2, time.correction=100, scale.res=c("Eon", "Period"), plot.type="phylogram", cex.age=0.6, cex.labels=0.6, relative.height=0.08, col.tree="grey40", label.offset=4, node.method="none", no.margin=TRUE)

## ---- fig.align='center', fig.cap='**Figure 2** Phylogeny with posterior distributions of age on nodes and timescale showing all units', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, cex.tips=0.2, time.correction=100, scale.res=c("Eon", "Period", "Epoch", "Age"), plot.type="phylogram", cex.age=0.4, cex.labels=0.5, relative.height=0.08, col.tree="grey40", label.offset=4, node.method="none", no.margin=TRUE)

## ---- fig.align='center', fig.cap='**Figure 3** Phylogeny with age uncertainty displayed as bars on nodes.', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, analysis.type="MCMCtree", cex.tips=0.2, time.correction=100, plot.type="phylogram", lwd.bar=2, scale.res=c("Eon", "Period"), node.method="bar", col.age="navy", no.margin=TRUE, label.offset=4)

## ---- fig.align='center', fig.cap='**Figure 4** Phylogeny with age uncertainty displayed spanning the vertical height of the node.', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, analysis.type="MCMCtree", cex.tips=0.2, time.correction=100, plot.type="phylogram", lwd.bar=2, scale.res=c("Eon", "Period"), node.method="node.length", col.age="#008b0040", no.margin=TRUE, label.offset=4)

## ---- fig.align='center', fig.cap='**Figure 5** Phylogeny with age uncertainty of one node spanning the height of the phylogeny.', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, analysis.type="MCMCtree", cex.tips=0.2, time.correction=100, plot.type="phylogram", lwd.bar=2, scale.res=c("Eon", "Period"), node.method="full.length", all.nodes=110, col.age="#ff000040", no.margin=TRUE, label.offset=4)

## ------------------------------------------------------------------------
# directory.mb <- "/User/MrBayes/output.nex.con.tre"
# MCMC.tree.plot(analysis.type="mrbayes", directory.files=directory.mb, cex.tips=0.33, plot.type="phylogram", lwd.bar=2, add.time.scale=FALSE, node.method="bar", col.age="navy")

## ------------------------------------------------------------------------
# directory.rb <- "/User/RevBayes/output.nex.con.tre"
# MCMC.tree.plot(analysis.type="revbayes", directory.files=directory.rb, cex.tips=0.33, plot.type="phylogram", lwd.bar=2, add.time.scale=FALSE, node.method="bar", col.age="navy")

## ---- fig.align='center', fig.cap='**Figure 6** Cladogram plot', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, analysis.type="MCMCtree", cex.tips=0.2, time.correction=100, plot.type="cladogram", lwd.bar=2, scale.res=c("Eon", "Period"), node.method="node.length", col.age="#008b0080", no.margin=TRUE, cex.labels=0.5)

## ---- fig.align='center', fig.cap='**Figure 7** Phylogeny with posterior distributions of age on nodes.', fig.height=6, fig.width=6----
MCMC.tree.plot(phy, MCMC.chain=MCMCtree.posterior, cex.tips=0.2, time.correction=100, plot.type="distributions", cex.age=0.4, cex.labels=0.5, relative.height=0.08, col.tree="grey40", scale.res=c("Eon", "Period"), no.margin=TRUE, label.offset=4, density.col = "#00000050", density.border.col = "#00000080")

## ---- fig.align='center', fig.cap='**Figure 8** Phylogeny with posterior distributions of age on nodes, with the root a different colour.', fig.height=6, fig.width=6----
node.colours <- rep("#ff008050", Nnode(phy$apePhy))
node.colours.border <- rep("#ff008090", Nnode(phy$apePhy))
node.colours[1] <- "#00008050"
node.colours.border[1] <- "#00008090"
MCMC.tree.plot(phy, MCMC.chain=MCMCtree.posterior, cex.tips=0.2, time.correction=100, scale.res=c("Eon", "Period"), plot.type="distributions", cex.age=0.4, cex.labels=0.5, relative.height=0.08, col.tree="grey40", col.age="black", density.col=node.colours, density.border.col=node.colours.border, grey.bars = TRUE, no.margin=TRUE)

## ---- fig.align='center', fig.cap='**Figure 9** Phylogeny showing the node and tip labels.', fig.height=3, fig.width=3----
data(MCMCtree.output)
attach(MCMCtree.output)
phy <- readMCMCtree(MCMCtree.phy, from.file = FALSE)
phy <- phy$apePhy
# Here a smaller tree is shown as an example.
phy.small <- extract.clade(phy, 177)
plot(phy.small, no.margin=TRUE, cex=0.6)
# view nodelabels on the tree
nodelabels(cex=1)

## ------------------------------------------------------------------------
node.numbers <- c(9, 10, 11, 12, 13, 14, 15)
# create a list to store each posterior sample for every node
node.posteriors <- vector(mode="list", length=Nnode(phy.small))
names(node.posteriors) <- node.numbers
for(i in 1:Nnode(phy.small)) node.posteriors[[i]] <- rnorm(1000, mean=branching.times(phy.small)[i], sd=0.1)
## substitute the random numbers with the appropriate posterior distribution for that node
## e.g.,
# your.age.values <- read.csv("posterior.ages.from.an.analysis")
# for(i in 1:Nnode(phy.small)) node.posteriors[[i]] <- your.age.values

## ---- fig.align='center', fig.cap='**Figure 10** Phylogeny showing the node and tip labels.', fig.height=4, fig.width=4----
MCMC.tree.plot(phy = phy.small, node.ages = node.posteriors, analysis.type = "user", cex.tips = 0.7, time.correction = 100, scale.res = c("Eon", "Period"), plot.type = "distributions", cex.age = 0.7, cex.labels = 0.5, relative.height = 0.08, col.tree = "grey40", no.margin = TRUE)

## ------------------------------------------------------------------------
# Not run. Plot tree with node labels to help work out how labels correspond to tree from software output.
## plot(ape)
## nodelabels()
# extract the MCMC chain and the phylogeny edge object
MCMC.chain <- MCMCtree.posterior
phy.edge <- phy$edge
# extract ages with node age posteriors from column 2
MCMC.node.ages <- MCMC.chain[,2:Ntip(phy)]
# extract the names from these data so they are numeric node labels that match the APE tree format
all.nodes <- as.numeric(gsub("t_n", "", colnames(MCMC.node.ages)))
# create a vector of names for each list element as internal nodes from APE tree, using phy$edge object.
node.ages.names <- c(Ntip(phy) +1, phy.edge[which(phy.edge[,2] > Ntip(phy)),2])
# find where each posterior node age appears in APE edge object.
match.nodes <- match(all.nodes, as.numeric(node.ages.names))
# create a list extracting the information from the MCMC chain in APE node order.
node.ages <- lapply(match.nodes, function(uu) MCMC.node.ages[,uu])
# name each element in list.
names(node.ages) <- node.ages.names

## ---- fig.align='center', fig.cap='**Figure 11** Phylogeny plotted with user-formatted data.', fig.height=6, fig.width=6----
MCMC.tree.plot(phy=phy, node.ages=node.ages, analysis.type="user", cex.tips=0.2, time.correction=100, scale.res=c("Eon", "Period"), plot.type="distributions", cex.age=0.4, cex.labels=0.5, relative.height=0.08, col.tree="grey40", no.margin=TRUE)

