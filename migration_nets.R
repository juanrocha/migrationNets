library(tidyverse)
library(network)
library(ergm)
library(ergm.count)
library(tictoc)

getwd()

# fit indegree because 1) it is power law, and 2) the centers receiving migration
# are likely to be the same


dat <- read_csv(file = "data/monthly/idp012016.csv", skip = 0, col_names = 1:74)
dat2 <- read_csv(file = "data/monthly/var012016.csv")
dat |>
    as.matrix() |>
    isSymmetric() # directed network

dat2

net <- network(as.matrix(dat), directed = TRUE)
net %e% "flow" <- as.matrix(dat)
net

dat2 <- dat2 |>
    mutate(node = str_c("X", id))

net %v% "fatality" <- dat2$fatality
net %v% "conflict" <- dat2$conflict_occ
net %v% "spi" <- dat2$spi
net %v% "pop" <- dat2$Pop2014UNFPA
net %v% "urban" <- dat2$Urban2014UNFPA
net %v% "rural" <- dat2$Rural2014UNFPA


plot(net)



# observed distribution of triangles
summary(net~triadcensus)
## check
??sna::triad.classify
## triangle 021U has large count = a-->b<--c, a<-!->c, which is outgoing shared partner "OSP" in dgwdsp()

#### ergm: not weighted ####

f0 <- ergm(net ~ edges)
f1 <- ergm(net ~ edges + diff("spi") + diff("conflict") + diff("fatality") + nodecov("pop") + nodecov("conflict") + nodecov("spi") + nodecov("fatality"))

summary(f1)

mcmc.diagnostics(f1)


f2 <- ergm(net ~ edges + diff("spi") + diff("conflict") + diff("fatality") + nodecov("pop") + nodecov("conflict") + nodecov("spi") + nodecov("fatality"))

## This model takes ages and do not converege
tic()
f2 <- ergm(net ~ edges + triadcensus)
toc()
## this one does not finish either
summary(net ~ edges + mutual + dgwdsp)

tic()
f3 <- ergm(net ~ edges + dgwdsp(1, fixed = FALSE, cutoff = 7, type = c("OSP")))
toc()

summary(f1)
#### ergm: valued ####

v0 <- ergm(
    net ~ nonzero + sum, response = "flow",
    reference = ~Poisson)

tic()
v1 <- ergm(net ~ nonzero + sum + mutual, response = "flow", reference = ~Poisson)
toc() # 511s

tic()
v2 <- ergm(net ~ nonzero + sum + mutual + triadcensus, response = "flow", reference = ~Poisson)
toc() # 511s

tic()
v2 <- ergm(net ~ nonzero + sum + mutual + dgwdsp(0.5, fixed = FALSE, type = "OSP"),
           response = "flow", reference = ~Poisson)
toc() # 511s


summary(v1)
