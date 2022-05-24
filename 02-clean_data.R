library(tidyverse)
library(fs)
library(network)
library(ergm)
library(ergm.count)
library(tictoc)


getwd()
fls <- dir_ls("data/monthly")
fls
fls <- fls |> str_subset("2017") |> str_subset("idp")
fls
out <- map(fls, read_csv, skip = 0, col_names = 1:74)
class(out)
length(out)
#out <- map2(.x = out, .y = 1:12, function(x,y) {x$month <- y; return(x)})
out <- map(out, function(x) as.matrix(x))

mat <- out[[1]] + out[[2]] + out[[3]] + out[[4]] + out[[5]] + out[[6]] + out[[7]] + out[[8]] + out[[9]] + out[[10]] + out[[11]] + out[[12]]

mat
## attributes

fls <- dir_ls("data/monthly")
fls

fls <- fls |> str_subset("2017") |> str_subset("var")

out <- map(fls, read_csv)
out <- map2(.x = out, .y = 1:12, function(x,y) {x$month <- y; return(x)})

out <-bind_rows(out)
out


dat <- out |> group_by(id) |>
  summarize(
    fatality = sum(fatality),
    conflict_occ = sum(conflict_occ),
    spi_m = mean(spi),
    spi_max = max(spi),
    spi_min = min(spi),
    pop = mean(Pop2014UNFPA),
    urban = mean(Urban2014UNFPA),
    rural = mean(Rural2014UNFPA)
  )


dat

## create the network

net <- network(mat, directed = TRUE)
net
net %e% "flow" <- mat
net %v% "fatality" <- dat$fatality
net %v% "conflict" <- dat$conflict_occ
net %v% "spi_m" <- dat$spi_m
net %v% "spi_max" <- dat$spi_max
net %v% "spi_min" <- dat$spi_min
net %v% "pop" <- dat$pop
net %v% "urban" <- dat$urban
net %v% "rural" <- dat$rural

## ERGMs
tic()
f0 <- ergm(net ~ edges)
toc() # 0.15s

tic()
f1 <- ergm(net ~ edges + diff("spi_m") +diff("spi_max")+diff("spi_min")+ 
               diff("conflict") + diff("fatality") + nodecov("pop") + 
               nodecov("conflict") + nodecov("fatality") + nodecov("spi_m") + 
               nodecov("spi_max") + nodecov("spi_min"))
toc() # 0.29s

tic()
f2 <- ergm(net ~ edges + diff("spi_m") + diff("spi_max") +
    diff("spi_min") + diff("conflict") + diff("fatality") + nodecov("pop") +
    nodecov("conflict") + nodecov("fatality") + nodecov("spi_m") +
    nodecov("spi_max") + nodecov("spi_min") + transitiveties)
toc() # 38s

summary(f0)
summary(f1)
summary(f2)

gof(f2)
## structural ergms

tic()
e1 <- ergm(net ~ edges + transitiveties)
toc() #10s

tic()
e2 <- ergm(net ~ edges + intransitive) #gwnsp(cutoff = 10) + gwesp(cutoff = 15)
toc() # 6s

tic()
e3 <- ergm(net ~ edges + gwesp(cutoff = 15),
           constraints = ~degreedist,
           control = snctrl(parallel = 3)
           ) #gwnsp(cutoff = 10) + gwesp(cutoff = 15)
toc() # 17s in parallel


summary(e3)
gof(e3)


## Weighted ergms
tic()
w0 <- ergm(net ~ nonzero + sum,
           response = "flow",
           reference = ~Poisson,
           #constraints = ~degreedist,
           control = snctrl(parallel = 3))
toc() # 358s

summary(w0)


save(e1, e2, e3, f0, f1, f2, net, file = "data/simple_ergms.RData")