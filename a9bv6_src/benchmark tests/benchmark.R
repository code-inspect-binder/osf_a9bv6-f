# benchmark tests

library(sanetr) # original function
source('sanet_2.R') # R3's function
library(spreadr) # R3's function + improvements

library(microbenchmark)
library(igraph)
library(ggplot2)
library(dplyr)

set.seed(1234)
g <- erdos.renyi.game(100, 0.15, type = 'gnp') # 100 nodes
V(g)$name <- 1:100

n <- g
t <- 10
r <- 0.5
d <- 0
s <- 0
initial_df <- data.frame(node = '1', activation = 100)

t1<-Sys.time()
res <-
  microbenchmark(
    sanetr::sanet(
      network = g,
      start_run = initial_df,
      time = t,
      retention = r,
      decay = d,
      suppress = s
    ),
    sanet_2(
      network = g,
      start_run = initial_df,
      time = t,
      retention = r,
      decay = d,
      suppress = s
    ),
    spreadr::spreadr(
      network = g,
      start_run = initial_df,
      time = t,
      retention = r,
      decay = d,
      suppress = s
    ),
    times = 100
  )
Sys.time()-t1 # ~ 1.2h, so you should go grab a coffee :)  
ggplot2::autoplot(res)

# check that outputs are identical  

x1 <- sanetr::sanet(
  network = g,
  start_run = initial_df,
  time = t,
  retention = r,
  decay = d,
  suppress = s
)

x2 <- sanet_2(
  network = g,
  start_run = initial_df,
  time = t,
  retention = r,
  decay = d,
  suppress = s
)

x3 <- spreadr::spreadr(
  network = g,
  start_run = initial_df,
  time = t,
  retention = r,
  decay = d,
  suppress = s
)

x1 %>% filter(node == '1')
x2 %>% filter(node == '1')
x3 %>% filter(node == '1')

# save(res, file = 'benchmark_output.RData')

