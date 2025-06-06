### set up for SA vs. RW simulations 

library(ggplot2)
library(spreadr)
library(igraph)
library(plyr)
library(dplyr)
library(tidyr)

### generate a random graph
## ER network
set.seed(1234)
g_er <- erdos.renyi.game(100,0.15,type='gnp') # 100 nodes, 738 edges 
V(g_er)$name <- 1:100
