#set up 
library(igraph)
library(dplyr)
library(spreadr)

load('usf.RData') # graph from nelson norms, 10617 nodes, 72168 edges
# data from usf_nodes and usf_edges 
# http://vlado.fmf.uni-lj.si/pub/networks/data/dic/fa/FreeAssoc.htm 

usf <- simplify(usf) # remove loops 
E(usf)$weight <- 1 # make it unweighted, now 10617 nodes, 63782 edges 

load('spp_200_pairs.RData')

library(parallel)
library(doParallel)
library(foreach)

# initialize
# Create cluster with desired number of cores
ncores <- parallel::detectCores()
cl <- parallel::makeCluster(ncores)
# Register cluster
doParallel::registerDoParallel(cl)

t1 <- Sys.time()
# create parallel function
x <- foreach::foreach(i = 1:nrow(spp_200_pairs), .combine = rbind, # nrow(spp_200_pairs)
                      .packages = c('dplyr', 'spreadr', 'igraph', 'foreach')) %dopar% {
                        
                        # set up initial_df
                        initial_df <- data.frame(node = spp_200_pairs$Prime[i], activation = 100, stringsAsFactors = F)
                        
                        # run the simulation
                        result <- spreadr::spreadr(network = usf, 
                                                   start_run = initial_df, retention = 0.6, time = 10, 
                                                   suppress = 0, decay = 0)
                        
                        # get the results
                        out <- as.data.frame(result %>% filter(node == spp_200_pairs$TargetWord[i]))
                        out2 <- c(spp_200_pairs$Prime[i], spp_200_pairs$TargetWord[i], out$activation)
                        out2
                        # end_run <- rbind(end_run, out2)
                        # 
                        # end_run
                      }
Sys.time()-t1 # 7.4 hours 
# close cluster
parallel::stopCluster(cl)

head(x) # outputs from parallel are saved here 

save(x, file = 'spp_retention_0.6.RData')
