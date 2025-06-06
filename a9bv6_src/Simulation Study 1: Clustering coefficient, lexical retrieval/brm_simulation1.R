# simulation study 1: clustering coefficient effect in word recognition
# replication of Vitevitch et al. (2011) and Chan & Vitevitch (2009)
# Siew. (submitted). spreadr: A R package to simulate spreading activation in a network. 

library(spreadr)
library(dplyr)
options(stringsAsFactors = FALSE)

# load data 

load('ego2hopnets_24.RData') # 24 networks 
v2011 <- read.csv('24toynets.csv') # degree and clustering coefficient values for each of the 24 words

# set up

retentions <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

# spreading activation simulation

t1 <- Sys.time() # 24*9 = 216 simulations

for (j in 1:9) {
  r <- retentions[j]
  for (i in 1:nrow(v2011)) {
    sim_result <- spreadr::spreadr(start_run = data.frame(node = v2011$name[i], activation = 100), decay = 0, # target node gets 100 units at t = 0
                                  retention = r, suppress = 0,
                                  network = ego2hopnets_24[[i]], time = 10)
    write.csv(sim_result, file = paste0(i, '_', v2011$name[i], '_', r, '.csv')) # save results for later 
  }
}

Sys.time()-t1 # 3.58s 

# compile output - get final activation of target node in all simulations

final_act <- data.frame(name = vector(), final_act = vector(), retention = vector())

for (j in 1:9) {
  r <- retentions[j]
  for (i in 1:nrow(v2011)) {
    data <- read.csv(paste0(i, '_', v2011$name[i], '_', r, '.csv')) # results from earlier
    max_t <- max(data$time) # last time step
    data <- data %>% filter(time == max_t) %>% filter(node == as.character(v2011$name[i]))
    data <- data[2:3]
    data$retention <- r
    colnames(data) <- c('name', 'final_act', 'retention')
    final_act <- rbind(final_act, data)
  }
}

# regression analysis - leave data in long format

v2011_sim <- left_join(final_act, v2011, by = 'name')

m1 <- lm(final_act ~ scale(retention) + scale(degree) + scale(clustering), data = v2011_sim) 

summary(m1)

