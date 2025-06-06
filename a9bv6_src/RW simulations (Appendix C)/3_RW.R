### random walk simulations 

set.seed(1)
rw_data <- data.frame()
# remember you always start from node 1, with the same network 

# path = 13, because start node = 1 and then you take 12 steps 

# repeat walk 10 times 
for(i in 1:10) {
    rw1 <- as.vector(random_walk(g_er, '1', 13, mode = 'all', stuck = 'return'))
    rw_data <- rbind(rw_data, data.frame(path = rw1, number_of_runs = 10, run_number = i, order = 1:13)) 
} # 12 * 10 = 120 rows 

# repeat walk 100 times 
for(i in 1:100) {
    rw1 <- as.vector(random_walk(g_er, '1', 13, mode = 'all', stuck = 'return'))
    rw_data <- rbind(rw_data, data.frame(path = rw1, number_of_runs = 100, run_number = i, order = 1:13)) 
} # 12 * 100 = 1200 rows 

# repeat walk 1000 times 
for(i in 1:1000) {
    rw1 <- as.vector(random_walk(g_er, '1', 13, mode = 'all', stuck = 'return'))
    rw_data <- rbind(rw_data, data.frame(path = rw1, number_of_runs = 1000, run_number = i, order = 1:13)) 
} # 12 * 1,000 = 12,000 rows 

# repeat walk 10,000 times 
for(i in 1:10000) {
    rw1 <- as.vector(random_walk(g_er, '1', 13, mode = 'all', stuck = 'return'))
    rw_data <- rbind(rw_data, data.frame(path = rw1, number_of_runs = 10000, run_number = i, order = 1:13)) 
} # 12 * 10,000 = 120,000 rows 

# repeat walk 100,000 times 
t1<-Sys.time()
for(i in 1:100000) {
    rw1 <- as.vector(random_walk(g_er, '1', 13, mode = 'all', stuck = 'return'))
    rw_data <- rbind(rw_data, data.frame(path = rw1, number_of_runs = 100000, run_number = i, order = 1:13)) 
} # 12 * 100,000 = 1,200,000 rows 
Sys.time()-t1 # 1.4h, very slow 

save(rw_data, file = 'rw_data.RData') 
