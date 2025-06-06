### spreading activation simulations 

source('1_setup.R')

### "pure" version: decay = 0, retention = 0, suppress = 0, t = until threshold of 0.001 is reached (t=12)
target_node <- as.character(V(g_er)$name[1])
initial_df <- data.frame(node = target_node, activation = 100)
result <- spreadr::spreadr(start_run = initial_df, decay = 0,
                           retention = 0, suppress = 0,
                           network = g_er, time = 20)
result2 <- spreadr::spreadr_2(start_run = initial_df, decay = 0, # variation of spreadr that includes the threshold to stop argument
                              retention = 0, suppress = 0,
                              network = g_er, time = 20, ignore_time = T, threshold_to_stop = 0.001)

# sanity check 
result %>% filter(node == 1) %>% filter(time == 5)
result2 %>% filter(node == 1) %>% filter(time == 5)
# everything is in order 

save(result2, file = 'sa_raw_data.RData')
