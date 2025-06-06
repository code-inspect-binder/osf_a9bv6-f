# comparing the results of spreadr and random walk simulations 
# load the data 
load('sa_raw_data.RData')
load('rw_data.RData')
source('helpr.R') # helper functions 

# SA results at time = 12 
# what proportion of activation did each node receive at the end of 12 time steps? 
result3 <- result2 %>% filter(time == 12) 
colnames(result3) <- c('node', 'act_prop', 'step')
result3$step <- 'SA'
result3$act_prop <- result3$act_prop/100 # convert to a proportion of initial amount of activation 

# compile RW results at the end of each walk 
# what is the probability that node i is found at the end of a 12-step walk? 

# ***these two values from spreadr and rw should match up when enough random walks are taken 

compiled_rw_2 <- data.frame(node = 1:100)
runs <- c(10,100,1000,10000,100000)

for(j in 1:5) {
    samba <- rw_data %>% filter(number_of_runs == runs[j]) %>% filter(order == (13)) # get the node at the end of each walk 
    dance <- get_prop(samba)
    colnames(dance)[2] <- paste0('final_step_',runs[j],'_runs')
    compiled_rw_2 <- merge(compiled_rw_2, dance, all.x = T)
}

out_long <- compiled_rw_2 %>% gather(step, act_prop, 
                                     `final_step_10_runs`:`final_step_1e+05_runs`, factor_key = T) # make long for ggplot
out_long$step <- mapvalues(out_long$step, from = c('final_step_10_runs', "final_step_100_runs", "final_step_1000_runs", "final_step_10000_runs", "final_step_1e+05_runs"), to = c('RW_10', "RW_100", "RW_1000", "RW_10000", "RW_100000"))

# combine spreadr and rw results and visualize them

out_long2 <- rbind(out_long, result3)

# max(out_long2$act_prop) # 0.1
out_long2$node = with(result3, reorder(node, act_prop, mean)) # order by SA 

# plot 1
# show all lines 
ggplot(out_long2, aes(x=as.numeric(node), y=act_prop, group=step)) + geom_line(aes(linetype=step)) + scale_linetype_manual(values=c('longdash', "twodash", "dotdash", 'dotted', 'dashed', 'solid')) + ylim(0,0.1) + xlim(1,100) + ggtitle('spreading activation vs. random walk') + ylab('activation proportion') + xlab('node, ordered') 

ggsave(filename = 'SA_RW_all.pdf', width = 11, height = 9.5, units = "in")

# plot 2 
# show select lines 
ggplot(out_long2, aes(x=as.numeric(node), y=act_prop, group=step)) + geom_line(aes(linetype=step)) + scale_linetype_manual(values=c('blank', "blank", "dotdash", 'blank', 'dashed', 'solid')) + ylim(0,0.022) + xlim(1,100) + ggtitle('spreading activation vs. random walk', subtitle = 'average of 1,000 vs. 100,000 runs of random walks vs. spreadr at t=12 (solid)') + ylab('activation proportion') + xlab('node, ordered') 

ggsave(filename = 'SA_RW_some.pdf', width = 11, height = 9.5, units = "in")

# plot 3 
# 100,000 vs SA
ggplot(out_long2, aes(x=as.numeric(node), y=act_prop, group=step)) + geom_line(aes(linetype=step)) + scale_linetype_manual(values=c('blank', "blank", "blank", 'blank', 'dashed', 'solid')) + ylim(0,0.022) + xlim(1,100) + ggtitle('spreading activation vs. random walk', subtitle = 'average of 100,000 runs of random walks vs. spreadr at t=12 (solid)') + ylab('activation proportion') + xlab('node, ordered') 

ggsave(filename = 'SA_RW_final.pdf', width = 11, height = 9.5, units = "in")

# correlations 

# convert to wide 
out_wide <- out_long2 %>% tidyr::spread(key = step, value = act_prop)

cor.test(out_wide$SA, out_wide$RW_10) 
cor.test(out_wide$SA, out_wide$`RW_100`)
cor.test(out_wide$SA, out_wide$`RW_1000`)
cor.test(out_wide$SA, out_wide$`RW_10000`)
cor.test(out_wide$SA, out_wide$`RW_100000`)

pdf('sa_rw_1.pdf', width = 5, height = 5)
plot(out_wide$SA, out_wide$RW_10, xlab = 'SA proportion', ylab ='RW proportion (10 runs)')
abline(a=0,b=1, col = 'blue')
dev.off()

pdf('sa_rw_2.pdf', width = 5, height = 5)
plot(out_wide$SA, out_wide$`RW_100`, xlab = 'SA proportion', ylab ='RW proportion (100 runs)')
abline(a=0,b=1, col = 'blue')
dev.off()

pdf('sa_rw_3.pdf', width = 5, height = 5)
plot(out_wide$SA, out_wide$`RW_1000`, xlab = 'SA proportion', ylab ='RW proportion (1,000 runs)')
abline(a=0,b=1, col = 'blue')
dev.off()

pdf('sa_rw_4.pdf', width = 5, height = 5)
plot(out_wide$SA, out_wide$`RW_10000`, xlab = 'SA proportion', ylab ='RW proportion (10,000 runs)')
abline(a=0,b=1, col = 'blue')
dev.off()

pdf('sa_rw_5.pdf', width = 5, height = 5)
plot(out_wide$SA, out_wide$`RW_100000`, ylim = c(0,0.02), xlim = c(0,0.02), xlab = 'SA proportion', ylab ='RW proportion (100,000 runs)')
abline(a=0,b=1, col = 'blue')
dev.off()
