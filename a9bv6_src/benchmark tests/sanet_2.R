## R3's function 

sanet_2 <- function(
  network, # AN `igraph` network object
  start_run, # A data.frame LIKE 'initial_df' FROM THE MANUSCRIPT
  retention = 0.5, # THE RETENTION PARAMATER
  time = 10, # THE NUMBER OF TIME STEPS
  decay = 0, # decay paramater
  suppress = 0 # suppress parameter
){
  ### ERROR MESSAGES
  # check if start_run is in the correct format
  if (is.data.frame(start_run) == F || colnames(start_run) != c('node', 'activation')) {
    stop('Initial activation dataframe is not in the correct format. Must be a dataframe with -node-
         and -activation- columns.')
  }

  # check if the node column in start_run is a factor (if so, it must be converted to character class)
  if (is.factor(start_run$node) == T) {
    start_run$node <- as.character(start_run$node)
  }

  # # check if node labels are unique
  if (length(unique(igraph::V(network)$name)) != igraph::gorder(network)) {
    stop('Nodes need to have unique labels.')
  }

  # check if decay is a number from 0 to 1
  if (decay < 0 || decay > 1) {
    stop('Decay value is not a number from 0 to 1.')
  }

  # check if retention is a number from 0 to 1
  if (retention < 0 || retention > 1) {
    stop('Retention value is not a number from 0 to 1.')
  }

  # check if time is a non-negative number
  if (time < 0 || is.numeric(time) == F) {
    stop('Something is off with the time value.')
  }

  # check if network is an igraph object
  if (igraph::is.igraph(network) == F) {
    stop('Network is not an igraph object.')
  }

  # check if the igraph object has a name attribute
  if (is.null(igraph::V(network)$name) == T) {
    stop('Network does not have a "name" attribute.')
  }

  # CONVERT IGRAPH OBJECT TO ADJACENCY MATRIX
  if(is.weighted(network) == T) {
    mat<-as_adj(network, sparse = F, attr="weight") # to keep the weights in a weighted graph
  } else {
    mat<-as_adj(network, sparse = F) # unweighted graph
  }
  d <- apply(mat, 2, sum) # GET THE DEGREE OF EACH NODE

  # CREATE THE ACTIVATION VECTOR
  a <- rep(0, length(d)) # empty vector
  names(a) <- names(d)
  a[start_run$node] <- start_run$activation # add the activation input in; multiple values ok

  # OBJECT TO STORE RESULTS
  out_df = data.frame(node = names(a), activation = a, time = 0L)

  # ACTIVATION AT TIME T
  a_t = a

  # REPEAT PROCESS FOR EACH TIME STEP
  for (i in seq_len(time)){
    # ACTIVATION AT TIME T - 1
    a_tm1 = a_t
    # ACTIVATION MATRIX AT TIME T
    mat_t <- apply(mat, 1, function(x) (1 - retention)*x*a_tm1/d) # this generalizes to weighted networks
    diag(mat_t) <- retention*a_tm1
    # suppress cells less than suppress parameters
    mat_t[mat_t < suppress] <- 0
    # UPDATED ACTIVATION VECTOR
    a_t <- apply(mat_t, 2, sum)
    # decay activation at the end of each time step
    a_t <- a_t * (1 - decay)
    # STORE RESULTS
    out_df <- rbind(out_df, data.frame( node = names(a_t),
                                        activation = a_t,
                                        time = i))
  }
  return(out_df) }

