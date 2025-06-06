na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

get_prop <- function(data) {
    x <- as.data.frame(table(data$path))
    colnames(x) <- c('node', 'prop')
    x$prop <- x$prop/nrow(data)
    return(na.zero(merge(data.frame(node = 1:100), x, all.x = T)))
}
