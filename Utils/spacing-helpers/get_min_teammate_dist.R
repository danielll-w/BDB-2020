get_min_teammate_dist <- function(position_table) {
    
    min_dist <- vector(mode = "double", length = nrow(position_table))
    min_dist_idx <- vector(mode = "logical", length = nrow(position_table))
    min_dist_x <- vector(mode = "double", length = nrow(position_table))
    min_dist_y <- vector(mode = "double", length = nrow(position_table))
    
    for (i in 1:nrow(position_table)) {
        x <- position_table$x[i]
        y <- position_table$y[i]
        team <- position_table$team[i]
        
        if (team == "football") {
            next()
        }
        
        idx <- position_table$team == team & position_table$nflId != position_table$nflId[i]
        min_dist[i] = (sqrt((x - position_table$x[idx])^2 + (y - position_table$y[idx])^2))
        
        nflId_sub <- position_table$nflId[idx]
        x_sub <- position_table$x[idx]
        y_sub <- position_table$y[idx]
        min_dist_idx[i] <- nflId_sub[which.min(sqrt((x - position_table$x[idx])^2 + (y - position_table$y[idx])^2))]
        min_dist_x[i] <- x_sub[which.min(sqrt((x - position_table$x[idx])^2 + (y - position_table$y[idx])^2))]
        min_dist_y[i] <- y_sub[which.min(sqrt((x - position_table$x[idx])^2 + (y - position_table$y[idx])^2))]
    }
    
    position_table$teammate_min_dist <- min_dist
    position_table$teammate_min_dist_idx <- min_dist_idx
    position_table$teammate_min_dist_x <- min_dist_x
    position_table$teammate_min_dist_y <- min_dist_y
    
    return(position_table)
    
}