compute_nearest_opponent_angle <- function(week1) {
    
    angle <- vector(mode = "double", length = nrow(week1))
    
    o <- (90 - week1$o)*(pi/180)
    
    for (i in 1:nrow(week1)) {
        idx <- week1$nflId == week1$opp_min_dist_idx[i] & !is.na(week1$nflId)
        
        if (any(idx)){
            angle[i] <- (sin(o[i])*(week1$y[idx] - week1$y[i]) + cos(o[i])*(week1$x[idx] - week1$x[i]))/
                sqrt((week1$y[idx] - week1$y[i])^2 + (week1$x[idx] - week1$x[i])^2)
            angle[i] <- acos(angle[i])
            angle[i] <- angle[i]*(180/pi)
        } else {
            angle[i] = NA
        }
    }
    
    week1$nearest_opponent_angle <- angle
    
    return(week1)
}