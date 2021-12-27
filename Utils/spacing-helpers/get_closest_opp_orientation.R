get_closest_opp_orientation <- function(position_table) {
    
    closest_opp_orientation <- vector(mode = "double", length = nrow(position_table))
    
    for (i in 1:nrow(position_table)) {
        x <- position_table$x[i]
        y <- position_table$y[i]
        dir <- position_table$dir[i]
        team <- position_table$team[i]
        
        if (team == "football") {
            next()
        }
        
        idx <- position_table$team != team & position_table$team != "football"
        closest_opp_orientation_idx = which.min(sqrt((x - position_table$x[idx])^2 + (y - position_table$y[idx])^2))
        closest_opp_orientation[i] = position_table$dir[closest_opp_orientation_idx]
        
    }
    
    position_table$closest_opp_orientation <- closest_opp_orientation
    
    return(position_table)
    
}