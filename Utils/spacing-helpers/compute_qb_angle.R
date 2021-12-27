compute_qb_angle <- function(week1) {
    idx <- week1$position == 'QB' & !is.na(week1$position)
    
    o <- (90 - week1$o)*(pi/180)
    qb_angle <- (sin(o)*(week1$y[idx] - week1$y) + cos(o)*(week1$x[idx] - week1$x))/
        sqrt((week1$y[idx] - week1$y)^2 + (week1$x[idx] - week1$x)^2)
    qb_angle <- acos(qb_angle)
    qb_angle <- qb_angle*(180/pi)
    
    week1$qb_angle <- as.list(qb_angle)
    
    
    return(week1)
}