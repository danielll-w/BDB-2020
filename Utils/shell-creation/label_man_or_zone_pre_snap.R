label_man_or_zone_pre_snap <- function(week1) {
  # Label Man or Zone (all based off at snap) 
  
  # Split at the snap
  week1_at_snap <- week1 %>% filter(event == 'ball_snap')
  
  # Split 0.5 seconds after the snap
  week1_at_snap_plus_t <- week1 %>% filter(FramesSinceSnap == 5)
  
  # Initialize variable that is 1 if zone, 0 if man, -1 if offense or pass rusher, -2 if indeterminate
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(position %in% c('WR', 'QB', 'RB', 'TE', 'FB') | PassRusher == 'PassRusher'~ -1,
                            TRUE ~ -2))
  
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>% 
    mutate(zone = case_when(position %in% c('WR', 'QB', 'RB', 'TE', 'FB') ~ -1,
                            TRUE ~ -2))
  
  # Get distance of each player to nearest opponent
  week1_at_snap <- week1_at_snap %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>%  
    lapply(get_min_opp_dist) %>% 
    bind_rows()
  
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>%  
    lapply(get_min_opp_dist) %>% 
    bind_rows()
  
  # Get distance of each player to nearest teammate
  week1_at_snap <- week1_at_snap %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>%  
    lapply(get_min_teammate_dist) %>% 
    bind_rows()
  
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>%  
    lapply(get_min_teammate_dist) %>% 
    bind_rows()
  
  # Create angle between player's eyes and QB
  week1_at_snap <- week1_at_snap %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>% 
    lapply(compute_qb_angle) %>% 
    bind_rows()
  
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>% 
    lapply(compute_qb_angle) %>% 
    bind_rows()
  
  # Create angle between player's eyes and nearest opponent
  week1_at_snap <- week1_at_snap %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>% 
    lapply(compute_nearest_opponent_angle) %>% 
    bind_rows()
  
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>%
    group_by(gameId, playId, frameId) %>% 
    group_split() %>% 
    lapply(compute_nearest_opponent_angle) %>% 
    bind_rows()
  
  
  # Add qb angle and opponent angle in future to 'at snap' table
  week1_at_snap_plus_t$qb_angle_plus_t <- week1_at_snap_plus_t$qb_angle
  week1_at_snap_plus_t$nearest_opponent_angle_plus_t <- week1_at_snap_plus_t$nearest_opponent_angle
  week1_at_snap_plus_t <- week1_at_snap_plus_t %>% select(gameId, playId, nflId, qb_angle_plus_t, nearest_opponent_angle_plus_t)
  week1_at_snap <- left_join(week1_at_snap, week1_at_snap_plus_t)
  
  # Safeties
  
  #If safety is high, they are playing zone (use highest variable to determine which players are safeties)
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Safety' ~ 1,
                            TRUE ~ zone))
  
  #If safety is low and is looking towards QB and not defender (by a large enough margin), then he is in zone and vice versa for man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD') & qb_angle_plus_t <= nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 1,
                            GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD') & qb_angle_plus_t > nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 0,
                            TRUE ~ zone))
  
  #If QB angle and nearest opponent angle are nearly the same, then default to zone
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD') & abs(as.numeric(qb_angle_plus_t) - as.numeric(nearest_opponent_angle_plus_t)) < 10 ~ 1,
                            TRUE ~ zone))
  
  # Corners
  
  # If corner is back >= 9 yards from LOS, he has to be playing zone
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Corner' & LeftColumn == 1 & (LeftCornerDepth - BallSnapX) >= 9 ~ 1,
                            GamePosition == 'Corner' & RightColumn == 1 & (RightCornerDepth - BallSnapX) >= 9 ~ 1,
                            TRUE ~ zone))
  
  #If corner is in press coverage, he is playing man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Corner' & LeftColumn == 1 & (LeftCornerDepth - opp_min_dist_x) <= 3 ~ 0,
                            GamePosition == 'Corner' & RightColumn == 1 & (RightCornerDepth - opp_min_dist_x) <= 3 ~ 0,
                            TRUE ~ zone))
  
  #If corner is laterally more than 10 yards from nearest teammate, must be playing man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Corner' & LeftColumn == 1 & (y - teammate_min_dist_y) >= 10 ~ 0,
                            GamePosition == 'Corner' & RightColumn == 1 & (y - teammate_min_dist_y) >= 10 ~ 0,
                            TRUE ~ zone))
  
  #If corner is looking towards QB and not defender (by a large enough margin), then he is in zone and vice versa for man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Corner' & LeftColumn == 1 & qb_angle_plus_t <= nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 1,
                            GamePosition == 'Corner' & RightColumn == 1 & qb_angle_plus_t <= nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 1,
                            GamePosition == 'Corner' & LeftColumn == 1 & qb_angle_plus_t > nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 0,
                            GamePosition == 'Corner' & RightColumn == 1 & qb_angle_plus_t > nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 0,
                            TRUE ~ zone))
  
  #If QB angle and nearest opponent angle are nearly the same, then default to zone
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(GamePosition == 'Corner' & LeftColumn == 1 & abs(as.numeric(qb_angle_plus_t) - as.numeric(nearest_opponent_angle_plus_t)) < 10 ~ 1,
                            GamePosition == 'Corner' & RightColumn == 1 & abs(as.numeric(qb_angle_plus_t) - as.numeric(nearest_opponent_angle_plus_t)) < 10 ~ 1,
                            TRUE ~ zone))
  
  #Other DB's labeled as non-safety and non-corner
 
  # If corner is back >= 9 yards from LOS, he has to be playing zone
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & (x - BallSnapX) >= 9 ~ 1,
                            ((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & (x - BallSnapX) >= 9 ~ 1,
                            TRUE ~ zone))
  
  #If corner is in press coverage, he is playing man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety'& DBPos == 'DB') & (x - opp_min_dist_x) <= 3 ~ 0,
                            ((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & (x - opp_min_dist_x) <= 3 ~ 0,
                            TRUE ~ zone))
  
  #If corner is laterally more than 10 yards from nearest teammate, must be playing man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & (y - teammate_min_dist_y) >= 10 ~ 0,
                            ((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & (y - teammate_min_dist_y) >= 10 ~ 0,
                            TRUE ~ zone))
  
  #If corner is looking towards QB and not defender (by a large enough margin), then he is in zone and vice versa for man
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & qb_angle_plus_t <= nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 1,
                            ((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & qb_angle_plus_t > nearest_opponent_angle_plus_t & (zone != 0 | zone != 1) ~ 0,
                            TRUE ~ zone))
  
  #If QB angle and nearest opponent angle are nearly the same, then default to zone
  week1_at_snap <- week1_at_snap %>% 
    mutate(zone = case_when(((GamePosition != 'Corner' | is.na(GamePosition)) & !(GamePosition %in% c('SafetyConflict', 'SafetyAdjacent', 'SafetyTBD')) & GamePosition != 'Safety' & DBPos == 'DB') & abs(as.numeric(qb_angle_plus_t) - as.numeric(nearest_opponent_angle_plus_t)) < 10 ~ 1,
                            TRUE ~ zone))
  
  #Concatenate shell and number of DB's in man and zone
  week1_at_snap <- week1_at_snap %>% 
    group_by(gameId, playId) %>% 
    mutate(nonCornerSafetyDBMan = sum(((GamePosition != 'Corner' | is.na(GamePosition)) & Highest != 'A' & Highest != 'B' & DBPos == 'DB') & zone == 0, na.rm = TRUE),
           nonCornerSafetyDBZone = sum(((GamePosition != 'Corner' | is.na(GamePosition)) & Highest != 'A' & Highest != 'B' & DBPos == 'DB') & zone == 1, na.rm = TRUE),
           cornerMan = sum(GamePosition == 'Corner' & zone == 0),
           cornerZone = sum(GamePosition == 'Corner' & zone == 1),
           safetyMan = sum((Highest == 'A' | Highest == 'B') & zone == 0),
           safetyZone = sum((Highest == 'A' | Highest == 'B') & zone == 1),
           totalDBMan = sum(DBPos == 'DB' & zone == 0),
           totalDBZone = sum(DBPos == 'DB' & zone == 1))
  
  
}