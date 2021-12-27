create_chute <- function(week1) {
    ###identify the chute
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(Chute = case_when(SnapY < LeftColumnLine & SnapY > RightColumnLine & OffDef == "Defense" ~ 1,
                                 TRUE ~ 0))
    
    #indicate highest DB in column
    #left
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, LeftColumn != 1, DBPos != 'DB', desc(SnapX)) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnHighest = c(1, rep(0, n() - 1)))
    
    #right
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, RightColumn != 1, DBPos != 'DB', desc(SnapX)) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnHighest = c(1, rep(0, n() - 1)))
    
    
    ##include safeties in the column
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(Chute = case_when(LeftColumnDBCount==2 & LeftColumnHighest ==1 & SnapX > BallSnapX + 7 ~ 1,
                                 TRUE ~ Chute))
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(Chute = case_when(RightColumnDBCount==2 & RightColumnHighest ==1 & SnapX > BallSnapX + 7 ~ 1,
                                 TRUE ~ Chute))
    
    #name these players "columnchutes"
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumnChute = case_when(LeftColumn == 1 & Chute == 1 ~ 1,
                                           TRUE ~ 0))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumnChute = case_when(RightColumn==1 & Chute ==1 ~ 1,
                                            TRUE ~ 0))
    
}