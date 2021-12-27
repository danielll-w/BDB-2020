create_columns <- function(week1) {
    ######creating column
    #L1 - L2
    week1 <- week1 %>%
        mutate(L1L2Diff = L1OffY - L2OffY)
    
    #R1 - R2 (opposite--has to be R2-R1)
    week1 <- week1 %>%
        mutate(R1R2Diff = R2OffY - R1OffY)
    
    ##create bounds of field
    #left side
    week1 <- week1 %>% 
        mutate(LeftSideline = 53.33)
    
    #right side
    week1 <- week1 %>% 
        mutate(RightSideline = 0)
    
    ##drop obs where there's no L1 or R1
    week1 <- week1[!is.na(week1$L1OffY), ]
    week1 <- week1[!is.na(week1$R1OffY), ]
    
    
    ##Create column lines
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumnLine = case_when(is.na(L1L2Diff) ~ (L1OffY + BallSnapY + 4) * .5,
                                          L1L2Diff > 3 ~ L1OffY - 1.5,
                                          L1L2Diff <=3 ~ (L1OffY-L2OffY)*.75 + L2OffY))
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumnLine = case_when(is.na(R1R2Diff) ~ (R1OffY + BallSnapY - 4) * .5,
                                           R1R2Diff > 3 ~ R1OffY + 1.5,
                                           R1R2Diff <=3 ~ (R2OffY-R1OffY)*.25 + R1OffY))
    
    ##account for tight splits 
    #create indicator 
    #left
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(TightSplitLeft = case_when(L1OffY <= BallSnapY + 6 ~ 1,
                                          TRUE ~ 0))
    #right
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(TightSplitRight = case_when(R1OffY >= BallSnapY - 6 ~ 1,
                                           TRUE ~ 0))
    ##change column line accordingly                                   
    #left
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftColumnLine = case_when(TightSplitLeft==1 & L1L2Diff >=1 ~ L1OffY - .5,
                                          TRUE ~ LeftColumnLine))
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightColumnLine = case_when(TightSplitRight==1 & R1R2Diff >=1 ~ R1OffY + .5,
                                           TRUE ~ RightColumnLine))
    
    ##create column indicator
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(SnapY >= LeftColumnLine & SnapY < LeftSideline & OffDef=="Defense" ~ 1,
                                      TRUE ~ 0))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(SnapY <= RightColumnLine & SnapY > RightSideline & OffDef=="Defense" ~ 1,
                                       TRUE ~ 0))
    
    
    ##column count(all positions)  
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnCount = sum(LeftColumn)) 
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnCount = sum(RightColumn)) 
    
    
    ##account for when column player is a little more than a yard inside number 1 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumnLine = case_when(LeftColumnCount==0 & TightSplitLeft == 0 & L1L2Diff > 4 ~ L1OffY - 2.5,
                                          TRUE ~ LeftColumnLine))
    
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumnLine = case_when(RightColumnCount==0 & TightSplitRight == 0 & R1R2Diff > 4 ~ R1OffY + 2.5,
                                           TRUE ~ RightColumnLine))
    
    ##redo column indicator
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(SnapY >= LeftColumnLine & SnapY < LeftSideline & OffDef=="Defense" ~ 1,
                                      TRUE ~ LeftColumn))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(SnapY <= RightColumnLine & SnapY > RightSideline & OffDef=="Defense" ~ 1,
                                       TRUE ~ RightColumn))
    
    ###update column
    ##eliminate blitzers
    #left
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftColumn = case_when(SecondAndHalfX < BallSnapX ~ 0,
                                      TRUE ~ LeftColumn))
    #right
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightColumn = case_when(SecondAndHalfX < BallSnapX ~ 0,
                                       TRUE ~RightColumn))
    ##Eliminate DL in column
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(LeftColumn == 1 & position == 'DL' ~ 0,
                                      TRUE ~ LeftColumn))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(RightColumn == 1 & position == 'DL' ~ 0,
                                       TRUE ~ RightColumn))
    
    ##if there is a tight split, eliminate LBs 
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(L1OffY <= BallSnapY + 6 & DBPos =="LB" & LeftColumnCount > 1~ 0,
                                      TRUE ~ LeftColumn))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(R1OffY >= BallSnapY - 6 & DBPos =="LB" & RightColumnCount > 1 ~ 0,
                                       TRUE ~ RightColumn))
    
    #redo column count
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnCount = sum(LeftColumn))
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnCount = sum(RightColumn))
    
    ##count number of DBs in column (just DBs)
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnDBCount = sum(DBPos=="DB" & LeftColumn==1))
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnDBCount = sum(DBPos=="DB" & RightColumn==1))
}