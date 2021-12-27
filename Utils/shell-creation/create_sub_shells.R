create_sub_shells <- function(week1) {
    
    ##some feature gen to help create labels
    #depth of corners 
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftCornerDeep = case_when(LeftCornerDepth - BallSnapX >= 6 ~ "Yes",
                                          LeftCornerDepth - BallSnapX < 6 ~ "No"))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightCornerDeep = case_when(RightCornerDepth - BallSnapX >= 6 ~ "Yes",
                                           RightCornerDepth - BallSnapX < 6 ~ "No"))
    
    #side 
    week1 <- week1 %>%
        mutate(Side= case_when(SnapY >= BallSnapY ~ "Left",
                               SnapY < BallSnapY ~ "Right"))
    
    #Concatenate positions and sides
    week1 <- week1 %>%
        mutate(GamePositionSide = paste0(Side, GamePosition))
    
    #is corner zone?
    #match the zone column
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftCornerZone = zone[match("LeftCorner", GamePositionSide)]) %>% 
        ungroup
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightCornerZone = zone[match("RightCorner", GamePositionSide)]) %>% 
        ungroup
    
    week1 <- week1 %>% 
        mutate(LeftCornerZone = case_when(LeftCornerZone == 0 ~ "Man",
                                          LeftCornerZone == 1 ~ "Zone"))
    
    week1 <- week1 %>% 
        mutate(RightCornerZone = case_when(RightCornerZone == 0 ~ "Man",
                                           RightCornerZone == 1 ~ "Zone"))
    
    #number of receivers
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(ReceiverCount = sum(SecondAndHalfX > BallSnapX & OffDef == 'Offense'))
    
    ########coverage identification
    ######single high
    #corners in man?
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Window == 1 & cornerMan == 0 ~ "Cover 3",
                                  Window == 1 & cornerMan == 1 ~ "3 Lock",
                                  Window == 1 & cornerMan == 2 ~ "Cover 1"))
    
    #number of rushers
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage == "Cover 3" & CoverageDefenders==6 ~ "Cover 33",
                                  Coverage == "Cover 3" & CoverageDefenders==7 ~ "Cover 3",
                                  Coverage == "Cover 3" & CoverageDefenders==8 ~ "Cover 38",
                                  Coverage == "Cover 3 Lock" & CoverageDefenders==6 ~ "Cover 33 Lock",
                                  Coverage == "3 Lock" & CoverageDefenders==7 ~ "3 Lock",
                                  Coverage == "3 Lock" & CoverageDefenders==8 ~ "Cover 38 Lock",
                                  Coverage == "Cover 1" & CoverageDefenders==8 ~ "Cover 38 Man",
                                  Coverage == "Cover 1" & CoverageDefenders==7 ~ "Cover 1",
                                  Coverage == "Cover 1" & CoverageDefenders==6 ~ "Cover 1",
                                  Coverage == "Cover 1" & CoverageDefenders==5 ~ "Cover 1", 
                                  TRUE ~ Coverage))
    
    
    #####two high  
    ##both corners in man  
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Window == 2 & cornerMan == 0 ~ "SplitBothZone",
                                  Window == 2 & cornerMan == 1 ~ "SplitOneMan",
                                  Window == 2 & cornerMan == 2 ~ "SplitBothMan", 
                                  TRUE ~ Coverage))                      
    
    ##both man?
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage == "SplitBothMan" ~ "Cover 5", 
                                  TRUE ~ Coverage))
    
    ##one man, one zone 
    #figure out which side the man side is then examine the depth of the corner on the zone side
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage == "SplitOneMan" & LeftCornerZone == "Man" & RightCornerDeep == "Yes" ~ "4 Lock",
                                  Coverage == "SplitOneMan" & LeftCornerZone == "Man" & RightCornerDeep == "No" ~ "2 Lock",
                                  Coverage == "SplitOneMan" & RightCornerZone == "Man" & LeftCornerDeep == "Yes" ~ "4 Lock",
                                  Coverage == "SplitOneMan" & RightCornerZone == "Man" & LeftCornerDeep == "No" ~ "2 Lock", 
                                  TRUE ~ Coverage))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage =="4 Lock" & CoverageDefenders == 8 ~ "48 Lock",
                                  Coverage =="4 Lock" & CoverageDefenders == 7 ~ "4 Lock",
                                  Coverage =="2 Lock" & CoverageDefenders == 8 ~ "28 Lock",
                                  Coverage =="2 Lock" & CoverageDefenders == 7 ~ "2 Lock",
                                  Coverage =="4 Lock" & CoverageDefenders == 6 ~ "24 Lock",
                                  Coverage =="2 Lock" & CoverageDefenders == 6 ~ "42 Lock", 
                                  TRUE ~ Coverage))
    
    ##both corners in zone
    #determine which corner is high and which is low
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage == "SplitBothZone" & LeftCornerDeep == "Yes" & RightCornerDeep == "Yes" ~ "Cover 4",
                                  Coverage == "SplitBothZone" & LeftCornerDeep == "No" & RightCornerDeep == "Yes" ~ "Cover 6",
                                  Coverage == "SplitBothZone" & LeftCornerDeep == "Yes" & RightCornerDeep == "No" ~ "Cover 6",
                                  Coverage == "SplitBothZone" & LeftCornerDeep == "No" & RightCornerDeep == "No" ~ "Cover 2", 
                                  TRUE ~ Coverage))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Coverage =="Cover 4" & CoverageDefenders == 8 ~ "Cover 48",
                                  Coverage =="Cover 4" & CoverageDefenders == 7 ~ "Cover 4",
                                  Coverage =="Cover 4" & CoverageDefenders == 6 ~ "Cover 24",
                                  Coverage =="Cover 6" & CoverageDefenders == 8 ~ "Cover 68",
                                  Coverage =="Cover 6" & CoverageDefenders == 7 ~ "Cover 6",
                                  Coverage =="Cover 6" & CoverageDefenders == 6 ~ "Cover 66",
                                  Coverage =="Cover 2" & CoverageDefenders == 8 ~ "Cover 28",
                                  Coverage =="Cover 2" & CoverageDefenders == 7 ~ "Cover 2",
                                  Coverage =="Cover 2" & CoverageDefenders == 6 ~ "Cover 42", 
                                  TRUE ~ Coverage))
    
    ##Zero high
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(Window == 0 ~ "Cover 0",
                                  TRUE ~ Coverage))                    
    
    #heavy blitzes
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(CoverageDefenders <= 5 ~ "Cover 0",
                                  TRUE ~ Coverage)) 
    
    #six man rush
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Coverage=case_when(CoverageDefenders == 5 & ReceiverCount <= 4  & Window == 1 ~ "Cover 1",
                                  CoverageDefenders == 5 & ReceiverCount <= 4  & Window != 1 ~ "Cover 0",
                                  CoverageDefenders == 5 & ReceiverCount == 5 ~ "Cover 0",
                                  TRUE ~ Coverage))
    
    #Coverage Family
    week1 <- week1 %>%
        mutate(CoverageFamily = case_when(Coverage == "Cover 0" ~ "Cover 0",
                                          Coverage == "Cover 1" ~ "Cover 1",
                                          Coverage == "Cover 2" ~ "Cover 2",
                                          Coverage == "2 Lock" ~ "Cover 2",
                                          Coverage == "Cover 28" ~ "Cover 2",
                                          Coverage == "28 Lock" ~ "Cover 2",
                                          Coverage == "Cover 42" ~ "Cover 2",
                                          Coverage == "42 Lock" ~ "Cover 2",
                                          Coverage == "Cover 3" ~ "Cover 3",
                                          Coverage == "3 Lock" ~ "Cover 3",
                                          Coverage == "Cover 33" ~ "Cover 3",
                                          Coverage == "Cover 33 Lock" ~ "Cover 3",
                                          Coverage == "Cover 38" ~ "Cover 3",
                                          Coverage == "Cover 38 Lock" ~ "Cover 3",
                                          Coverage == "Cover 38 Man" ~ "Cover 3",
                                          Coverage == "Cover 4" ~ "Cover 4",
                                          Coverage == "4 Lock" ~ "Cover 4",
                                          Coverage == "Cover 24" ~ "Cover 4",
                                          Coverage == "24 Lock" ~ "Cover 4",
                                          Coverage == "Cover 48" ~ "Cover 4",
                                          Coverage == "48 Lock" ~ "Cover 4",
                                          Coverage == "Cover 5" ~ "Cover 5",
                                          Coverage == "Cover 6" ~ "Cover 6",
                                          Coverage == "Cover 66" ~ "Cover 6",
                                          Coverage == "Cover 68" ~ "Cover 6"))
    
    week1 <- week1 %>%
        mutate(Window= case_when(Coverage == "Cover 0" ~ 0,
                                 TRUE ~ Window))
    
    week1 <- week1 %>%
        mutate(Shell = case_when(Coverage == "Cover 0" ~ "0",
                                 TRUE ~ Shell))

}