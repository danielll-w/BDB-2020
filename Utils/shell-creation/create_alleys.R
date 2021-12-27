create_alleys <- function(week1) {
    ######alley 
    #L2 - L3
    week1 <- week1 %>%
        mutate(L2L3Diff = L2OffY - L3OffY)
    
    #R2 - R3 (opposite--has to be R3-R2)
    week1 <- week1 %>%
        mutate(R2R3Diff = R3OffY - R2OffY)
    
    ##is #2 inside tackle?
    #left
    week1 <- week1 %>%
        mutate(L2OffInsideOT = case_when(is.na(L2OffY) ~ 0,
                                         L2OffY < (BallSnapY + 4) ~ 1,
                                         L2OffY >= (BallSnapY + 4) ~ 0))
    #right
    week1 <- week1 %>%
        mutate(R2OffInsideOT = case_when(is.na(R2OffY) ~ 0,
                                         R2OffY < (BallSnapY - 4) ~ 1,
                                         R2OffY >= (BallSnapY - 4) ~ 0))
    
    #what number is the QB
    week1 <- week1 %>%
        mutate(ReceiverNumber  = case_when(ReceiverNumber == "L2Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "L3Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "L4Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "L5Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "L6Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "R2Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "R3Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "R4Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "R5Offense" & isBackfieldQB == 1 ~ "QB",
                                           ReceiverNumber == "R6Offense" & isBackfieldQB == 1 ~ "QB",
                                           TRUE ~ ReceiverNumber))
    
    
    #Receiver count by side 
    week1$LeftReceiverCount <- rowSums(!is.na(week1[c('L1OffY', 'L2OffY', 'L3OffY', "L4OffY")]))
    week1$RightReceiverCount <- rowSums(!is.na(week1[c('R1OffY', 'R2OffY', 'R3OffY', "R4OffY")]))
    
    ##ID players inside tight end
    #Left
    #L1 inside tight?
    week1 <- week1 %>%
        mutate(L1InsideTE = case_when(L1OffY <= (BallSnapY + 4) ~ 1,
                                      L1OffY > (BallSnapY + 4) ~ 0))
    #L2 inside tight?
    week1 <- week1 %>%
        mutate(L2InsideTE = case_when(is.na(L2OffY) ~ 0,
                                      L2OffY <= (BallSnapY + 4) ~ 1,
                                      L2OffY > (BallSnapY + 4) ~ 0))
    #L3 inside tight?
    week1 <- week1 %>%
        mutate(L3InsideTE = case_when(is.na(L2OffY) ~ 0,
                                      is.na(L3OffY) ~ 0,
                                      L3OffY <= (BallSnapY + 4) ~ 1,
                                      L3OffY > (BallSnapY + 4) ~ 0))
    #L4 inside tight?
    week1 <- week1 %>%
        mutate(L4InsideTE = case_when(is.na(L2OffY) ~ 0,
                                      is.na(L3OffY) ~ 0,
                                      is.na(L4OffY) ~ 0,
                                      L4OffY <= (BallSnapY + 4) ~ 1,
                                      L4OffY > (BallSnapY + 4) ~ 0))
    #Right
    #R1 inside tight?
    week1 <- week1 %>%
        mutate(R1InsideTE = case_when(R1OffY >= (BallSnapY - 4) ~ 1,
                                      R1OffY < (BallSnapY - 4) ~ 0))
    
    #R2 inside tight?
    week1 <- week1 %>%
        mutate(R2InsideTE = case_when(is.na(R2OffY) ~ 0,
                                      R2OffY >= (BallSnapY - 4) ~ 1,
                                      R2OffY < (BallSnapY - 4) ~ 0))
    
    #R3 inside tight?
    week1 <- week1 %>%
        mutate(R3InsideTE = case_when(is.na(R2OffY) ~ 0,
                                      is.na(R3OffY) ~ 0,
                                      R3OffY >= (BallSnapY - 4) ~ 1,
                                      R3OffY < (BallSnapY - 4) ~ 0))
    
    #R4 inside tight?
    week1 <- week1 %>%
        mutate(R4InsideTE = case_when(is.na(R2OffY) ~ 0,
                                      is.na(R3OffY) ~ 0,
                                      is.na(R4OffY) ~ 0,
                                      R4OffY >= (BallSnapY - 4) ~ 1,
                                      R4OffY < (BallSnapY - 4) ~ 0))
    
    #drop plays where number 1 is inside tackle
    week1 <- filter(week1, L1InsideTE != 1)
    week1 <- filter(week1, R1InsideTE != 1)
    
    
    
    ###subtract count if inside tackle
    #Left
    #L4
    week1 <- week1 %>%
        mutate(LeftReceiverCountOutsideTackle = LeftReceiverCount - L4InsideTE)
    #L3
    week1$LeftReceiverCountOutsideTackle <- week1$LeftReceiverCountOutsideTackle - week1$L3InsideTE
    #L2
    week1$LeftReceiverCountOutsideTackle <- week1$LeftReceiverCountOutsideTackle - week1$L2InsideTE
    #L1
    week1$LeftReceiverCountOutsideTackle <- week1$LeftReceiverCountOutsideTackle - week1$L1InsideTE
    #Right
    #R4
    week1 <- week1 %>%
        mutate(RightReceiverCountOutsideTackle = RightReceiverCount - R4InsideTE)
    #R3
    week1$RightReceiverCountOutsideTackle <- week1$RightReceiverCountOutsideTackle - week1$R3InsideTE
    #R2
    week1$RightReceiverCountOutsideTackle <- week1$RightReceiverCountOutsideTackle - week1$R2InsideTE
    #R1
    week1$RightReceiverCountOutsideTackle <- week1$RightReceiverCountOutsideTackle - week1$R1InsideTE
    
    
    
    #Create indicator if first player outside is outside 5 yards from ball laterally
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftAlleyID = case_when(LeftReceiverCountOutsideTackle == 1 & L1OffY > (BallSnapY + 5) ~ 'L1Plus5',
                                       LeftReceiverCountOutsideTackle == 1 & L1OffY <= (BallSnapY + 5) ~ 'L1Minus5',
                                       LeftReceiverCountOutsideTackle == 2 & L2OffY > (BallSnapY + 5) ~ 'L2Plus5',
                                       LeftReceiverCountOutsideTackle == 2 & L2OffY <= (BallSnapY + 5) ~ 'L2Minus5',
                                       LeftReceiverCountOutsideTackle == 3 & L3OffY > (BallSnapY + 5) ~ 'L3Plus5',
                                       LeftReceiverCountOutsideTackle == 3 & L3OffY <= (BallSnapY + 5) ~ 'L3Minus5',
                                       LeftReceiverCountOutsideTackle == 4 & L3OffY > (BallSnapY + 5) ~ 'L4Plus5',
                                       LeftReceiverCountOutsideTackle == 4 & L3OffY <= (BallSnapY + 5) ~ 'L4Minus5'))
    
    #right #NOTE--"plus 5" just means that the receiver is more than five yards from the ball 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightAlleyID = case_when(RightReceiverCountOutsideTackle == 1 & R1OffY < (BallSnapY - 5) ~ 'R1Plus5',
                                        RightReceiverCountOutsideTackle == 1 & R1OffY >= (BallSnapY - 5) ~ 'R1Minus5',
                                        RightReceiverCountOutsideTackle == 2 & R2OffY < (BallSnapY - 5) ~ 'R2Plus5',
                                        RightReceiverCountOutsideTackle == 2 & R2OffY >= (BallSnapY - 5) ~ 'R2Minus5',
                                        RightReceiverCountOutsideTackle == 3 & R3OffY < (BallSnapY - 5) ~ 'R3Plus5',
                                        RightReceiverCountOutsideTackle == 3 & R3OffY >= (BallSnapY - 5) ~ 'R3Minus5',
                                        RightReceiverCountOutsideTackle == 4 & R3OffY < (BallSnapY - 5) ~ 'R4Plus5',
                                        RightReceiverCountOutsideTackle == 4 & R3OffY >= (BallSnapY - 5) ~ 'R4Minus5'))
    
    ##Create alley lines
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftAlleyLine = case_when(LeftAlleyID == 'L1Plus5' ~ BallSnapY + 5,
                                         LeftAlleyID == 'L1Minus5' ~ (L1OffY + BallSnapY + 4)* .5,
                                         LeftAlleyID == 'L2Plus5' ~ BallSnapY + 5,
                                         LeftAlleyID == 'L2Minus5' ~ (L2OffY + (BallSnapY + 4))*.5,
                                         LeftAlleyID == 'L3Plus5' ~ BallSnapY + 5,
                                         LeftAlleyID == 'L3Minus5' ~ (L3OffY + (BallSnapY + 4))*.5,
                                         LeftAlleyID == 'L4Plus5' ~ BallSnapY + 5,
                                         LeftAlleyID == 'L4Minus5' ~ (L4OffY + (BallSnapY + 4))*.5))
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightAlleyLine = case_when(RightAlleyID == 'R1Plus5' ~ BallSnapY - 5,
                                          RightAlleyID == 'R1Minus5' ~ (R1OffY + (BallSnapY - 4))* .5,
                                          RightAlleyID == 'R2Plus5' ~ BallSnapY - 5,
                                          RightAlleyID == 'R2Minus5' ~ (R2OffY + (BallSnapY - 4))*.5,
                                          RightAlleyID == 'R3Plus5' ~ BallSnapY - 5,
                                          RightAlleyID == 'R3Minus5' ~ (R3OffY + (BallSnapY - 4))*.5,
                                          RightAlleyID == 'R4Plus5' ~ BallSnapY - 5,
                                          RightAlleyID == 'R4Minus5' ~ (R4OffY + (BallSnapY - 4))*.5))
    
    
    #replace obs where alley line is wider than L1 or L2  
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftAlleyLine = case_when(LeftAlleyLine > LeftColumnLine ~ LeftColumnLine,
                                         TRUE ~ LeftAlleyLine))
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightAlleyLine = case_when(RightAlleyLine < RightColumnLine ~ RightColumnLine,
                                          TRUE ~ RightAlleyLine))                                    
    
    #create alley indicator
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftAlley = case_when(SnapY < LeftColumnLine & SnapY >= LeftAlleyLine & OffDef=="Defense" ~ 1,
                                     TRUE ~ 0))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightAlley = case_when(SnapY > RightColumnLine & SnapY <= RightAlleyLine & OffDef == "Defense" ~ 1,
                                      TRUE ~ 0))
    
}