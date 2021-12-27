create_shell <- function(week1) {
    ####Finally creating the shell
    ###corner depth (depth of column defenders)
    #Left
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LeftCornerDepth = SnapX[match(1, LeftColumn)]) %>% 
        ungroup()
    
    #Right
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(RightCornerDepth = SnapX[match(1, RightColumn)]) %>% 
        ungroup()
    
    ##identify the shell
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(Shell = case_when(Window == 0 ~ '0',
                                 Window == 1 ~ '3',
                                 Window == 2 & LeftCornerDepth < BallSnapX + 6 & RightCornerDepth < BallSnapX + 6  ~ '2', 
                                 Window == 2 & LeftCornerDepth >= BallSnapX + 6 & RightCornerDepth < BallSnapX + 6  ~ '6',
                                 Window == 2 & LeftCornerDepth < BallSnapX + 6 & RightCornerDepth >= BallSnapX + 6  ~ '6',
                                 Window == 2 & LeftCornerDepth >= BallSnapX + 6 & RightCornerDepth >= BallSnapX + 6  ~ '4'))
    
    
    ##identify positions redo
    week1 <- week1 %>%
        mutate(GamePosition = case_when(LeftColumn == 1 ~ "Corner",
                                        RightColumn == 1 ~ "Corner",
                                        Highest == "A" ~ "Safety",
                                        Highest == "B" & Window == 2 ~ "Safety",
                                        Highest == "B" & Window == 1 ~ "SafetyTBD",
                                        Highest == "B" & Window == 0 ~ "Safety",
                                        LeftAlley == 1 & Highest == "-" ~ "Conflict",
                                        RightAlley == 1 & Highest == "-" ~ "Conflict",
                                        LeftAlleyLine > SnapY & SnapY > RightAlleyLine & Highest == "-" ~ "Adjacent"))
    #figure out SafetyTBD                                 
    week1 <- week1 %>%
        mutate(GamePosition = case_when(GamePosition == "SafetyTBD" & LeftAlley==1 ~ "SafetyConflict",
                                        GamePosition == "SafetyTBD" & RightAlley==1 ~ "SafetyConflict",
                                        GamePosition == "SafetyTBD" & LeftAlleyLine > SnapY & SnapY > RightAlleyLine ~ "SafetyAdjacent",
                                        TRUE ~ GamePosition))                                
    
    #get rid of offense and pass rushers                             
    week1 <- week1 %>%
        mutate(GamePosition = case_when(OffDef=="Offense" ~ "Offense",
                                        PassRusher=="PassRusher" ~ "PassRusher",
                                        TRUE ~ GamePosition))
    
    #delete useless columns
    week1 <- week1 %>%
        select(-time, -SideDuring, -qb_team, -QBTeam, -uniqueplay, -L1OffY, -L2OffY, -L3OffY, -L4OffY, -R1OffY,
               -R2OffY, -R3OffY, -R4OffY, -L1L2Diff, -R1R2Diff, -LeftSideline, -RightSideline, -TightSplitLeft, 
               -TightSplitRight, -LeftColumnDBCount, -RightColumnDBCount, -L2L3Diff, -R2R3Diff, -L2OffInsideOT, 
               -R2OffInsideOT, -LeftReceiverCount, -RightReceiverCount, -L1InsideTE, -L2InsideTE, -L3InsideTE,
               -L4InsideTE, -R1InsideTE, -R2InsideTE, -R3InsideTE, -R4InsideTE, -LeftReceiverCountOutsideTackle,
               -RightReceiverCountOutsideTackle, -LeftAlleyID, -RightAlleyID, -LeftColumnHighest, -RightColumnHighest,
               -LeftColumnChute, -RightColumnChute, -LeftColumnSafety, -RightColumnSafety, -LeftColumnWidest,
               -RightColumnWidest, -NewLeftColumnCount, -LeftShield, -LeftOffColumnCount, -LeftOffAlleyCount,
               -LeftOffShield, -LeftDBDiff, -NewRightColumnCount, -RightShield, -RightOffColumnCount, 
               -RightOffAlleyCount, -RightOffShield, -RightDBDiff)
}