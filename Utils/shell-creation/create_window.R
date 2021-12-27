create_window <- function(week1) {
    #####defining the window
    ##define two highest DBs (need to make it just DBs, need to make it just players in the chute)
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, Chute != 1,DBPos != 'DB', desc(SnapX)) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(Highest = c('A','B', rep('-', n() - 2)))
    
    #new columns for higher DB
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
        ungroup
    
    #new column for lower DB
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
        ungroup
    
    #test
    datswitch <- filter(week1, LowDBX>HighDBX)
    
    #ok just need to switch A and B if the safeties switch depth by .8 seconds
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(Highest = case_when(Highest =="A" & LowDBX > HighDBX ~ "B",
                                   Highest =="B" & LowDBX > HighDBX ~ "A", 
                                   TRUE ~ Highest))
    
    ##now just redo the eight tenths x and y columns (to account for those observations where the safeties flipped depth)
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
        ungroup
    
    #new column for lower DB
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
        ungroup
    
    #test
    datswitch <- filter(week1, LowDBX>HighDBX)
    
    #create channel safety (is there a middle safety on the play?)
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(ChannelSafety = case_when(HighDBY < LeftAlleyLine & HighDBY > RightAlleyLine ~ 1,
                                         TRUE ~ 0))
    
    ##determine if these are column-safeties or just high corners
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumnSafety = case_when(LeftColumnChute == 1  & ChannelSafety == 1 ~ 0,
                                            LeftColumnChute == 1 & Highest=="A" & ChannelSafety == 0 ~ 1,
                                            LeftColumnChute == 1 & Highest=="B" & ChannelSafety == 0 ~ 1))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumnSafety = case_when(RightColumnChute == 1  & ChannelSafety == 1 ~ 0,
                                             RightColumnChute == 1 & Highest=="A" & ChannelSafety == 0 ~ 1,
                                             RightColumnChute == 1 & Highest=="B" & ChannelSafety == 0 ~ 1))
    
    ##eliminate these column-safeties from columns
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(LeftColumnSafety == 1 ~ 0,
                                      TRUE ~ LeftColumn))
    
    #Right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(RightColumnSafety == 1 ~ 0,
                                       TRUE ~ RightColumn))
    
    #remove columnchute corners from the chute 
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(Chute = case_when(LeftColumnChute ==1 & LeftColumnSafety == 0 ~ 0,
                                 TRUE ~ Chute))
    
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(Chute = case_when(RightColumnChute==1 & RightColumnSafety == 0 ~ 0,
                                 TRUE ~ Chute))
    
    #redo highest player ID--now that we've eliminated column corners from the chute they can't be considered safeties (but column safeties can)
    ##define two highest DBs (need to make it just DBs, need to make it just players in the chute)
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, Chute != 1,DBPos != 'DB', desc(SnapX)) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(Highest = c('A','B', rep('-', n() - 2)))
    
    #create new columns for higher DB
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBY = EightTenthsY[match('A', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(HighDBX = EightTenthsX[match('A', Highest)]) %>%
        ungroup
    
    #new column for lower DB
    #Y
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBY = EightTenthsY[match('B', Highest)]) %>%
        ungroup
    #X
    week1 <- week1 %>%
        group_by(gameId, playId) %>% 
        mutate(LowDBX = EightTenthsX[match('B', Highest)]) %>%
        ungroup
    #count column again
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnCount = sum(LeftColumn))
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnCount = sum(RightColumn))
    
    dat2leftcolumn <- filter(week1, LeftColumnCount==2)
    dat2rightcolumn <- filter(week1, RightColumnCount==2)
    
    
    ###now if there are still 2 in a column, make the column defender the widest guy 
    ##create the indicator
    #left
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, LeftColumn != 1, desc(SnapY)) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnWidest = c(1, rep(0, n() - 1)))
    
    #right
    week1 <- week1 %>%
        arrange(gameId, playId, frameId, RightColumn != 1, SnapY) %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnWidest = c(1, rep(0, n() - 1)))
    
    ##now rename the inner player a conflict defender. In the next step we will remove his column label.
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftAlley = case_when(LeftColumn==1 & LeftColumnCount ==2 & LeftColumnWidest==0 ~ 1,
                                     TRUE ~ LeftAlley))
    
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightAlley = case_when(RightColumn==1 & RightColumnCount ==2 & RightColumnWidest==0 ~ 1,
                                      TRUE ~ RightAlley))
    
    ##in the remaining observations with two players in column, make the widest DB the column player--set all others to 0
    #left
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(LeftColumn = case_when(LeftColumnCount==2 & LeftColumnWidest==1 ~ 1,
                                      LeftColumnCount==2 & LeftColumnWidest==0 ~ 0,
                                      TRUE ~ LeftColumn))
    #right
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(RightColumn = case_when(RightColumnCount==2 & RightColumnWidest==1 ~ 1,
                                       RightColumnCount==2 & RightColumnWidest==0 ~ 0,
                                       TRUE ~ RightColumn))
    ##count again
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftAlleyCount = sum(LeftAlley)) 
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightAlleyCount = sum(RightAlley)) 
    
    datleftalley <- filter(week1, LeftAlley == 1)
    datrightalley <- filter(week1, RightAlley == 1)
    
    #count column again
    #left
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftColumnCount = sum(LeftColumn))
    
    #right
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightColumnCount = sum(RightColumn))
    
    #test
    dat2leftcolumn <- filter(week1, LeftColumnCount==2)
    dat2rightcolumn <- filter(week1, RightColumnCount==2)
    
    #delete three in column (it's only one play each)
    week1 <- filter(week1, RightColumnCount != 3)
    week1 <- filter(week1, LeftColumnCount != 3)
    
    #delete none in column (it's only about 10 plays each)
    week1 <- filter(week1, RightColumnCount != 0)
    week1 <- filter(week1, LeftColumnCount != 0)
    
    #test
    dat3leftcolumn <- filter(week1, LeftColumnCount==3)
    dat2leftcolumn <- filter(week1, LeftColumnCount==2)
    week1leftcolumn <- filter(week1, LeftColumnCount==1)
    dat0leftcolumn <- filter(week1, LeftColumnCount==0)
    
    dat3rightcolumn <- filter(week1,RightColumnCount==3)
    dat2rightcolumn <- filter(week1, RightColumnCount==2)
    week1rightcolumn <- filter(week1, RightColumnCount==1)
    dat0rightcolumn <- filter(week1, RightColumnCount==0)
    
    datchute <- filter(week1, Chute==1)
    datleftcolumn <- filter(week1, LeftColumn==1)
    datrightcolumn <- filter(week1, RightColumn==1)
    
    dat2leftcolumn <- filter(dat2leftcolumn, LeftColumn==1)
    dat2leftcolumn <- filter(dat2leftcolumn, event=="ball_snap")
    dat2leftcolumn$disfromlos <- dat2leftcolumn$SnapX - dat2leftcolumn$BallSnapX
    
    
    #midpoint of safety x values
    week1$SafetyPointX <- week1$LowDBX
    week1$SafetyPointY <- week1$HighDBY
    ##maybe do a case when for the y value. Maybe use bottom sideline when right safety is higher etc.
    
    ###Creating the contours
    ##create line segments
    #low DB to sideline
    week1$LowDBtoRefPointSegment <- sqrt((week1$LowDBX - week1$SafetyPointX)^2 + (week1$LowDBY - week1$SafetyPointY)^2)
    #high DB to sideline
    week1$HighDBtoRefPointSegment <- sqrt((week1$HighDBX - week1$SafetyPointX)^2 + (week1$HighDBY - week1$SafetyPointY)^2)
    #low DB to high DB
    week1$LowDBHighDBSegment <- sqrt((week1$HighDBX - week1$LowDBX)^2 + (week1$HighDBY - week1$LowDBY)^2)
    
    ##determine the angle
    week1 <- week1 %>%
        mutate(Sine = HighDBtoRefPointSegment/LowDBHighDBSegment) %>%
        mutate(SafetyAngle = asin(Sine))
    #this is in radians, need to convert to degrees
    week1$SafetyAngle <- (week1$SafetyAngle*180)/pi
    
    #above 7 yards indicator 
    week1 <- week1 %>% 
        mutate(HighSafetyDepth = case_when(HighDBX >= BallSnapX + 7 ~ 'HighSafeHigh',
                                           HighDBX < BallSnapX + 7 ~ 'HighSafeLow'))
    
    week1 <- week1 %>% 
        mutate(LowSafetyDepth = case_when(LowDBX >= BallSnapX + 7 ~ 'LowSafeHigh',
                                          LowDBX < BallSnapX + 7 ~ 'LowSafeLow'))
    
    week1 <- week1 %>%
        mutate(SafetyOver7Count = case_when(HighSafetyDepth == 'HighSafeHigh' & LowSafetyDepth == 'LowSafeHigh' ~ 2,
                                            HighSafetyDepth == 'HighSafeHigh' & LowSafetyDepth == 'LowSafeLow'~ 1,
                                            HighSafetyDepth == 'HighSafeLow' & LowSafetyDepth == 'LowSafeLow' ~ 0))
    
    ##########safeties in window?
    #difference on each side
    #ID rushers and eliminate from alley
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(PassRusher = case_when(SecondAndHalfX < BallSnapX & OffDef == "Defense" ~ "PassRusher",
                                      SecondAndHalfX >= BallSnapX & OffDef == "Defense" ~ "Coverage",
                                      TRUE ~ "Offense"))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftAlley = case_when(LeftAlley == 1 & PassRusher == "PassRusher" ~ 0,
                                     TRUE ~ LeftAlley))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightAlley = case_when(RightAlley == 1 & PassRusher == "PassRusher" ~ 0,
                                      TRUE ~ RightAlley))
    
    
    ##left difference
    #defense left alley count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftAlleyCount = sum(LeftAlley)) 
    #defense left column count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(NewLeftColumnCount = sum(SnapY >= LeftColumnLine & OffDef == "Defense" & PassRusher=="Coverage")) 
    
    #sum defense left column and alley (alley tube + column tube = 'shield')
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftShield = LeftAlleyCount + NewLeftColumnCount)
    
    #offense
    #offense left column count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftOffColumnCount = sum(SnapY >= LeftColumnLine & OffDef == "Offense"))
    #offense left alley count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(LeftOffAlleyCount = sum(SnapY < LeftColumnLine & SnapY >= LeftAlleyLine & OffDef == "Offense"))
    #sum offense left shield
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftOffShield = LeftOffColumnCount + LeftOffAlleyCount)
    #left defense minus offensive players outside tackle
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftDBDiff = LeftShield - LeftOffShield)
    
    ##right diff 
    #defense
    #defense right alley count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightAlleyCount = sum(RightAlley)) 
    #defense right column count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(NewRightColumnCount = sum(SnapY <= RightColumnLine & OffDef == "Defense" & PassRusher =="Coverage")) 
    #sum right column and alley
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightShield = RightAlleyCount + NewRightColumnCount)
    
    #offense
    #offense right column count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightOffColumnCount = sum(SnapY <= RightColumnLine & OffDef == "Offense"))
    #offense right alley count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(RightOffAlleyCount = sum(SnapY > RightColumnLine & SnapY <= RightAlleyLine & OffDef == "Offense"))
    #sum offense right shield
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightOffShield = RightOffColumnCount + RightOffAlleyCount)
    #left defense minus offensive players outside tackle
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightDBDiff = RightShield - RightOffShield)
    
    #label diffs
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(LeftDiffLabel = case_when(LeftDBDiff < 1 ~ "OneHighIndicator",
                                         LeftDBDiff >= 1 ~ "TwoHighIndicator"))
    
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(RightDiffLabel = case_when(RightDBDiff < 1 ~ "OneHighIndicator",
                                          RightDBDiff >= 1 ~ "TwoHighIndicator"))
    
    #indicator
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(DiffLabel = case_when(LeftDiffLabel == "OneHighIndicator" & RightDiffLabel == "OneHighIndicator" ~ "One",
                                     LeftDiffLabel == "OneHighIndicator" & RightDiffLabel == "TwoHighIndicator" ~ "Mix",
                                     LeftDiffLabel == "TwoHighIndicator" & RightDiffLabel == "OneHighIndicator" ~ "Mix",
                                     LeftDiffLabel == "TwoHighIndicator" & RightDiffLabel == "TwoHighIndicator" ~ "Two"))
    
    #add channel into the diff indicator
    week1 <- week1 %>%
        group_by(gameId, playId) %>%
        mutate(DiffLabel = case_when(ChannelSafety == 1 & DiffLabel=="Mix" ~ "MixOne",
                                     ChannelSafety == 0 & DiffLabel=="Mix" ~ "MixTwo",
                                     TRUE ~ DiffLabel))
    
    ###using the safety angle, depth, and player difference to label the window
    week1 <- week1 %>%
        mutate(Window = case_when(SafetyOver7Count == 2 & DiffLabel == "Two" & SafetyAngle <= 30 ~ 2,
                                  SafetyOver7Count == 2 & DiffLabel == "Two" & SafetyAngle > 30 ~ 1,
                                  SafetyOver7Count == 2 & DiffLabel == "One" & SafetyAngle <= 20 ~ 2,
                                  SafetyOver7Count == 2 & DiffLabel == "One" & SafetyAngle > 20 ~ 1,
                                  SafetyOver7Count == 2 & DiffLabel == "MixOne" & SafetyAngle <= 22.5 ~ 2,
                                  SafetyOver7Count == 2 & DiffLabel == "MixOne" & SafetyAngle > 22.5 ~ 1,
                                  SafetyOver7Count == 2 & DiffLabel == "MixTwo" & SafetyAngle <= 27.5 ~ 2,
                                  SafetyOver7Count == 2 & DiffLabel == "MixTwo" & SafetyAngle > 27.5 ~ 1,
                                  SafetyOver7Count == 1 ~ 1,
                                  SafetyOver7Count == 0 ~ 0))
    
    week1 <- week1 %>%
        mutate(Window = case_when(SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "Two"  & SafetyAngle <= 35 ~ 2,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "Two"  & SafetyAngle > 35 ~ 1,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "One"  & SafetyAngle <= 25 ~ 2,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "One"  & SafetyAngle > 25 ~ 1,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixOne"  & SafetyAngle <= 22.5 ~ 2,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixOne"  & SafetyAngle > 22.5 ~ 1,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixTwo"  & SafetyAngle <= 27.5 ~ 2,
                                  SafetyOver7Count == 2 & LowDBX >= 12 & DiffLabel == "MixTwo"  & SafetyAngle > 27.5 ~ 1,
                                  TRUE ~ Window))
    
    #test
    dat2High <- filter(week1, Window == 2)
    week1High <- filter(week1, Window == 1)
    dat0High <- filter(week1, Window == 0)
    
    ####How many coverage defenders?
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(CoverageDefenders = sum(SecondAndHalfX > BallSnapX & OffDef == 'Defense'))
    
}