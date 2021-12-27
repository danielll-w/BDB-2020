preprocess_before_shell <- function(week1) {
    # Preprocessing ####
    
    #flip x,y,direction, orientation
    week1 <- week1 %>% 
        mutate(x = case_when(playDirection == "right" ~ x , 
                             playDirection == "left" ~ 120 - x)) %>%
        mutate(y = case_when(playDirection == "right" ~ y , 
                             playDirection == "left" ~ 53.33 - y)) %>%
        mutate(o = case_when(playDirection == "right" ~ o,
                             playDirection == "left" ~ mod(o+180, 360))) %>%
        mutate(dir = case_when(playDirection == "right" ~ dir,
                               playDirection == "left" ~ mod(dir+180, 360)))
    
    ##ball location
    #y
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(BallSnapY = y[match('Football ball_snap', paste(displayName, event))]) %>% 
        ungroup
    
    #x
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(BallSnapX = x[match('Football ball_snap', paste(displayName, event))]) %>% 
        ungroup
    
    #snap frame
    week1 <- week1 %>%
        group_by(gameId, playId) %>%  
        mutate(SnapFrame = frameId[match('ball_snap', event)])
    
    #is it the snap frame?
    week1 <- week1 %>%
        group_by(gameId, playId) %>%  
        mutate(isSnapFrame = case_when(frameId==SnapFrame ~ 1,
                                       TRUE ~0))
    #frames since the snap
    week1 <- week1 %>%
        group_by(gameId, playId) %>%  
        mutate(FramesSinceSnap = frameId - SnapFrame)
    
    ##side of player relative to spot where ball is snapped from (changes continuously)
    week1 <- week1 %>% 
        mutate(SideDuring = case_when(BallSnapY < y ~ "Left", 
                                      BallSnapY > y ~ "Right",
                                      BallSnapY == y ~ "OTB"))
    
    #side at the snap (should be SideDuring for that player when the ball was snapped)
    week1 <- week1 %>% 
        group_by(gameId, playId) %>%
        mutate(SideSnap = case_when(event == 'ball_snap' & y >= BallSnapY ~ 'Left', 
                                    event == 'ball_snap' & y < BallSnapY ~ 'Right', 
                                    event == 'ball_snap' & y == BallSnapY ~ 'Center')) %>%
        mutate(SideSnap = replace(SideSnap, event != 'ball_snap', SideSnap[event == 'ball_snap'])) %>%
        ungroup
    
    ##need to fix this mutate command^^
    
    
    ##eliminate plays in opposing redzone
    week1 <- filter(week1, BallSnapX < 90)
    #needs to be 90 to account for left endzone
    
    ##location of each player at snap
    #Y
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(SnapY = y[event == "ball_snap"]) %>%
        ungroup
    
    #X
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(SnapX = x[event == "ball_snap"]) %>%
        ungroup
    
    #ID correct QB
    week1 <- week1 %>% 
        group_by(gameId , playId) %>% 
        mutate(ball_snap_y = y[match('Football ball_snap' , paste(displayName, event))]) %>%
        mutate(ydist2ball_snap = abs(SnapY - BallSnapY)) %>%
        mutate( isBackfieldQB = case_when(position == "QB" & ydist2ball_snap < 2 ~ 1, 
                                          TRUE ~ 0)) %>% 
        mutate(ball_snap_y = NULL) %>% mutate(dist2ball_snap = NULL) 
    
    #backfield count
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>%
        mutate(BackfieldQBCount = sum(isBackfieldQB)) 
    
    dattest <- filter(week1, BackfieldQBCount == 0 & FramesSinceSnap==0)
    
    #eliminate no QB under center 
    week1 <- filter(week1, BackfieldQBCount != 0)
    
    #eliminate 2 QBs under center
    week1 <- filter(week1, BackfieldQBCount != 2)
    
    ##is player on offense or defense
    #create qb team                        
    week1 <- week1 %>%
        group_by(gameId, playId, frameId) %>% 
        mutate(qb_team = team[match(1, isBackfieldQB)]) %>%
        ungroup
    
    #is player on QB's team?
    week1 <- week1 %>%
        mutate(OffDef = case_when(team == qb_team ~ "Offense",
                                  team != qb_team ~ "Defense")) %>%
        mutate(OffDef = case_when(displayName != "Football" ~ OffDef,
                                  displayName == "Football" ~ "Football"))
    ##delete plays where  QB isn't directly behind center
    
    #create QB team
    week1 <- week1 %>% 
        group_by(gameId, playId) %>%
        mutate(QBTeam = case_when(isBackfieldQB == 1 ~ "QB",
                                  TRUE ~ OffDef))
    
    ##number receivers on each side
    week1 <- week1 %>% 
        group_by(gameId, playId, QBTeam) %>% 
        mutate(Num = local({
            fsnap <- isSnapFrame == 1
            y <- y[fsnap]
            left <- SideSnap[fsnap] == "Left"
            right <- !left
            x <- integer(length(y))
            names(x) <- displayName[fsnap]
            x[left] <- rank(-y[left], ties.method = "min")
            x[right] <- rank(y[right], ties.method = "min")
            unname(x[displayName])
        }))
    
    #need to get rid of plays where pass happens before frame 26
    week1 <- week1 %>%
        group_by(gameId, playId) %>%  
        mutate(ThrowFrame = frameId[match('pass_forward', event)]) %>%
        mutate(ShovelThrowFrame = frameId[match('pass_shovel', event)]) %>%
        filter(ThrowFrame >= 26 | is.na(ThrowFrame) | ShovelThrowFrame >= 26)
    
    #need to get rid of plays where theres not 15 frames after snap
    week1 <- week1 %>%
        group_by(gameId, playId) %>%  
        mutate(MaxFramesSinceSnap = max(FramesSinceSnap)) %>%
        filter(MaxFramesSinceSnap >= 15)
    
    ##half-second into the play
    #Y
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(HalfSecondY = y[FramesSinceSnap == 5]) %>%
        ungroup
    
    #X
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(HalfSecond = x[FramesSinceSnap == 5]) %>%
        ungroup
    
    ##8/10s of second into the play
    #Y
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(EightTenthsY = y[FramesSinceSnap == 8]) %>%
        ungroup
    #X
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(EightTenthsX = x[FramesSinceSnap == 8]) %>%
        ungroup
    
    ##1.5 seconds into the play
    #Y
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(SecondAndHalfY = y[FramesSinceSnap == 15]) %>%
        ungroup
    #X
    week1 <- week1 %>% 
        group_by(displayName, gameId, playId) %>%
        mutate(SecondAndHalfX = x[FramesSinceSnap == 15]) %>%
        ungroup
    
    week1 <- week1 %>% 
        filter(FramesSinceSnap == 0 | FramesSinceSnap == 5 | FramesSinceSnap == 8 | FramesSinceSnap == 15)
    
    #new db position name
    week1 <- week1 %>% 
        mutate(DBPos = case_when(position == "DB" |position == "CB"| position == "FS" | position == "SS" |position == "S" ~ "DB",
                                 position == "MLB" |position == "LB" | position == "OLB" |position == "ILB"~"LB",
                                 TRUE ~ position))
    
    ##L and R
    week1 <- week1%>%
        mutate(LR = case_when(SideSnap == "Left" ~ "L",
                              SideSnap == "Right" ~ "R",
                              SideSnap == "OTB" ~ "C"))
    ##paste
    week1$ReceiverNumber <- paste(week1$LR, week1$Num, sep="")
    ##paste again
    week1$ReceiverNumber <- paste(week1$ReceiverNumber, week1$QBTeam, sep="")
    
    
    #unique playid
    week1 <- week1 %>%
        mutate(uniqueplay = (gameId*1000)+ playId)
    
    ###location of each player by number
    ##Left
    #L1 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(L1OffY = y[match('L1Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #L2 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(L2OffY = y[match('L2Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #L3 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(L3OffY = y[match('L3Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #L4 
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(L4OffY = y[match('L4Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    ##Right  
    #R1
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(R1OffY = y[match('R1Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #R2
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(R2OffY = y[match('R2Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #R3
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(R3OffY = y[match('R3Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
    
    #R4
    week1 <- week1 %>% 
        group_by(gameId, playId) %>% 
        mutate(R4OffY = y[match('R4Offense ball_snap', paste(ReceiverNumber, event))]) %>%
        ungroup
}