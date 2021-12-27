# Script to apply coverage labels to each play of each week and return a combined data table

# Source functions for identifying coverage shell and man vs. zone designation
lapply(paste0(MainDir, '/Utils/', list.files('Utils', recursive = TRUE)),  source)

if (run_coverage_id_code_sw == 1) {
    
    # Loop through each week, identify shell and whether a defender is in man vs. zone, and save as .RDS file for easier 
    # loading
    for (i in 1:17) {
        
        currentWeekData <- tracking_data[[i]]
        
        # Eliminate problematic plays
        if (i == 2) {
            currentWeekData <- currentWeekData %>% filter(!(gameId = 2018091605 & playId == 2715))
        }
        
        if (i == 3) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018092301 | (gameId == 2018092304 & playId == 1687)))
        }
        
        if (i == 4) {
            #drop this game where Michael Thomas's tracker double counts every frame
            currentWeekData <- currentWeekData %>%
                filter(!(gameId == 2018093011))
        }
        
        if (i == 5) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018100710 & playId == 4418))
        }
        
        if (i == 11) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018111900 & playId == 5048))
        }
        
        if (i == 12) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018112510 & playId == 3572))
        }
        
        if (i == 13) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018120206 & playId == 3991))
        }
        
        if (i == 14) {
            currentWeekData$event[currentWeekData$gameId == 2018120905 & currentWeekData$playId == 1426 & currentWeekData$event == 'ball_snap' & currentWeekData$frameId == 12] = 'None'
            currentWeekData <- currentWeekData %>% filter(!(gameId = 2018120905 & playId == 1426))
        }
        
        if (i == 15) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018121605))
        }
        
        if (i == 16) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018122307 & playId == 4434))
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018122400 & playId == 2493))
            
        }
        
        if (i == 17) {
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018123001 & playId == 435))
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018123004 & playId == 2309))
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018123006))
            currentWeekData <- currentWeekData %>% filter(!(gameId == 2018123010 & playId == 334))
        }
        
        # Call Andrew's Shell code to create shell
        currentWeekData <- preprocess_before_shell(currentWeekData)
        currentWeekData <- create_columns(currentWeekData)
        currentWeekData <- create_alleys(currentWeekData)
        currentWeekData <- create_chute(currentWeekData)
        currentWeekData <- create_window(currentWeekData)   
        currentWeekData <- create_shell(currentWeekData)
        
        # Label Man or Zone (all based off at snap and fixed time after snap) 
        currentWeekData_coverage_identification <- label_man_or_zone_pre_snap(currentWeekData)
        
        # Create subshells
        currentWeekData_coverage_identification <- create_sub_shells(currentWeekData_coverage_identification)
        
        if (i == 1) {
            coverageData <- currentWeekData_coverage_identification
        } else {
            coverageData <- bind_rows(coverageData, currentWeekData_coverage_identification)
        }
        
        print(paste('Added Week', i, sep = ""))
    }
    
    saveRDS(coverageData, paste(MainDir, '/Data/', 'allWeekCoverageID.RDS', sep = ""))
    
    # Clear temporary variables
    rm(currentWeekData, currentWeekData_coverage_identification)
    
} else if(run_coverage_id_code_sw == 0) {
    coverageData <- readRDS('Data/allWeekCoverageID.RDS')
}
