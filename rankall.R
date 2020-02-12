rankAll <- function(outcome, num = "best") {
    
    setwd("C:\\Users\\Gemma\\Documents\\R\\Projects\\chapter4\\rprog_data_ProgAssignment3-data")
    
    hospitalData <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    states <<- sort(unique( hospitalData$State))
    
    stateRows <- data.frame(Name=character(), State=character (), stringsAsFactors = FALSE )
    VstateRows <- data.frame(Name=character(), State=character (), stringsAsFactors = FALSE )
    
    pickedStateRows <- data.frame()
    
    for (state in seq_along(states)) {
        
        if (outcome == "heart attack") {
            
            pickedStateRows <- hospitalData[hospitalData$State == states[state], c(2, 7, 11)]
            
            #message("heart attack data")
            
        }
        
        if (outcome == "heart failure") {
            
            pickedStateRows <- hospitalData[hospitalData$State == states[state], c(2, 7, 17)]
            
            #message("heart failure data")
            
        }
        
        if (outcome == "pneumonia") {
            
            pickedStateRows <- hospitalData[hospitalData$State == states[state], c(2, 7, 23)]
            
            #message("pneumonia data")
            
        }
        
        colnames(pickedStateRows) <- c("Name","State", "Type")
        
        pickedStateRows[,"Type"] <- as.numeric(pickedStateRows[,"Type"])
        
        pickedStateRows <- pickedStateRows[!is.na(as.numeric(as.character(pickedStateRows$Type))), ]
        
        pickedStateRows <- pickedStateRows[order(pickedStateRows$Type,pickedStateRows$Name), ]
        
        if (num == "best") {
            location = 1
        }
        
        else if (num == "worst") {
            location = nrow(pickedStateRows)
        }
        
        else {
            location = num
        }
        
        picked <- data.frame(Name=character(), State=character (), stringsAsFactors = FALSE )
        
        picked = pickedStateRows[location , c(1,2)]
        
        if (is.na(picked$State)) {
            picked$State = states[state]
        }
        
        stateRows <- rbind(stateRows, picked)
        
    }
    
    #print(stateRows)
}
