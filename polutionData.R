pollutantmean <- function(directory, pollutant, id = 1:332, removeNA = TRUE) {

    meanD <- vector(mode="numeric")
    for(idNumber in id) {
        append <- "" 
        if(idNumber < 10) append <- "00" 
        else if(idNumber >= 10 && idNumber < 100) append <- "0"
        meanD <- c(meanD, read.csv(paste(".\\", directory,"\\", append, idNumber, ".csv", sep=""))[, c(pollutant)])
    }
    print(mean(meanD, na.rm = removeNA))
}

complete <- function(directory, id = 1:332, removeNA = TRUE) {
    
    for(idNumber in id) {
        append <- "" 
        if(idNumber < 10) append <- "00" 
        else if(idNumber >= 10 && idNumber < 100) append <- "0"
        meanD <- read.csv(paste(".\\", directory,"\\", append, idNumber, ".csv", sep=""))
        completeR <- 0
        for(rowN in 1: nrow(meanD)) {
            if(!is.na(meanD[rowN, "sulfate"]) && !is.na(meanD[rowN, "nitrate"])) {
                completeR <- completeR + 1 
            }
        }
        print(paste(idNumber, " : ", completeR))
    }
}

corr <- function(directory, threshold = 0, id = 1:332,  removeNA = TRUE) {
    
    for(idNumber in id) {
        append <- "" 
        if(idNumber < 10) append <- "00" 
        else if(idNumber >= 10 && idNumber < 100) append <- "0"
        meanD <- read.csv(paste(".\\", directory,"\\", append, idNumber, ".csv", sep=""))[, 2:3]
        ##meanD <- meanD[!is.na(meanD)]
        for(rowN in 1: nrow(meanD)) {
            if(is.na(meanD[rowN, "sulfate"]) | is.na(meanD[rowN, "nitrate"])) {
                meanD <- meanD[-c(rowN)] 
            }
        }
        ##meanD <- meanD[meanD$sulfate > threshold && meanD$nitrate > threshold]
        print(meanD)
        for(rowN in 1: nrow(meanD)) {
            if(meanD[rowN, "sulfate"] <  threshold || meanD[rowN, "nitrate"] < threshold) {
                meanD <- meanD[-c(rowN)] 
            }
        }       
        print(meanD)
    }
}

