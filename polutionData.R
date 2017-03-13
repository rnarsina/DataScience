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

corr <- function(directory, threshold = 0, id = 1:332) {
    
    corrVector = numeric()
    for(idNumber in id) {
        append <- "" 
        if(idNumber < 10) append <- "00" 
        else if(idNumber >= 10 && idNumber < 100) append <- "0"
        meanD <- read.csv(paste(".\\", directory,"\\", append, idNumber, ".csv", sep=""))[ ,2:3]
        sulfates <- numeric()
        nitrates <- numeric()
        for(rowN in 1: nrow(meanD)) {

            if(!is.na(meanD[rowN, "sulfate"]) && !is.na(meanD[rowN, "nitrate"])) {
                sulfates <- append(sulfates, meanD[rowN, "sulfate"])
                nitrates <- append(nitrates, meanD[rowN, "nitrate"])
            }
        }
        
        lengthR <- length(sulfates)
        if(lengthR >= threshold) {
            corrVector <- append(corrVector, cor(sulfates, nitrates))
        }
    }
    corrVector
}

