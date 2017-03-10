myfunction <- function() {
    x <- norm(100)
    mean(x)
}

second <- function(x) {
    x + rnorm(length(x))
}

third <- function() {
    x <- c(4, "a", TRUE)
    x
}

fourth <- function() {
    x <- 1:4 
    y <- 2:3
    x + y
}

columnmean <- function(y, removeNA = TRUE) {
    nc <- ncol(y)
    means <- numeric(nc)
    for(i in 1: nc) {
        means[i] <- mean(y[, i], na.rm = removeNA)
    }
    means
}

Test1 <- function() {
    x <- 1:10
    if(x > 5) {
        x <- 0
    }
    x
}

f1 <- function() {
    x <- 5
    y <- if(x < 3) {
        NA
    } else {
        10
    }
    y
}


