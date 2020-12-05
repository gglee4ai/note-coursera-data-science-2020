## name: rankhospital
##  - find the best hospital name from the arguments condition
## args:
##  - state: state name, ex) "NY"
##  - outcome: one of c("heart attack", "heart failure", "pneumonia")
##  - num: rank of the hospital
## returns:
##  - hospital name
##
rankhospital <- function(state, outcome, num) {
  ## read csv file and make a dataframe
  default_warn <- getOption("warn")
  options(warn = -1)
  raw <- read.csv("outcome-of-care-measures.csv")
  df <- raw[, c(2, 7, 11, 17, 23)]
  names(df) <- c("hospital", "state", "hattack", "hfailure", "pneumonia")
  df$hattack <- as.numeric(df$hattack)
  df$hfailure <- as.numeric(df$hfailure)
  df$pneumonia <- as.numeric(df$pneumonia)
  options(warn = default_warn)
  
  ## check state
  states <- sort(unique(df$state))
  if (!(state %in% states)) {
    stop("invalid state")
  }
  
  ## check outcome
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## filtering data.frame
  col_pos <- which(outcome == outcomes)
  target <- df[df$state == state, c(1, 2, col_pos + 2)]
  filtered <- target[order(target[,3], target[,1]),]
  filtered <- filtered[complete.cases(filtered),]
  nfiltered <- nrow(filtered)
  
  ## check num
  if (num == "worst") { 
    n <- nfiltered 
  } else if (num == "best") {
    n <- 1
  } else {
    n <- num
  }
  
  ## return
  if (n > nfiltered || n < 1) return(NA)
  filtered[[n, 1]]
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
