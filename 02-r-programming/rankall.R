## name: rankhospital
##  - find the best hospital name from the arguments condition
## args:
##  - state: state name, ex) "NY"
##  - outcome: one of c("heart attack", "heart failure", "pneumonia")
##  - num: rank of the hospital
## returns:
##  - hospital name
##
rankall <- function(outcome, num = "best") {
  ## check outcome
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  ## read csv file and make a dataframe
  default_warn <- getOption("warn")
  options(warn = -1)
  raw <- read.csv("outcome-of-care-measures.csv")
  df <- raw[, c(2, 7, 11, 17, 23)]
  names(df) <- c("hospital", "state", "hattack", "hfailure", "pneumonia")
  df$hattack <- as.numeric(df$hattack)
  df$hfailure <- as.numeric(df$hfailure)
  df$pneumonia <- as.numeric(df$pneumonia)
  #df <- df[complete.cases(df),]
  splited <- split(df, df$state)
  options(warn = default_warn)
  
  rankhospital <- function(data, outcome, num) {
    #outcomes <- c("heart attack", "heart failure", "pneumonia")
    col_pos <- which(outcome == c("heart attack", "heart failure", "pneumonia"))
    target <- data[, c(1, 2, col_pos + 2)]
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
    if (n > nfiltered || n < 1) hname <- NA
    else hname <- filtered[[n, 1]]
    state_name <- unique(filtered$state)
    return(data.frame(hospital = hname, state = state_name))
  }
  res <- sapply(splited, rankhospital, outcome = outcome, num = num)
  res <- as.data.frame(t(res))
  res
}
## test
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
