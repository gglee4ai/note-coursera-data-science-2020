## name: best
##  - find the best hospital for selected outcome in a state
## args:
##  - state: state name, ex) "NY"
##  - outcome: one of c("heart attack", "heart failure", "pneumonia")
## returns:
##  - hospital name with the lowest rate in the outcome category
##
best <- function(state, outcome) {
  ## setup data.frame
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
  col_pos <- which(outcomes == outcome)
  target <- df[df$state == state, c(1, 2, col_pos + 2)]
  filtered <- target[order(target[,3], target[,1]),]
  filtered <- filtered[complete.cases(filtered),]
  ## return the first value
  filtered[[1, 1]]
}

## test
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

