pollutantmean <- function(directory, pollutant, id = 1:332) {
  df <- data.frame()
  for (i in id) {
    filename <- paste0(directory, "/", formatC(i, width = 3, flag = "0"), ".csv")
    df_read <- read.csv(filename)
    df <- rbind(df, read.csv(filename))
  }
  mean(df[[pollutant]], na.rm = TRUE)
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


complete <- function(directory, id = 1:332) {
  nobs <- vector("numeric", length(id))
  for (i in seq_along(id)) {
    filename <- paste0(directory, "/", formatC(id[[i]], width = 3, flag = "0"), ".csv")
    df <- read.csv(filename)
    nobs[[i]] <- sum(complete.cases(df))
  }
  data.frame(id = id, nobs = nobs)
}
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


corr <- function(directory, threshold = 0) {
  filenames <- dir(directory)
  cors <- list()
  for (filename in filenames) {
    filename <- file.path(directory, filename)
    df <- read.csv(filename)
    df <- df[complete.cases(df), ]
    if (nrow(df) > threshold) {
      cors[[filename]] <- cor(df$sulfate, df$nitrate)
    }
  }
  append(vector("numeric"), unname(unlist(cors)))
}
cr <- corr("specdata", 150)
head(cr)
cr <- corr("specdata", 400)
head(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)

