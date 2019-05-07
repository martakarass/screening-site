#' @author 
#' Marta Karas
#' 
#' @description 
#' Script to simulate some foo data for the Shiny app testing purposes. 

rm(list = ls())
library(stringi)

set.seed(1)

## Params
save.path <- "data/data-foo.csv"
N <- 30
url_link.vals <- c(
  "https://www.jhu.edu/",
  "https://www.jhu.edu/about/",
  "https://www.jhu.edu/about/university-leadership/",
  "https://www.jhu.edu/about/history/",
  "https://www.jhu.edu/about/notable-alumni/",
  "https://www.jhu.edu/about/community/",
  "https://www.jhu.edu/about/international/",
  "https://hub.jhu.edu/"
)
special.vals <- ",,\\4c#[Y~TWKzp8]6Bzg<eBQn&B{.}_XgG#t5]-A9~_kxhWj7@.\\QqF\\$h5T%bNzZ,a,"

## Simulate meta data 
study.vec    <- paste0("Study name ", 1:30)
url_link.vec <- sample(url_link.vals, N, replace = TRUE)
title.vec    <- replicate(N, substr(stri_rand_lipsum(1, start_lipsum = FALSE), 1, sample(20:100, 1)))
info.vec     <- replicate(N, substr(stri_rand_lipsum(1, start_lipsum = FALSE), 1, sample(20:30, 1)))
summary.vec  <- paste0("Text with special characters: ", 
                       replicate(N, substr(special.vals, 1, sample(2:(nchar(special.vals)), 1))))

## Simulate outcome data 
## var1
var1.mat <- matrix(0, nrow = N, ncol = 6)
for (i in 1:(round(N/2))){
  var1.mat[i, sample(1:6, 1)] <- 1
}
for (i in (round(N/2)+1):N){
  var1.mat[i, sample(2:6, 2, replace = FALSE)] <- 1
}
for (i in (N-3):N){
  var1.mat[i, ] <- 1
}
## var2
var2.mat <- matrix(0, nrow = N, ncol = 4)
for (i in 1:(round(N/2))){
  var2.mat[i, sample(1:4, 1)] <- 1
}
for (i in (round(N/2)+1):N){
  var2.mat[i, sample(2:4, 2, replace = FALSE)] <- 1
}
for (i in (N-3):N){
  var2.mat[i, ] <- 1
}

## Combine simulated variables into a data frame
out.df <- data.frame(
  study = study.vec,
  url_link = url_link.vec,
  title = title.vec,
  info = info.vec,
  summary = summary.vec,
  stringsAsFactors = FALSE
)
out.df <- cbind(out.df, as.data.frame(var1.mat))
out.df <- cbind(out.df, as.data.frame(var2.mat))
names(out.df) <- c(
  "Study",
  "Url",
  "Title",
  "Info",
  "Summary",
  "Setting=Metastatic",
  "Setting=Adjuvant",
  "Setting=Neoadjuvant",
  "Setting=Radiation",
  "Setting=Window/Perioperative",
  "Setting=PreOperative",
  "Subtype=ER+",
  "Subtype=ER- PR- HER2-",
  "Subtype=HER2+",
  "Subtype=Non-Invasive"
)
out.df$`Subtype=No records should match` <- 0

str(out.df)

## Write table to file 
write.table(out.df, save.path, quote = TRUE, row.names = FALSE, sep = ",", na="")




