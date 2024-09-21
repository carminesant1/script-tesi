setwd("C:/Users/csant/Desktop/TESI/df")
library(mice)
library(missForest)
library(imputeTestbench)
library(microbenchmark)

glass <- read.csv("glass.data")
glass[,1] <- NULL
glass[, 10] <- factor(glass[,10], labels = c("1", "2", "3", "4", "5", "6"))

patterns_glass <- matrix(
  c(1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 0),
  nrow = 4, byrow = TRUE)

x1 <- matrix(0, nrow = 100, ncol = 5); colnames(x1) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x3 <- matrix(0, nrow = 100, ncol = 5); colnames(x3) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x5 <- matrix(0, nrow = 100, ncol = 5); colnames(x5) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x6 <- matrix(0, nrow = 100, ncol = 5); colnames(x6) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x10 <- matrix(0, nrow = 100, ncol = 5); colnames(x10) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
for(i in 1:100){
  df <- ampute(glass, patterns = patterns_glass)$amp
  df[,10] <- as.factor(df[,10])
  df_itc <- itc(df)
  df_iinpirf <- iinpirf(df)
  df_binpi <- binpi(df)
  df_missF <- missForest(df)$ximp
  mice <- mice(df)
  df_mice <- complete(mice, action = 5)
  na <- which(is.na(df), arr.ind = TRUE)
  na_x1 <- subset(na, na[,2] == 1)
  na_x3 <- subset(na, na[,2] == 3)
  na_x5 <- subset(na, na[,2] == 5)
  na_x6 <- subset(na, na[,2] == 6)
  na_x10 <- subset(na, na[,2] == 10)
  x1[i,1] <- rmse(as.numeric(glass[na_x1]),as.numeric(df_itc[na_x1]))
  x1[i,2] <- rmse(as.numeric(glass[na_x1]),as.numeric(df_iinpirf[na_x1]))
  x1[i,3] <- rmse(as.numeric(glass[na_x1]),as.numeric(df_binpi[na_x1]))
  x1[i,4] <- rmse(as.numeric(glass[na_x1]),as.numeric(df_missF[na_x1]))
  x1[i,5] <- rmse(as.numeric(glass[na_x1]),as.numeric(df_mice[na_x1]))
  x3[i,1] <- rmse(as.numeric(glass[na_x3]),as.numeric(df_itc[na_x3]))
  x3[i,2] <- rmse(as.numeric(glass[na_x3]),as.numeric(df_iinpirf[na_x3]))
  x3[i,3] <- rmse(as.numeric(glass[na_x3]),as.numeric(df_binpi[na_x3]))
  x3[i,4] <- rmse(as.numeric(glass[na_x3]),as.numeric(df_missF[na_x3]))
  x3[i,5] <- rmse(as.numeric(glass[na_x3]),as.numeric(df_mice[na_x3]))
  x5[i,1] <- rmse(as.numeric(glass[na_x5]),as.numeric(df_itc[na_x5]))
  x5[i,2] <- rmse(as.numeric(glass[na_x5]),as.numeric(df_iinpirf[na_x5]))
  x5[i,3] <- rmse(as.numeric(glass[na_x5]),as.numeric(df_binpi[na_x5]))
  x5[i,4] <- rmse(as.numeric(glass[na_x5]),as.numeric(df_missF[na_x5]))
  x5[i,5] <- rmse(as.numeric(glass[na_x5]),as.numeric(df_mice[na_x5]))
  x6[i,1] <- rmse(as.numeric(glass[na_x6]),as.numeric(df_itc[na_x6]))
  x6[i,2] <- rmse(as.numeric(glass[na_x6]),as.numeric(df_iinpirf[na_x6]))
  x6[i,3] <- rmse(as.numeric(glass[na_x6]),as.numeric(df_binpi[na_x6]))
  x6[i,4] <- rmse(as.numeric(glass[na_x6]),as.numeric(df_missF[na_x6]))
  x6[i,5] <- rmse(as.numeric(glass[na_x6]),as.numeric(df_mice[na_x6]))
  x10[i,1] <- sum(glass[na_x10] == df_itc[na_x10])/length(glass[na_x10])
  x10[i,2] <- sum(glass[na_x10] == df_iinpirf[na_x10])/length(glass[na_x10])
  x10[i,3] <- sum(glass[na_x10] == df_binpi[na_x10])/length(glass[na_x10])
  x10[i,4] <- sum(glass[na_x10] == df_missF[na_x10])/length(glass[na_x10])
  x10[i,5] <- sum(glass[na_x10] == df_mice[na_x10])/length(glass[na_x10])
}

colMeans(x1); colMeans(x3); colMeans(x5); colMeans(x6)
colMeans(x10)

times <- microbenchmark(itc(df),
                        iinpirf(df),
                        binpi(df),
                        missForest(df),
                        mice(df))
summary(times)