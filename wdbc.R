setwd("C:/Users/csant/Desktop/TESI/df")
library(mice)
library(missForest)
library(imputeTestbench)

wdbc <- read.csv("wdbc.data")
wdbc[,1] <- NULL
wdbc[,1] <- factor(wdbc[,1], labels = c("1","2"))

patterns_wdbc <- matrix(
  c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1),
  nrow = 5, byrow = TRUE)

x9 <- matrix(0, nrow = 100, ncol = 5); colnames(x9) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x11 <- matrix(0, nrow = 100, ncol = 5); colnames(x11) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x12 <- matrix(0, nrow = 100, ncol = 5); colnames(x12) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x13 <- matrix(0, nrow = 100, ncol = 5); colnames(x13) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x17 <- matrix(0, nrow = 100, ncol = 5); colnames(x17) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x20 <- matrix(0, nrow = 100, ncol = 5); colnames(x20) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x23 <- matrix(0, nrow = 100, ncol = 5); colnames(x23) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x28 <- matrix(0, nrow = 100, ncol = 5); colnames(x28) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x29 <- matrix(0, nrow = 100, ncol = 5); colnames(x29) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x1 <- matrix(0, nrow = 100, ncol = 5); colnames(x1) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
for (i in 1:100) {
  df <- ampute(wdbc, patterns = patterns_wdbc)$amp
  df[,1] <- as.factor(df[,1])
  df_itc <- itc(df)
  df_iinpirf <- iinpirf(df)
  df_binpi <- binpi(df)
  df_missF <- missForest(df)$ximp
  MICE <- mice(df)
  df_mice <- complete(MICE, action = 5)
  na <- which(is.na(df), arr.ind = TRUE)
  na_x9 <- subset(na, na[,2] == 9)
  na_x11 <- subset(na, na[,2] == 11)
  na_x12 <- subset(na, na[,2] == 12)
  na_x13 <- subset(na, na[,2] == 13)
  na_x17 <- subset(na, na[,2] == 17)
  na_x20 <- subset(na, na[,2] == 20)
  na_x23 <- subset(na, na[,2] == 23)
  na_x28 <- subset(na, na[,2] == 28)
  na_x29 <- subset(na, na[,2] == 29)
  na_x1 <- subset(na, na[,2] == 1)
  x9[i,1] <- rmse(as.numeric(wdbc[na_x9]),as.numeric(df_itc[na_x9]))
  x9[i,2] <- rmse(as.numeric(wdbc[na_x9]),as.numeric(df_iinpirf[na_x9]))
  x9[i,3] <- rmse(as.numeric(wdbc[na_x9]),as.numeric(df_binpi[na_x9]))
  x9[i,4] <- rmse(as.numeric(wdbc[na_x9]),as.numeric(df_missF[na_x9]))
  x9[i,5] <- rmse(as.numeric(wdbc[na_x9]),as.numeric(df_mice[na_x9]))
  x11[i,1] <- rmse(as.numeric(wdbc[na_x11]),as.numeric(df_itc[na_x11]))
  x11[i,2] <- rmse(as.numeric(wdbc[na_x11]),as.numeric(df_iinpirf[na_x11]))
  x11[i,3] <- rmse(as.numeric(wdbc[na_x11]),as.numeric(df_binpi[na_x11]))
  x11[i,4] <- rmse(as.numeric(wdbc[na_x11]),as.numeric(df_missF[na_x11]))
  x11[i,5] <- rmse(as.numeric(wdbc[na_x11]),as.numeric(df_mice[na_x11]))
  x12[i,1] <- rmse(as.numeric(wdbc[na_x12]),as.numeric(df_itc[na_x12]))
  x12[i,2] <- rmse(as.numeric(wdbc[na_x12]),as.numeric(df_iinpirf[na_x12]))
  x12[i,3] <- rmse(as.numeric(wdbc[na_x12]),as.numeric(df_binpi[na_x12]))
  x12[i,4] <- rmse(as.numeric(wdbc[na_x12]),as.numeric(df_missF[na_x12]))
  x12[i,5] <- rmse(as.numeric(wdbc[na_x12]),as.numeric(df_mice[na_x12]))
  x13[i,1] <- rmse(as.numeric(wdbc[na_x13]),as.numeric(df_itc[na_x13]))
  x13[i,2] <- rmse(as.numeric(wdbc[na_x13]),as.numeric(df_iinpirf[na_x13]))
  x13[i,3] <- rmse(as.numeric(wdbc[na_x13]),as.numeric(df_binpi[na_x13]))
  x13[i,4] <- rmse(as.numeric(wdbc[na_x13]),as.numeric(df_missF[na_x13]))
  x13[i,5] <- rmse(as.numeric(wdbc[na_x13]),as.numeric(df_mice[na_x13]))
  x17[i,1] <- rmse(as.numeric(wdbc[na_x17]),as.numeric(df_itc[na_x17]))
  x17[i,2] <- rmse(as.numeric(wdbc[na_x17]),as.numeric(df_iinpirf[na_x17]))
  x17[i,3] <- rmse(as.numeric(wdbc[na_x17]),as.numeric(df_binpi[na_x17]))
  x17[i,4] <- rmse(as.numeric(wdbc[na_x17]),as.numeric(df_missF[na_x17]))
  x17[i,5] <- rmse(as.numeric(wdbc[na_x17]),as.numeric(df_mice[na_x17]))
  x20[i,1] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_itc[na_x20]))
  x20[i,2] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_iinpirf[na_x20]))
  x20[i,3] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_binpi[na_x20]))
  x20[i,4] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_missF[na_x20]))
  x20[i,5] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_mice[na_x20]))
  x23[i,1] <- rmse(as.numeric(wdbc[na_x23]),as.numeric(df_itc[na_x23]))
  x23[i,2] <- rmse(as.numeric(wdbc[na_x23]),as.numeric(df_iinpirf[na_x23]))
  x23[i,3] <- rmse(as.numeric(wdbc[na_x23]),as.numeric(df_binpi[na_x23]))
  x23[i,4] <- rmse(as.numeric(wdbc[na_x23]),as.numeric(df_missF[na_x23]))
  x23[i,5] <- rmse(as.numeric(wdbc[na_x23]),as.numeric(df_mice[na_x23]))
  x28[i,1] <- rmse(as.numeric(wdbc[na_x28]),as.numeric(df_itc[na_x28]))
  x28[i,2] <- rmse(as.numeric(wdbc[na_x28]),as.numeric(df_iinpirf[na_x28]))
  x28[i,3] <- rmse(as.numeric(wdbc[na_x28]),as.numeric(df_binpi[na_x28]))
  x28[i,4] <- rmse(as.numeric(wdbc[na_x28]),as.numeric(df_missF[na_x28]))
  x28[i,5] <- rmse(as.numeric(wdbc[na_x28]),as.numeric(df_mice[na_x28]))
  x29[i,1] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_itc[na_x29]))
  x29[i,2] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_iinpirf[na_x29]))
  x29[i,3] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_binpi[na_x29]))
  x29[i,4] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_missF[na_x29]))
  x29[i,5] <- rmse(as.numeric(wdbc[na_x20]),as.numeric(df_mice[na_x29]))
  x1[i,1] <- sum(wdbc[na_x1] == df_itc[na_x1])/length(wdbc[na_x1])
  x1[i,2] <- sum(wdbc[na_x1] == df_iinpirf[na_x1])/length(wdbc[na_x1])
  x1[i,3] <- sum(wdbc[na_x1] == df_binpi[na_x1])/length(wdbc[na_x1])
  x1[i,4] <- sum(wdbc[na_x1] == df_missF[na_x1])/length(wdbc[na_x1])
  x1[i,5] <- sum(wdbc[na_x1] == df_mice[na_x1])/length(wdbc[na_x1])
}

colMeans(x9); colMeans(x11); colMeans(x12); colMeans(x13); colMeans(x17); colMeans(x20) 
colMeans(x23); colMeans(x28); colMeans(x29)
colMeans(x1)

times <- microbenchmark(itc(df),
                        iinpirf(df),
                        binpi(df),
                        missForest(df),
                        mice(df))
summary(times)



