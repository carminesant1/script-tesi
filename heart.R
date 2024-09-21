setwd("C:/Users/csant/Desktop/TESI/df")
library(mice)
library(missForest)
library(imputeTestbench)

heart <- read.csv("heart_failure_clinical_records_dataset.csv")
heart[, c(2,4,6,10,11,13)] <- lapply(heart[, c(2,4,6,10,11,13)], function(x) factor(x, levels = c(0, 1)))

patterns_heart <- matrix(
  c(1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
    1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1),
  nrow = 5, byrow = TRUE)

x1 <- matrix(0, nrow = 100, ncol = 5); colnames(x1) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x2 <- matrix(0, nrow = 100, ncol = 5); colnames(x2) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x3 <- matrix(0, nrow = 100, ncol = 5); colnames(x3) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x4 <- matrix(0, nrow = 100, ncol = 5); colnames(x4) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x7 <- matrix(0, nrow = 100, ncol = 5); colnames(x7) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x9 <- matrix(0, nrow = 100, ncol = 5); colnames(x9) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x11<- matrix(0, nrow = 100, ncol = 5); colnames(x11) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x13 <- matrix(0, nrow = 100, ncol = 5); colnames(x13) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
for(i in 1:100){
  df <- ampute(heart, patterns = patterns_heart)$amp
  df[, c(2,4,6,10,11,13)] <- lapply(df[, c(2,4,6,10,11,13)], function(x) factor(x, levels = c(1, 2), labels = c(0, 1)))
  df_itc <- itc(df)
  df_iinpirf <- iinpirf(df)
  df_binpi <- binpi(df)
  df_missF <- missForest(df)$ximp
  MICE <- mice(df)
  df_mice <- complete(MICE, action = 5)
  na <- which(is.na(df), arr.ind = TRUE)
  na_x1 <- subset(na, na[,2] == 1)
  na_x3 <- subset(na, na[,2] == 3)
  na_x7 <- subset(na, na[,2] == 7)
  na_x9 <- subset(na, na[,2] == 9)
  na_x2 <- subset(na, na[,2] == 2)
  na_x4 <- subset(na, na[,2] == 4)
  na_x11 <- subset(na, na[,2] == 11)
  na_x13 <- subset(na, na[,2] == 13)
  x1[i,1] <- rmse(as.numeric(heart[na_x1]),as.numeric(df_itc[na_x1]))
  x1[i,2] <- rmse(as.numeric(heart[na_x1]),as.numeric(df_iinpirf[na_x1]))
  x1[i,3] <- rmse(as.numeric(heart[na_x1]),as.numeric(df_binpi[na_x1]))
  x1[i,4] <- rmse(as.numeric(heart[na_x1]),as.numeric(df_missF[na_x1]))
  x1[i,5] <- rmse(as.numeric(heart[na_x1]),as.numeric(df_mice[na_x1]))
  x3[i,1] <- rmse(as.numeric(heart[na_x3]),as.numeric(df_itc[na_x3]))
  x3[i,2] <- rmse(as.numeric(heart[na_x3]),as.numeric(df_iinpirf[na_x3]))
  x3[i,3] <- rmse(as.numeric(heart[na_x3]),as.numeric(df_binpi[na_x3]))
  x3[i,4] <- rmse(as.numeric(heart[na_x3]),as.numeric(df_missF[na_x3]))
  x3[i,5] <- rmse(as.numeric(heart[na_x3]),as.numeric(df_mice[na_x3]))
  x7[i,1] <- rmse(as.numeric(heart[na_x7]),as.numeric(df_itc[na_x7]))
  x7[i,2] <- rmse(as.numeric(heart[na_x7]),as.numeric(df_iinpirf[na_x7]))
  x7[i,3] <- rmse(as.numeric(heart[na_x7]),as.numeric(df_binpi[na_x7]))
  x7[i,4] <- rmse(as.numeric(heart[na_x7]),as.numeric(df_missF[na_x7]))
  x7[i,5] <- rmse(as.numeric(heart[na_x7]),as.numeric(df_mice[na_x7]))
  x9[i,1] <- rmse(as.numeric(heart[na_x9]),as.numeric(df_itc[na_x9]))
  x9[i,2] <- rmse(as.numeric(heart[na_x9]),as.numeric(df_iinpirf[na_x9]))
  x9[i,3] <- rmse(as.numeric(heart[na_x9]),as.numeric(df_binpi[na_x9]))
  x9[i,4] <- rmse(as.numeric(heart[na_x9]),as.numeric(df_missF[na_x9]))
  x9[i,5] <- rmse(as.numeric(heart[na_x9]),as.numeric(df_mice[na_x9]))
  x2[i,1] <- sum(heart[na_x2] == df_itc[na_x2])/length(heart[na_x2])
  x2[i,2] <- sum(heart[na_x2] == df_iinpirf[na_x2])/length(heart[na_x2])
  x2[i,3] <- sum(heart[na_x2] == df_binpi[na_x2])/length(heart[na_x2])
  x2[i,4] <- sum(heart[na_x2] == df_missF[na_x2])/length(heart[na_x2])
  x2[i,5] <- sum(heart[na_x2] == df_mice[na_x2])/length(heart[na_x2])
  x4[i,1] <- sum(heart[na_x4] == df_itc[na_x4])/length(heart[na_x4])
  x4[i,2] <- sum(heart[na_x4] == df_iinpirf[na_x4])/length(heart[na_x4])
  x4[i,3] <- sum(heart[na_x4] == df_binpi[na_x4])/length(heart[na_x4])
  x4[i,4] <- sum(heart[na_x4] == df_missF[na_x4])/length(heart[na_x4])
  x4[i,5] <- sum(heart[na_x4] == df_mice[na_x4])/length(heart[na_x4])
  x11[i,1] <- sum(heart[na_x11] == df_itc[na_x2])/length(heart[na_x11])
  x11[i,2] <- sum(heart[na_x11] == df_iinpirf[na_x2])/length(heart[na_x11])
  x11[i,3] <- sum(heart[na_x11] == df_binpi[na_x2])/length(heart[na_x11])
  x11[i,4] <- sum(heart[na_x11] == df_missF[na_x2])/length(heart[na_x11])
  x11[i,5] <- sum(heart[na_x11] == df_mice[na_x2])/length(heart[na_x11])
  x13[i,1] <- sum(heart[na_x13] == df_itc[na_x13])/length(heart[na_x13])
  x13[i,2] <- sum(heart[na_x13] == df_iinpirf[na_x13])/length(heart[na_x13])
  x13[i,3] <- sum(heart[na_x13] == df_binpi[na_x13])/length(heart[na_x13])
  x13[i,4] <- sum(heart[na_x13] == df_missF[na_x13])/length(heart[na_x13])
  x13[i,5] <- sum(heart[na_x13] == df_mice[na_x13])/length(heart[na_x13])
}

colMeans(x1); colMeans(x3); colMeans(x7); colMeans(x9)
colMeans(x2); colMeans(x4); colMeans(x11); colMeans(x13)

times <- microbenchmark(itc(df),
                        iinpirf(df),
                        binpi(df),
                        missForest(df),
                        mice(df)
                        )
summary(times)
