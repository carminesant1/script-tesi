library(mice)
library(missForest)
library(imputeTestbench)
library(microbenchmark)

data("iris")
iris[,5] <- factor(iris[,5], labels = c("1","2","3"))

patterns_iris <- matrix(
  c(0, 1, 1, 1, 1,
    1, 1, 1, 1, 0,
    1, 0, 1, 1, 0),
  nrow = 3, byrow = TRUE)

x1 <- matrix(0, nrow = 100, ncol = 5); colnames(x1) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x2 <- matrix(0, nrow = 100, ncol = 5); colnames(x2) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
x5 <- matrix(0, nrow = 100, ncol = 5); colnames(x5) <- c("ITC", "IINPIRF", "BINPI", "MissForest", "MICE")
for(i in 1:100){
  df <- ampute(iris, patterns = patterns_iris)$amp
  df[,5] <- as.factor(df[,5])
  df_itc <- itc(df)
  df_iinpirf <- iinpirf(df)
  df_binpi <- binpi(df)
  df_missF <- missForest(df)$ximp
  MICE <- mice(df, method = "rf")
  df_mice <- complete(MICE, action = 5)
  na <- which(is.na(df), arr.ind = TRUE)
  na_x1 <- subset(na, na[,2] == 1)
  na_x2 <- subset(na, na[,2] == 2)
  na_x5 <- subset(na, na[,2] == 5)
  x1[i,1] <- rmse(as.numeric(iris[na_x1]),as.numeric(df_itc[na_x1]))
  x1[i,2] <- rmse(as.numeric(iris[na_x1]),as.numeric(df_iinpirf[na_x1]))
  x1[i,3] <- rmse(as.numeric(iris[na_x1]),as.numeric(df_binpi[na_x1]))
  x1[i,4] <- rmse(as.numeric(iris[na_x1]),as.numeric(df_missF[na_x1]))
  x1[i,5] <- rmse(as.numeric(iris[na_x1]),as.numeric(df_mice[na_x1]))
  x2[i,1] <- rmse(as.numeric(iris[na_x2]),as.numeric(df_itc[na_x2]))
  x2[i,2] <- rmse(as.numeric(iris[na_x2]),as.numeric(df_iinpirf[na_x2]))
  x2[i,3] <- rmse(as.numeric(iris[na_x2]),as.numeric(df_binpi[na_x2]))
  x2[i,4] <- rmse(as.numeric(iris[na_x2]),as.numeric(df_missF[na_x2]))
  x2[i,5] <- rmse(as.numeric(iris[na_x2]),as.numeric(df_mice[na_x2]))
  x5[i,1] <- sum(iris[na_x5] == df_itc[na_x5])/length(iris[na_x5])
  x5[i,2] <- sum(iris[na_x5] == df_iinpirf[na_x5])/length(iris[na_x5])
  x5[i,3] <- sum(iris[na_x5] == df_binpi[na_x5])/length(iris[na_x5])
  x5[i,4] <- sum(iris[na_x5] == df_missF[na_x5])/length(iris[na_x5])
  x5[i,5] <- sum(iris[na_x5] == df_mice[na_x5])/length(iris[na_x5])
}

colMeans(x1); colMeans(x2) 
colMeans(x5)

times <- microbenchmark(itc(df),
                        iinpirf(df),
                        binpi(df),
                        missForest(df),
                        mice(df))
summary(times)
