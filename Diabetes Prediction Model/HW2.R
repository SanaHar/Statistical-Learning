library(data.table)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(caret)
library(leaps)
library(glmnet)


# load datasets
d1 = fread('C:/Users/Beethoven/Downloads/diabetes_012_health_indicators_BRFSS2015.csv')
d2 = fread('C:/Users/Beethoven/Downloads/diabetes_binary_5050split_health_indicators_BRFSS2015.csv')
d3 = fread('C:/Users/Beethoven/Downloads/diabetes_binary_health_indicators_BRFSS2015.csv')

# factorize outcomes
d1$Diabetes_012 = as.factor(d1$Diabetes_012)
d2$Diabetes_binary = as.factor(d2$Diabetes_binary)
d3$Diabetes_binary = as.factor(d3$Diabetes_binary)

# Q1------------------------------------------------------------------

# see effect of BMI
p1 <- ggplot(d1, aes(BMI, group = Diabetes_012, color = Diabetes_012)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of BMI on Diabetes - Dataset1")

p2 <- ggplot(d2, aes(BMI, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of BMI on Diabetes - Dataset2")

p3 <- ggplot(d3, aes(BMI, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of BMI on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of AGE
p1 <- ggplot(d1, aes(Age, group = Diabetes_012, color = Diabetes_012)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Age on Diabetes - Dataset1")

p2 <- ggplot(d2, aes(Age, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Age on Diabetes - Dataset2")

p3 <- ggplot(d3, aes(Age, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Age on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of Income
p1 <- ggplot(d1, aes(Income, group = Diabetes_012, color = Diabetes_012)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Income on Diabetes - Dataset1")

p2 <- ggplot(d2, aes(Income, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Income on Diabetes - Dataset2")

p3 <- ggplot(d3, aes(Income, group = Diabetes_binary, color = Diabetes_binary)) +
  geom_boxplot(alpha = .75)+
  ggtitle(label = "Effect of Income on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of NoDocbcCost
d1$one = 1
ds = d1[, .(n = sum(one)), .(Diabetes_012, NoDocbcCost)]
ds[, n_total := sum(n), .(NoDocbcCost)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(NoDocbcCost), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of NoDocbcCost on Diabetes - Dataset1")


d2$one = 1
ds = d2[, .(n = sum(one)), .(Diabetes_binary, NoDocbcCost)]
ds[, n_total := sum(n), .(NoDocbcCost)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(NoDocbcCost), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of NoDocbcCost on Diabetes - Dataset2")


d3$one = 1
ds = d3[, .(n = sum(one)), .(Diabetes_binary, NoDocbcCost)]
ds[, n_total := sum(n), .(NoDocbcCost)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(NoDocbcCost), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of NoDocbcCost on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of Sex
ds = d1[, .(n = sum(one)), .(Diabetes_012, Sex)]
ds[, n_total := sum(n), .(Sex)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(Sex), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of Sex on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, Sex)]
ds[, n_total := sum(n), .(Sex)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(Sex), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of Sex on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, Sex)]
ds[, n_total := sum(n), .(Sex)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(Sex), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of Sex on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of HighChol
ds = d1[, .(n = sum(one)), .(Diabetes_012, HighChol)]
ds[, n_total := sum(n), .(HighChol)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(HighChol), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighChol on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, HighChol)]
ds[, n_total := sum(n), .(HighChol)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(HighChol), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighChol on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, HighChol)]
ds[, n_total := sum(n), .(HighChol)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(HighChol), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighChol on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of HighBP
ds = d1[, .(n = sum(one)), .(Diabetes_012, HighBP)]
ds[, n_total := sum(n), .(HighBP)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(HighBP), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighBP on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, HighBP)]
ds[, n_total := sum(n), .(HighBP)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(HighBP), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighBP on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, HighBP)]
ds[, n_total := sum(n), .(HighBP)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(HighBP), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of HighBP on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of CholCheck
ds = d1[, .(n = sum(one)), .(Diabetes_012, CholCheck)]
ds[, n_total := sum(n), .(CholCheck)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(CholCheck), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of CholCheck on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, CholCheck)]
ds[, n_total := sum(n), .(CholCheck)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(CholCheck), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of CholCheck on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, CholCheck)]
ds[, n_total := sum(n), .(CholCheck)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(CholCheck), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of CholCheck on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)


#see effect MentHlth
ds = d1[, .(n = sum(one)), .(Diabetes_012, MentHlth)]
ds[, n_total := sum(n), .(MentHlth)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(MentHlth), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of MentHlth on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, MentHlth)]
ds[, n_total := sum(n), .(MentHlth)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(MentHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of MentHlth on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, MentHlth)]
ds[, n_total := sum(n), .(MentHlth)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(MentHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of MentHlth on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of PhysHlth
ds = d1[, .(n = sum(one)), .(Diabetes_012, PhysHlth)]
ds[, n_total := sum(n), .(PhysHlth)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(PhysHlth), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of PhysHlth on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, PhysHlth)]
ds[, n_total := sum(n), .(PhysHlth)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(PhysHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of PhysHlth on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, PhysHlth)]
ds[, n_total := sum(n), .(PhysHlth)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(PhysHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of PhysHlth on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# see effect of GenHlth
ds = d1[, .(n = sum(one)), .(Diabetes_012, GenHlth)]
ds[, n_total := sum(n), .(GenHlth)]
ds[, n_percent := n / n_total]
p1 <- ggplot(ds, aes(as.factor(GenHlth), n_percent, fill = Diabetes_012))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of GenHlth on Diabetes - Dataset1")

ds = d2[, .(n = sum(one)), .(Diabetes_binary, GenHlth)]
ds[, n_total := sum(n), .(GenHlth)]
ds[, n_percent := n / n_total]
p2 <- ggplot(ds, aes(as.factor(GenHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of GenHlth on Diabetes - Dataset2")

ds = d3[, .(n = sum(one)), .(Diabetes_binary, GenHlth)]
ds[, n_total := sum(n), .(GenHlth)]
ds[, n_percent := n / n_total]
p3 <- ggplot(ds, aes(as.factor(GenHlth), n_percent, fill = Diabetes_binary))+
  geom_bar(stat = 'identity', )+
  ggtitle(label = "Effect of GenHlth on Diabetes - Dataset3")

grid.arrange(p1, p2, p3, ncol = 3)

# Q2------------------------------------------------------------------
d1 = d1[, 1:22]
d2 = d2[, 1:22]
d3 = d3[, 1:22]

train_ratio = 0.25
split1<- sample(c(rep(0, train_ratio * nrow(d1)), rep(1, (1-train_ratio) * nrow(d1))))
train1 <- d1[split1 == 0, ]   
test1 <- d1[split1== 1, ]  

model1 <- randomForest(
  Diabetes_012 ~ ., data = train1, 
  ntree = 100,            
  nodesize = 2,         
  mtry = sqrt(ncol(train1)),  
  parallel = TRUE)

best_features1 <- data.frame(importance(model1))
predictors <- rownames(best_features1)
index <-order(best_features1$MeanDecreaseGini, decreasing = TRUE)
predictors[index]
sort(best_features1$MeanDecreaseGini, decreasing = TRUE)

split2<- sample(c(rep(0, train_ratio * nrow(d2)), rep(1, (1-train_ratio) * nrow(d2))))
train2 <- d2[split2 == 0, ]   
test2 <- d2[split2 == 1, ] 
model2 <- randomForest(
  Diabetes_binary ~ ., data = train2, 
  ntree = 100,            
  nodesize = 2,         
  mtry = sqrt(ncol(train2)),  
  parallel = TRUE)

best_features2 <- data.frame(importance(model2))
predictors <- rownames(best_features2)
index <-order(best_features2$MeanDecreaseGini, decreasing = TRUE)
predictors[index]
sort(best_features2$MeanDecreaseGini, decreasing = TRUE)

split3<- sample(c(rep(0, train_ratio * nrow(d3)), rep(1, (1-train_ratio) * nrow(d3))))
train3 <- d3[split3 == 0, ]   
test3 <- d3[split3 == 1, ] 
model3 <- randomForest(
  Diabetes_binary ~ ., data = train3, 
  ntree = 100,            
  nodesize = 2,         
  mtry = sqrt(ncol(train3)),  
  parallel = TRUE)

best_features3 <- data.frame(importance(model3))
predictors <- rownames(best_features3)
index <-order(best_features3$MeanDecreaseGini, decreasing = TRUE)
predictors[index]
sort(best_features3$MeanDecreaseGini, decreasing = TRUE)

# Q3---------------------------------

regfit.full <- regsubsets(Diabetes_012 ~ ., data = d1, nvmax = 21)
reg.summary <- summary(regfit.full)

par( mfrow =  c(2,2) ) # 2 row, 2 columns in plot window
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# find the peak of adj. R^2
adjr2.max <- which.max( reg.summary$adjr2 )
points(adjr2.max, reg.summary$adjr2[adjr2.max], col = "red", pch = 20, cex = 2)

# cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp.min <- which.min(reg.summary$cp)
points(cp.min, reg.summary$cp[cp.min], col = "red", cex = 2, pch = 20)

# bic
bic.min <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(bic.min, reg.summary$bic[bic.min], col = "red", cex = 2, pch = 20)

par( mfrow = c(1,1), oma = c(3,0,0,0))

# plot model comparison based on R^2
plot(regfit.full, scale = "r2")

# plot model comparison based on adjusted R^2
plot(regfit.full, scale = "adjr2")


# plot model comparison based on adjusted CP
plot(regfit.full, scale = "Cp")

# plot model comparison based on adjusted BIC
plot(regfit.full, scale = "bic")

coef(regfit.full, bic.min)

# mdoel coefficients of best 5-variable models
coef(regfit.full, 5)


regfit.fwd <- regsubsets(Diabetes_012 ~ ., data = d1, nvmax = 21, method = "forward")
regfit.bwd <- regsubsets(Diabetes_012 ~ ., data = d1, nvmax = 21, method = "backward")


# mdoel coefficients of best 5-variable models
coef(regfit.fwd, 5)

# mdoel coefficients of best 5-variable models
coef(regfit.bwd, 5)



# Choosing Among Models Using the Validation Set Approach and Cross-Validation
set.seed(1)

# sample true or false for each observation
train <- sample( c(TRUE, FALSE), size = nrow(d1), replace = TRUE )
# the complement
test <- (!train)

# best subset on full data set
regfit.best <- regsubsets(Diabetes_012 ~ ., data = d1[train, ], nvmax = 21)

test.mat <- model.matrix(Diabetes_012 ~ ., data = d1[test, ])

val.errors <- NA
for (i in 1:21 ){
  coefi <- coef(regfit.best, id = i)
  y_hat <- test.mat[, names(coefi)] %*% coefi
  y <- as.numeric(d1$Diabetes_012[test])
  val.errors[i] <- mean((y - y_hat)^2   )
}

val.errors

# which model has smallest error
min.val.errors <- which.min(val.errors)
# coefficients of that model
coef( regfit.best, min.val.errors )



# precict function for repeatedly choosing model with lowest test error
predict.regsubsets <- function( object, newdata, id, ... ){
  # get the formula from the model
  m.formula <- as.formula( object$call[[2]] )
  # use that formula to create the model matrix for some new data
  mat <- model.matrix( m.formula, newdata )
  # get coeffiecients where id is the number of variables
  coefi <- coef( object, id = id )
  # get the variable names of current model
  xvars <- names( coefi )
  # multiply data with coefficients
  mat[ , xvars ] %*% coefi
}


k <- 10
set.seed(1)
# fold assignment for each observation
folds <- sample(1:k, nrow(d1), replace = TRUE)
# frequency table of fold assignment (should be relatively balanced)
table(folds)

# container for cross-validation errors
cv.errors <- matrix(NA, nrow = k, ncol = 21, dimnames = list(NULL, paste(1:21)))
# have a look at the matrix
cv.errors



# loop over folds
for (a in 1:k){
  
  # best subset selection on training data
  best.fit <- regsubsets(Diabetes_012 ~ ., data = d1[ folds != a, ], nvmax = 21)
  
  # loop over the 21 subsets
  for (b in 1:21){
    
    # predict response for test set for current subset
    pred <- predict(best.fit, d1[ folds == a ,], id = b )
    
    # MSE into container; rows are folds; columns are subsets
    real <- as.numeric(d1$Diabetes_012[folds==a])
    
    cv.errors[a, b] <- mean( (real - pred)^2 )
    
  } # end of loop over the 16 subsets
} # end of loop over folds
# the cross-validation error matrix
cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# visualize
par( mfrow = c(1,1) , oma = c(0,0,0,0))
plot( mean.cv.errors, type = "b" )

# run regsubsets on full data set
reg.best <- regsubsets(Diabetes_012 ~ ., data = d1, nvmax = 21)
# coefficients of subset which minimized test error
coef(reg.best, which.min(mean.cv.errors))

# final best model
coef(reg.best, 5)


## dataset2

regfit.full <- regsubsets(Diabetes_binary ~ ., data = d2, nvmax = 21)
reg.summary <- summary(regfit.full)

par( mfrow =  c(2,2) ) # 2 row, 2 columns in plot window
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# find the peak of adj. R^2
adjr2.max <- which.max( reg.summary$adjr2 )
points(adjr2.max, reg.summary$adjr2[adjr2.max], col = "red", pch = 20, cex = 2)

# cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp.min <- which.min(reg.summary$cp)
points(cp.min, reg.summary$cp[cp.min], col = "red", cex = 2, pch = 20)

# bic
bic.min <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(bic.min, reg.summary$bic[bic.min], col = "red", cex = 2, pch = 20)

par( mfrow = c(1,1), oma = c(3,0,0,0))

# plot model comparison based on R^2
plot(regfit.full, scale = "r2")

# plot model comparison based on adjusted R^2
plot(regfit.full, scale = "adjr2")


# plot model comparison based on adjusted CP
plot(regfit.full, scale = "Cp")

# plot model comparison based on adjusted BIC
plot(regfit.full, scale = "bic")

coef(regfit.full, bic.min)

# mdoel coefficients of best 5-variable models
coef(regfit.full, 5)


regfit.fwd <- regsubsets(Diabetes_binary ~ ., data = d2, nvmax = 21, method = "forward")
regfit.bwd <- regsubsets(Diabetes_binary ~ ., data = d2, nvmax = 21, method = "backward")


# mdoel coefficients of best 5-variable models
coef(regfit.fwd, 5)

# mdoel coefficients of best 5-variable models
coef(regfit.bwd, 5)


k <- 10
set.seed(1)
# fold assignment for each observation
folds <- sample(1:k, nrow(d2), replace = TRUE)
# frequency table of fold assignment (should be relatively balanced)
table(folds)

# container for cross-validation errors
cv.errors <- matrix(NA, nrow = k, ncol = 21, dimnames = list(NULL, paste(1:21)))
# have a look at the matrix
cv.errors



# loop over folds
for (a in 1:k){
  
  # best subset selection on training data
  best.fit <- regsubsets(Diabetes_binary ~ ., data = d2[ folds != a, ], nvmax = 21)
  
  # loop over the 21 subsets
  for (b in 1:21){
    
    # predict response for test set for current subset
    pred <- predict(best.fit, d2[ folds == a ,], id = b )
    
    # MSE into container; rows are folds; columns are subsets
    real <- as.numeric(d2$Diabetes_binary[folds==a])
    
    cv.errors[a, b] <- mean( (real - pred)^2 )
    
  } # end of loop over the 16 subsets
} # end of loop over folds
# the cross-validation error matrix
cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# visualize
par( mfrow = c(1,1) , oma = c(0,0,0,0))
plot( mean.cv.errors, type = "b" )

# run regsubsets on full data set
reg.best <- regsubsets(Diabetes_binary ~ ., data = d2, nvmax = 21)
# coefficients of subset which minimized test error
coef(reg.best, which.min(mean.cv.errors))

# final best model
coef(reg.best, 5)


## dataset3

regfit.full <- regsubsets(Diabetes_binary ~ ., data = d3, nvmax = 21)
reg.summary <- summary(regfit.full)

par( mfrow =  c(2,2) ) # 2 row, 2 columns in plot window
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "Residual Sum of Squares", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")

# find the peak of adj. R^2
adjr2.max <- which.max( reg.summary$adjr2 )
points(adjr2.max, reg.summary$adjr2[adjr2.max], col = "red", pch = 20, cex = 2)

# cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp.min <- which.min(reg.summary$cp)
points(cp.min, reg.summary$cp[cp.min], col = "red", cex = 2, pch = 20)

# bic
bic.min <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(bic.min, reg.summary$bic[bic.min], col = "red", cex = 2, pch = 20)

par( mfrow = c(1,1), oma = c(3,0,0,0))

# plot model comparison based on R^2
plot(regfit.full, scale = "r2")

# plot model comparison based on adjusted R^2
plot(regfit.full, scale = "adjr2")


# plot model comparison based on adjusted CP
plot(regfit.full, scale = "Cp")

# plot model comparison based on adjusted BIC
plot(regfit.full, scale = "bic")

coef(regfit.full, bic.min)

# mdoel coefficients of best 5-variable models
coef(regfit.full, 5)


regfit.fwd <- regsubsets(Diabetes_binary ~ ., data = d3, nvmax = 21, method = "forward")
regfit.bwd <- regsubsets(Diabetes_binary ~ ., data = d3, nvmax = 21, method = "backward")


# mdoel coefficients of best 5-variable models
coef(regfit.fwd, 5)

# mdoel coefficients of best 5-variable models
coef(regfit.bwd, 5)


k <- 10
set.seed(1)
# fold assignment for each observation
folds <- sample(1:k, nrow(d3), replace = TRUE)
# frequency table of fold assignment (should be relatively balanced)
table(folds)

# container for cross-validation errors
cv.errors <- matrix(NA, nrow = k, ncol = 21, dimnames = list(NULL, paste(1:21)))
# have a look at the matrix
cv.errors



# loop over folds
for (a in 1:k){
  
  # best subset selection on training data
  best.fit <- regsubsets(Diabetes_binary ~ ., data = d3[ folds != a, ], nvmax = 21)
  
  # loop over the 21 subsets
  for (b in 1:21){
    
    # predict response for test set for current subset
    pred <- predict(best.fit, d3[ folds == a ,], id = b )
    
    # MSE into container; rows are folds; columns are subsets
    real <- as.numeric(d3$Diabetes_binary[folds==a])
    
    cv.errors[a, b] <- mean( (real - pred)^2 )
    
  } # end of loop over the 16 subsets
} # end of loop over folds
# the cross-validation error matrix
cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

# visualize
par( mfrow = c(1,1) , oma = c(0,0,0,0))
plot( mean.cv.errors, type = "b" )

# run regsubsets on full data set
reg.best <- regsubsets(Diabetes_binary ~ ., data = d3, nvmax = 21)
# coefficients of subset which minimized test error
coef(reg.best, which.min(mean.cv.errors))

# final best model
coef(reg.best, 5)
