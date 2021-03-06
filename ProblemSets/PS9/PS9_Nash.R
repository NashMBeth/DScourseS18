library(mlr)
install.packages("glmnet")
library(glmnet)

# Read in housing data
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
head(housing)
mean(housing$medv)

housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula <- as.formula (lmedv ~ .^3 +
                          poly (crim , 6) +
                          poly (zn , 6) +
                          poly (indus , 6) +
                          poly (nox , 6) +
                          poly (rm , 6) +
                          poly (age , 6) +
                          poly (dis , 6) +
                          poly (rad , 6) +
                          poly (tax , 6) +
                          poly (ptratio , 6) +
                          poly (b, 6) +
                          poly (lstat , 6))
mod_matrix <- data.frame( model.matrix ( formula , housing ))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix )[1] = "lmedv" 
n <- nrow(mod_matrix )
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train ,]
housing.test <- mod_matrix[test , ]

#6
#Estimate OLS

mean(housing$lmedv)
housing$medv <- NULL
housing$dis2 <- housing$dis^2
housing$chasNOX <- housing$crim * housing$nox

# Break up the data:
n <- nrow(housing)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- housing[train,]
housing.test  <- housing[test, ]

theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))
print(sampleResults$aggr)

# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)
#Optimal lambda is 0.029

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))

#In sample RMSE is 0.213.
sqrt(mean((prediction$data$truth-prediction$data$response)^2))
#Out-of-sample RMSE is 0.207.

#7 Ridge Regression
# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

#In-sample RMSE is 0.207
sqrt(mean((prediction$data$truth-prediction$data$response)^2))
#Out-of-sample RMSE is 0.198.
#The optimal lambda is 0.0132. 

#8 Elastic Net
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set
prediction <- predict(finalModel, newdata = housing.test)
#In-sample RMSE is 0.201
performance(prediction, measures = list(rmse))
#Out-of-sample RMSE is 0.192
#The optimal lambda is 0.024. The optimal alpha is 0.198. Since this is closer to 0,
# this leads me to belive that more weight should be placed on the ridge model.
