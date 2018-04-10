library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(glmnet)

#Starter code
set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#5


thetask <- makeClassifTask(data = income.train, target = "high.earner")
print(thetask)

#Set resampling strategy--3-fold
resample <- makeResampleDesc(method = "CV", iters = 3)

tune <- makeTuneControlRandom(maxit = 10L)

#Six learners
trees <- makeLearner("classif.rpart", predict.type = "response")
logit <- makeLearner("classif.glmnet", predict.type = "response")
nnet <- makeLearner("classif.nnet", predict.type = "response")
nb <- makeLearner("classif.naiveBayes", predict.type = "response")
knn <-makeLearner("classif.kknn", predict.type = "response")
svm <- makeLearner("classif.svm", predict.type = "response")

#6
#Set hyperparameters

#Tree model
parameters.tree <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))

#Logit model 
parameters.logit <- makeParamSet(
  makeNumericParam("lambda",lower=0,upper=3),
  makeNumericParam("alpha",lower=0,upper=1))

#Neural network model
parameters.nn <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = 0.1, upper = 0.5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000))

#KNN
parameters.knn <- makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))

#SVM
parameters.svm  <- makeParamSet(
  makeDiscreteParam("cost", values = 2^c(-2, -1, 0, 1, 2, 10)), 
  makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))

#tune

#tree
tunedModel.trees <- tuneParams(learner = trees,
                               task = thetask,
                               resampling = resample,
                               measures = list(f1, gmean),
                               par.set = parameters.tree,
                               control = tune,
                               show.info = TRUE)

#logit
tunedmodel.logit <- tuneParams(learner = logit,
                               task = thetask,
                               resampling = resample,
                               measures = list(f1, gmean),
                               par.set = parameters.logit,
                               control = tune,
                               show.info = TRUE)
#nn
tunedmodel.nn <- tuneParams(learner = nnet,
                            task = thetask,
                            resampling = resample,
                            measures = list(f1, gmean),      
                            par.set = parameters.nn,
                            control = tune,
                            show.info = TRUE)

#knn
tunedmodel.knn <- tuneParams(learner = knn,
                             task = thetask,
                             resampling = resample,
                             measures = list(f1, gmean),      
                             par.set = parameters.knn,
                             control = tune,
                             show.info = TRUE)

#svm
tunedmodel.svm <- tuneParams(learner = svm,
                             task = thetask,
                             resampling = resample,
                             measures = list(f1, gmean),      
                             par.set = parameters.svm,
                             control = tune,
                             show.info = TRUE)

#Set algorithm
predtrees <- setHyperPars(learner=trees, par.vals = tunedmodel.trees$x)
predlogit <- setHyperPars(learner=logit, par.vals = tunedmodel.logit$x)
prednn    <- setHyperPars(learner=nnet, par.vals = tunedmodel.nn$x)
predknn   <- setHyperPars(learner=knn, par.vals = tunedmodel.knn$x)
predsvm   <- setHyperPars(learner=svm, par.vals = tunedmodel.svm$x)


resultstree  <- resample(learner = predtrees, task = thetask, resampling = resample, measures=list(gmean))
resultslogit <- resample(learner = predlogit, task = thetask, resampling = resample, measures=list(gmean))
resultsnn    <- resample(learner = prednn, task = thetask, resampling = resample, measures=list(gmean))
resultsknn   <- resample(learner = predknn, task = thetask, resampling = resample, measures=list(gmean))
resultssvm   <- resample(learner = predsvm, task = thetask, resampling = resample, measures=list(gmean))


finalmodel.tree  <- train(learner = predtrees, thetask)
finalmodel.logit <- train(learner = predlogit, task = thetask)
finalmodel.nn    <- train(learner = prednn, task = thetask)
finalmodel.knn   <- train(learner = predknn, task = thetask)
finalmodel.nb    <- train(learner = nb, task = thetask)
finalmodel.svm   <- train(learner = predsvm, task = thetask)

#Predict in test
prediction.test.tree  <- predict(finalmodel.tree, newdata = income.test)
prediction.test.logit <- predict(finalmodel.logit, newdata = income.test)
prediction.test.nn    <- predict(finalmodel.nn, newdata = income.test)
prediction.test.knn   <- predict(finalmodel.knn, newdata = income.test)
prediction.test.nb    <- predict(finalmodel.nb, newdata = income.train)
prediction.test.svm   <- predict(finalmodel.svm, newdata = income.test)

#Out of sample f1 and gmean 
performance(prediction.test.tree, measures = list(f1, gmean))
performance(prediction.test.logit, measures = list(f1, gmean))
performance(prediction.test.nn, measures = list(f1, gmean))
performance(prediction.test.knn, measures = list(f1, gmean))
performance(prediction.test.nb, measures = list(f1, gmean))
performance(prediction.test.svm, measures = list(f1, gmean))
