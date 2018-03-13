library(RCurl)
x <- getURL("https://raw.githubusercontent.com/tyleransom/DScourseS18/master/ModelingOptimization/wages.csv")
wages <- read.csv(text = x)
View(wages)

#Drop observations that are missing hgc and tenure
na.omit(wages, cols=c("hgc", "tenure"))
sum(is.na(wages$hgc))
sum(is.na(wages$tenure))
wages2<-wages[!is.na(wages$hgc),]
sum(is.na(wages2$hgc))
wages3 <- wages2[!is.na(wages2$tenure),]
sum(is.na(wages3$tenure))
View(wages3)

#Produce summary table using stargazer
library(stargazer)
stargazer(wages3)
#Whether logwage is missing or not does not appear to depend on other values; therefore, the
#data is MCAR.

#Various missing value imputation methods

#Complete cases
completecases <- wages3[!is.na(wages3$logwage),]
View(completecases)
ccreg <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=completecases)
summary(ccreg)

#Mean Imputation:
meanimpwage = transform(wages3, logwage = ifelse(is.na(logwage), mean(logwage, na.rm=TRUE), logwage))
View(meanimpwage)
meanimpreg <- lm(logwage ~ hgc + college + tenure +tenure^2 + age + married, data=meanimpwage)
summary(meanimpreg)

#Predicted values imputation
predictedvalwage <- mice(wages3, method = "norm.nob", m = 1)
datapredictedval <- complete(predictedvalwage)
View(datapredictedval)
pvreg <- lm(logwage ~ hgc + college + tenure + tenure^2 + age + married, data=datapredictedval)
summary(pvreg)

#Multiple Imputation
library(mice)
nhances2.imp = mice(wages3, seed = 12345)
summary(nhances2.imp)
mimpreg <- with(nhances2.imp, lm(logwage ~ hgc + college + tenure +tenure^2 + age + married))
round(summary(pool(mimpreg)),2)

#Make stargazer table with all results
library(stargazer)
stargazer(ccreg, meanimpreg, pvreg)
