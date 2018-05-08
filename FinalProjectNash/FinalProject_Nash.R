library(readxl)
DSA_5970_Data <- read_excel("~/05Homework/DSA 5970 Data.xlsx")

#print summary stats of relevant variables. If I ask for summary stats of the full dataset,
#it gets hard to read because the data set includes reference columns for cleaning purposes.
View(DSA_5970_Data)

summary(DSA_5970_Data$GRI2015)
sd(DSA_5970_Data$GRI2015)

summary(DSA_5970_Data$GDPpppD)
sd(DSA_5970_Data$GDPpppD)

summary(DSA_5970_Data$Unemp)
sd(DSA_5970_Data$Unemp)

summary(DSA_5970_Data$PopD)
sd(DSA_5970_Data$PopD)

summary(DSA_5970_Data$RuralPer)
sd(DSA_5970_Data$RuralPer)

summary(DSA_5970_Data$BoP)
sd(DSA_5970_Data$BoP)

summary(DSA_5970_Data$Tim)
sd(DSA_5970_Data$Tim)

summary(DSA_5970_Data$Mim)
sd(DSA_5970_Data$Mim)

#regression with GRI as dependent variable. stargazer table created of regression output
relfit <- lm(GRI2015 ~ Tim + Mim + Mus + Jew + Hind + Buddh + PopD + GDPpppD
          + Unemp + BoP, data=DSA_5970_Data)
summary(relfit)
plot(relfit)
stargazer(relfit)

#regression with GDP as dependent variable. stargazer table created
relfit2 <- lm(GDPpppD ~ PopD + GDPg + Unemp + BoP + RuralPer + Tim + 
                Mim + Mus + Jew + Hind + Buddh, data=DSA_5970_Data)
summary(relfit2)
stargazer(relfit2)

#regression with immigration as dependent out of curiosity
relfit3 <- lm(Tim ~ PopD + GDPg + Unemp + BoP + RuralPer + GDPpppD + GRI2015 + Mim + Mus + Jew + Hind + Buddh, data=DSA_5970_Data)
summary(relfit3)

#test regressions for heteroskedasticity
library(lmtest)
bptest(relfit)
bptest(relfit2)

#use robust standard errors to resolve heteroskedasticity in first model
library(sandwich)
coeftest(relfit, vcov = vcovHC(relfit, "HC1"))


#Visualizations for each regression
countries_select <- DSA_5970_Data[DSA_5970_Data$Religion %in% c("Muslim", "Christian", "Hindu", "Buddhist"), ]

# Scatterplot
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(countries_select, aes(GRI2015, Mim)) + 
  labs(subtitle="Examining the Relationship between Muslim Immigration and the GRI",
       title="Government Restrictions on Religion in Various Countries")

g + geom_jitter(aes(col=Religion, size=PopD)) + 
  geom_smooth(aes(col=Religion), method="lm", se=F)

#Second scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(countries_select, aes(GRI2015, GDPpppD)) + 
  labs(subtitle="Examining the Relationship between GDP and the GRI",
       title="Government Restrictions on Religion in Various Countries")

g + geom_jitter(aes(col=Religion, size=PopD)) + 
  geom_smooth(aes(col=Religion), method="lm", se=F)
