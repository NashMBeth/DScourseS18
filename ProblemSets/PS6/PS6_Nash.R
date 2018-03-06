library(tidyquant)

#Get data
stock_list <- tibble(stock = c("AMZN","BKS"))
View(stockinfo)
start <- "2010-01-01"
end <- "2018-01-01"
stockinfo <- stock_list %>%
  tq_get(get = "stock.prices", from = start, to = end)
order.by=as.POSIXct(stockinfo$date)

#Calculate yearly returns
stk <- as.tbl(stockinfo)
stk$date <- as.Date(stk$date)
stockinfo_returns_yearly <- stk %>% group_by(stock) %>% tq_transmute(ohlc_fun=Ad, mutate_fun = periodReturn, period = "yearly", col_rename = "AnnualReturns")
stockinfo_returns_yearly

#Visualize the data
stockinfo_returns_yearly %>%
  ggplot(aes(x = year(date), y = AnnualReturns, fill = stock)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Comparison of Amazon and Barnes and Noble Yearly Returns",
       y = "Returns", x = "", color = "") +
  scale_y_continuous(labels = scales::percent) +
  theme_tq() +
  scale_fill_tq()+theme(plot.title = element_text(hjust = 0.5))

>library(RColorBrewer)
>rf <- colorRampPalette(rev(brewer.pal(40,'Set3')))
>hexbinplot(diamonds$price~diamonds$carat, data=diamonds, colramp=rf)

#2

Alcohol_Drug_Age$druguse <- sum(Alcohol_Drug_Age$marijuana0use, Alcohol_Drug_Age$cocaine_use, Alcohol_Drug_Age$crack0use, Alcohol_Drug_Age$heroin_use, Alcohol_Drug_Age$hallucinogen_use, Alcohol_Drug_Age$inhalant0use, Alcohol_Drug_Age$meth0use)
View(Alcohol_Drug_Age)
ggplot(data = Alcohol_Drug_Age, aes(x = age, y = heroin_use)) +
  geom_point(aes(color = age))

#3
library(ggplot2)
theme_set(theme_bw())

library(ggplot2)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

countries_select <- HON_3993_GRI[HON_3993_GRI$Religion %in% c("Muslim", "Christian", "Hindu", "Buddhist"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(countries_select, aes(Unemp, GRI2015)) + 
  labs(subtitle="Examining the Relationship between Unemployment and the GRI",
       title="Government Restrictions on Religion in Various Countries")

g + geom_jitter(aes(col=Religion, size=PopD)) + 
  geom_smooth(aes(col=Religion), method="lm", se=F)
