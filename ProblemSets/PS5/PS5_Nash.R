install.packages("rvest")
library(rvest)
webpage1 <- "https://coinmarketcap.com/all/views/all/" #read in webpage
webpage1
webpage <- webpage1 %>% read_html()
webpage
webpage2 <- webpage %>% html_nodes(xpath = '//*[@id="currencies-all"]') #parse out data of interest
webpage3 <- webpage2 %>% html_table() %>% data.frame() #store as data frame
webpage4 <- webpage3[,-1] #take out index columm
head(webpage4)

#remove unwanted characters
cleaname <- strsplit(webpage4$Name,"\n")
cleaname <- sapply(cleaname,`[`,2)
cleaname <- gsub(" ","-",cleaname)
webpage4$Name <- cleaname
str(webpage4) #check and see if it worked

#Trying this for the Corruption Index
transparency_url <- "https://www.transparency.org/cpi2012/results"
transparency <- transparency_url %>% read_html()
transparency <- transparency %>% html_nodes(xpath = '//*[@id="flex1"]')
transparency

transparency <- transparency %>% html_table() %>% data.frame() #this command doesn't work--will return to it later and figure out why. Use code above for official assignment.
transparency



#4 Finance data using tidyquant

install.packages("tidyquant")
install.packages("corrr")
library(tidyquant)
library(broom)
library(corrr)

quandl_api <- ("Z1KSJSG-Ssm7yUoXwyz6")
quandl_api_key(quandl_api)

stock_list_quandl <- tribble(
  ~code,          ~symbol,
  "WIKI/AMZN", "AMZN",
  "WIKI/F",    "F",
  "WIKI/GE",   "GE",
  "WIKI/MSFT", "MSFT"
)
stock_list_quandl #Make sure all tickers are accurate

stock_returns_quandl <- stock_list_quandl %>%
  tq_get(get          = "quandl",
         from         = "2016-01-01",
         to           = "2017-12-31",
         transform    = "rdiff",
         collapse     = "monthly",
         column_index = 11) %>%
  rename(monthly.returns = adj.close)
stock_returns_quandl
stock_returns_quandl_df <- as.data.frame(stock_returns_quandl)
head(stock_returns_quandl_df)
stock_returns_quandl_df
