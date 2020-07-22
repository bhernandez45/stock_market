#   Creating CSV Files
# Historical Ticker Info

# packages to install

#install.packages("lattice") # need for quantmod
#install.packages("zoo") # need for quantmod
#install.packages("xts") # need for quantmod
#install.packages("quantmod")
#install.packages("tidyquant")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lme4")

# librarys needed

library(quantmod)
library(tidyquant)
library(ggplot2)
library(dplyr)
library(lme4)

# getting past ticker information
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# loading tickers to fetch
ticker_dataset = read.csv("https://raw.githubusercontent.com/bhernandez45/stock_market/master/data_sets/other_data/tickers.csv")

# grabbing just the names of the tickers from ticker_dataset
ticker_dataset[,1]=as.character(ticker_dataset[,1]) # the variable is set to as.factor by default
tickers=unique(ticker_dataset[,1])  # pulls the unique values from the first column

# getting the past ticker information based on dates
prices <- tq_get(tickers,
                 from = "2017-01-01",
                 to = "2017-03-01",
                 get = "stock.prices")

# adding variables to the prices dataset
prices$diff=prices$close-prices$open
prices$perc_diff=prices$diff/prices$open

# creating subsets of the prices data for each ticker
tickers_matrix=t(as.matrix(unique(prices$symbol)))

for (i in 1:length(tickers_matrix)) {
  ticker_sub=tickers_matrix[1,i] # ticker we're subsetting by
  ticker_sub2=gsub("[^[:alnum:][:blank:]?&/\\-]", "", ticker_sub) # removes non alpha-numeric characters
  prices_sub=prices[which(prices$symbol==ticker_sub),] #subsets based on the ticker
  
  # adding a variable for the day 1-nrow
  prices_sub$day=seq(from=1, to=nrow(prices_sub))
  
  prices_sub=mutate(prices_sub, pd_open=lag(open)) # pd=previous day
  prices_sub=mutate(prices_sub, pd_close=lag(close)) # pd=previous day
  prices_sub=mutate(prices_sub, pd_per_dif=lag(perc_diff)) # pd=previous day
  
  prices_sub=mutate(prices_sub, l5_open=rollmean(x=open, 5, align = "right", fill = NA)) # l5= last 5 trading days (approx weekly average)
  prices_sub=mutate(prices_sub, l5_open=rollmean(x=close, 5, align = "right", fill = NA)) # l5= last 5 trading days (approx weekly average)
  prices_sub=mutate(prices_sub, l5_open=rollmean(x=perc_diff, 5, align = "right", fill = NA)) # l5= last 5 tradings days (approx weekly average)
  
  prices_sub=mutate(prices_sub, l20_open=rollmean(x=open, 20, align = "right", fill = NA)) # l5= last 20 days (approx monthly average: 5 tradings days per week * 4 weeks)
  prices_sub=mutate(prices_sub, l20_open=rollmean(x=close, 20, align = "right", fill = NA)) # l5= last 20 days (approx monthly average: 5 tradings days per week * 4 weeks)
  prices_sub=mutate(prices_sub, l20_open=rollmean(x=perc_diff, 20, align = "right", fill = NA)) # l5= last 20 days (approx monthly average: 5 tradings days per week * 4 weeks)

  prices_sub=mutate(prices_sub, l260_open=rollmean(x=open, 260, align = "right", fill = NA)) # l5= last 260 days (approx yearly average: 5 tradings days per week * 52 weeks)
  prices_sub=mutate(prices_sub, l260_open=rollmean(x=close, 260, align = "right", fill = NA)) # l5= last 260 days (approx yearly average: 5 tradings days per week * 52 weeks)
  prices_sub=mutate(prices_sub, l260_open=rollmean(x=perc_diff, 260, align = "right", fill = NA)) # l5= last 260 days (approx yearly average: 5 tradings days per week * 52 weeks)
  
  assign(ticker_sub2, prices_sub) # calls the ticker name and assigns it to the dataset
}

# bind the data back together with the new information
#tickers_matrix2=gsub("[^[:alnum:][:blank:]?&/\\-]", "", tickers_matrix) # removes non alpha-numeric characters
#tickers_matrix2=as.vector(tickers_matrix2, mode=list)
#subset_data=noquote(tickers_matrix2[1,1:ncol(tickers_matrix2)])
#rbind(c(noquote(tickers_matrix2)))

# plots growth
prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  facet_wrap(~symbol,scales = 'free_y') +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Price Chart") +
  scale_x_date(date_breaks = "month",
               date_labels = "%b\n%y")


# analyses
cor(prices)

# reccodes and cleaning
#finance_data$time=
#finance_data$success[finance_data$close>0]=1
#finance_data$success[finance_data$close<0]=0

# building the growth model:
# unconditional growth model
m0=lmer(success~ 1 + (1|time), data=finance_data, family="binomial")

m1=lmer(success~1 + (1|time), data=finance_data, family="binomial")

# ICC is the proportion of variance attributed to the grouping structure
# within / total variance
#icc_mod_fin=
