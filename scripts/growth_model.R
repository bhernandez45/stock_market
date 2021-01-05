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
#install.pacjages("cronR")

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
ticker_dataset = read.csv("https://raw.githubusercontent.com/bhernandez45/stock_market/master/data_sets/yahoo_finance/tickers.csv")

# grabbing just the names of the tickers from ticker_dataset
ticker_dataset[,1]=as.character(ticker_dataset[,1]) # the variable is set to as.factor by default
tickers=unique(ticker_dataset[,1][ticker_dataset$User == "Bryan"])  # pulls the unique values from the first column

# getting the past ticker information based on dates
from_date = "2010-01-01"
to_date = Sys.Date()
prices <- tq_get(tickers,
                 from = from_date,
                 to = to_date, # change to current day date
                 get = "stock.prices")

# cleaning/adding variables to the prices dataset
smallint=.0000000001 
prices$open[prices$open==0]=smallint # adding a values to companys open value if they open within the specified time range
prices$volume[prices$volume ==0] = NA # recoding to NA for days with no trades

prices$diff=prices$close-prices$open # close - open (+ indicate closed higher than open)

prices$perc_diff=prices$diff/prices$open # percent difference
prices$perc_diffXvol=prices$perc_diff*prices$volume # percent difference * volume

prices$perc_diff[prices$open==smallint]=NA # recoding 

prices$success=NA
prices$success[prices$diff >= 0]=1 # Binary code for up or down
prices$success[prices$diff < 0]=0 # Binary code for up or down


# creating subsets of the prices data for each ticker
tickers_matrix=t(as.matrix(unique(prices$symbol)))

for (i in 1:length(tickers_matrix)) {
  ticker_sub=tickers_matrix[1,i] # ticker we're subsetting by
  ticker_sub2=gsub("[^[:alnum:][:blank:]?&/\\-]", "", ticker_sub) # removes non alpha-numeric characters
  
  prices_sub=prices[which(prices$symbol==ticker_sub),] #subsets based on the ticker
  
  # adding a variable for the day 1-nrow
  prices_sub= prices_sub[order(prices_sub[,"date"]),] # order data (ascending)
  prices_sub$day=seq(from=1, to=nrow(prices_sub)) # create a sequence for "time"
  
  prices_sub=mutate(prices_sub, pd_open=lag(open)) # pd=previous day
  prices_sub=mutate(prices_sub, pd_close=lag(close)) # pd=previous day
  prices_sub=mutate(prices_sub, pd_perc_diff=lag(perc_diff)) # pd=previous day
  prices_sub=mutate(prices_sub, pd_perc_diffXvol=lag(perc_diffXvol)) # pd=previous day
  
  prices_sub=mutate(prices_sub, l5_open=rollmean(x=open, 5, align = "right", fill = NA)) # l5= last 5 trading days (approx weekly average)
  prices_sub=mutate(prices_sub, l5_close=rollmean(x=close, 5, align = "right", fill = NA)) # l5= last 5 trading days (approx weekly average)
  prices_sub=mutate(prices_sub, l5_perc_diff=rollmean(x=perc_diff, 5, align = "right", fill = NA)) # l5= last 5 tradings days (approx weekly average)
  
  prices_sub=mutate(prices_sub, l20_open=rollmean(x=open, 20, align = "right", fill = NA)) # l20= last 20 trading days (approx monthly average: 5 tradings days per week * 4 weeks)
  prices_sub=mutate(prices_sub, l20_close=rollmean(x=close, 20, align = "right", fill = NA)) # l20= last 20 trading days (approx monthly average: 5 tradings days per week * 4 weeks)
  prices_sub=mutate(prices_sub, l20_perc_diff=rollmean(x=perc_diff, 20, align = "right", fill = NA)) # l20= last 20 trading days (approx monthly average: 5 tradings days per week * 4 weeks)

  prices_sub=mutate(prices_sub, l260_open=rollmean(x=open, 260, align = "right", fill = NA)) # l260= last 260 trading days (approx yearly average: 5 tradings days per week * 52 weeks)
  prices_sub=mutate(prices_sub, l260_close=rollmean(x=close, 260, align = "right", fill = NA)) # l260= last 260 trading days (approx yearly average: 5 tradings days per week * 52 weeks)
  prices_sub=mutate(prices_sub, l260_perc_diff=rollmean(x=perc_diff, 260, align = "right", fill = NA)) # l260= last 260 trading days (approx yearly average: 5 tradings days per week * 52 weeks)
  
  prices_sub$pd_perc_diff_cmc=prices_sub$pd_perc_diff-prices_sub$l20_perc_diff # cmc = cluster mean centered
    
  assign(ticker_sub2, prices_sub) # calls the ticker name and assigns it to the dataset
  if (i==1) 
    {
      prices_rebind=prices_sub
    } 
  else if (i>1)
    {
      prices_rebind=rbind(prices_rebind, prices_sub)
    }
}



# analyses
cor(prices_rebind)

# building the growth model:
# unconditional growth model
m0=lm(perc_diff~day, data=prices_rebind)
#m0_re=as.data.frame(VarCorr(m0)) # grabbing the random effects
#m0_icc=m0_re[1,"vcov"]/(m0_re[1,"vcov"]+m0_re[2,"vcov"]) # percent variance accounted for based on grouping

m1=lmer(perc_diff~day + (1|symbol), data=prices_rebind)
m1_re=as.data.frame(VarCorr(m1)) # grabbing the random effects
m1_icc=m1_re[1,"vcov"]/(m1_re[1,"vcov"]+m1_re[2,"vcov"]) # percent variance accounted for based on grouping

m2=lmer(perc_diff~day + open + pd_perc_diffXvol + (1|symbol), data=prices_rebind)
m2_re=as.data.frame(VarCorr(m2)) # grabbing the random effects
m2_icc=m2_re[1,"vcov"]/(m2_re[1,"vcov"]+m2_re[2,"vcov"]) # percent variance accounted for based on grouping


# plots growth
#prices %>%
 # ggplot(aes(x = date, y = adjusted, color = symbol)) +
  #geom_line() +
  #facet_wrap(~symbol,scales = 'free_y') +
  #theme_classic() +
  #labs(x = 'Date',
   #    y = "Adjusted Price",
    #   title = "Price Chart") +
  #scale_x_date(date_breaks = "month",
   #            date_labels = "%b\n%y")
