#   Creating CSV Files
# Historical Ticker Info

#packages to install
#install.packages("quantmod")
#install.packages("tidyquant")
#install.packages("dplyr")

#library needed
library(tidyquant)
library(ggplot2)
library(dplyr)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# loading tickers to fetch
ticker_dataset = read.csv("https://raw.githubusercontent.com/bhernandez45/stock_market/master/data_sets/other_data/tickers.csv")

####HELP!!!!!!
##trying to load in ticker data and convert to string formatting like "AAPL","TSLA"...etc

tickers_rows = top_n(as.data.frame(ticker_dataset$Ticker),10)

test_prices <- tq_get(tickers_rows,
                      from = "2019-01-01",
                      to = "2020-01-01",
                      get = "stock.prices"
                      )
                     
                     
                     

tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

prices <- tq_get(tickers,
                 from = "2017-01-01",
                 to = "2017-03-01",
                 get = "stock.prices")

head(AAPL)

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















#install.packages("lme4")
library(lme4)


corr(finance_data)

# reccodes and cleaning
#finance_data$time=
#finance_data$success[finance_data$close>0]=1
#finance_data$success[finance_data$close<0]=0


# unconditional growth model
m0=lmer(success~ 1 + (1|time), data=finance_data, family="binomial")

m1=lmer(success~1 + (1|time), data=finance_data, family="binomial")

icc_mod_fin=
