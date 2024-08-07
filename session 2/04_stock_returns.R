library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(here)
library(tidyquant)
library(rvest)    # scrape websites
library(purrr)  
library(lubridate) #to handle dates
library(janitor)
library(ggrepel)
library(plotly)

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"

#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF



myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = Sys.Date() - 3*365, # last 3 years of data
         to   = Sys.Date()) %>% # today
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame


#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

# histogram for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  geom_histogram()+
  facet_wrap(~symbol)+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.25),
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Over the last four years",
    x=""
  )


# boxplot for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns, y=symbol))+
  geom_boxplot()+
  theme_bw()+
  scale_x_continuous(
    breaks = seq(-0.5, 0.9, by = 0.1),
    labels = scales::percent_format(accuracy=.1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Over the last four years",
    x=""
  )


# ECDF for monthly returns for each stock
myStocks_returns_monthly %>% 
  filter(symbol!= "SPY") %>% 
  ggplot(aes(x= monthly_returns))+
  stat_ecdf()+
  facet_wrap(~symbol, scales = "free")+
  theme_bw()+
  scale_x_continuous(
    labels = scales::percent_format(accuracy=1))+
  labs(
    title = "Monthly returns of the 30 DJIA stocks",
    subtitle = "Over the last four years",
    x="", y= ""
  )+
  theme(legend.position="none")


# Create a dataframe that summarises monthly returns for each of the stocks and `SPY`; 
# min, max, median, mean, SD, and CI for mean
monthly_summaries <- myStocks_returns_monthly %>% 
  group_by(symbol) %>% 
  summarise(
    min = min(monthly_returns),
    max = max(monthly_returns), 
    mean_return = mean(monthly_returns),
    sd_return = sd(monthly_returns),
    count = n(),
    se_return = sd_return / sqrt(count),
    t_critical = qt (0.975, count - 1),
    lower_95 = mean_return - t_critical * se_return,
    upper_95 = mean_return + t_critical * se_return,
  ) %>% 
  arrange(desc(mean_return))

monthly_summaries




