## Data Science in Practice Hands-on Workshop: Data acquisition

# Plotting time series (stock example)
install.packages(c("quantmod", "ggplot2", "magrittr","broom", "tidyverse"))
lapply(c("quantmod", "ggplot2", "magrittr","broom","tidyverse"), require, character.only = TRUE)
library(quantmod)
library(ggplot2)
library(magrittr)
library(broom)

## Getting stock data
# Setting time period
# This is for getting a balanced data set, i.e. all series have data from same start and end dates

start = as.Date("2020-01-01") 
end = as.Date("2022-07-12")

# Collect stock names from Yahoo Finance
getSymbols(c("BABA","JD","PDD"), src = "yahoo", 
             from = start, to = end)
chartSeries(BABA)
chart_Series(BABA)

## Plotting multiple series using ggplot2
chinastocks = as.xts(data.frame(BABA  =BABA[,"BABA.Adjusted"], JD=JD[,"JD.Adjusted"], 
                                PDD=PDD[,"PDD.Adjusted"]))
           
# Index by date
names(chinastocks) = c("Alibaba", "JD/Jindong", "PDD/Pinduoduo")
index(chinastocks) = as.Date(index(chinastocks))

library(tidyverse)

# Plot1
stocks_series = tidy(chinastocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(cex=1) +
  theme_bw()
stocks_series

# Plot2
stocks_series = tidy(chinastocks) %>% 
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line(cex=1) +
  theme_bw() +
  labs(title = "Daily Stock Prices, 01/1/2020 - 07/12/2022",
     subtitle = "End of Day Adjusted Prices",
     caption = "Source: Yahoo Finance") +
  xlab("Date") + ylab("Price") +
  scale_color_manual(values = c("steelblue", "firebrick","purple")) +
  theme(text = element_text(family = "Palatino"), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.position="top")
stocks_series


## COVID data: Our World in Data
install.packages("vroom") # vroom reads data super fast!
library(vroom)
# Reading all real time data
# vroom is the champion in reading github date, < 3 sec.
owidall = vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true")

# Get today's COVID data
owidtoday = subset(vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"), date == Sys.Date())

# Subset by year
owid2022 = subset(owidall, format(as.Date(date),"%Y")==2022)
owid2021 = subset(owidall, format(as.Date(date),"%Y")==2021)
owid2020 = subset(vroom("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"), format(as.Date(date),"%Y")==2020)

# Subset by country: United States
owidus = subset(owidall, location=="United States")

# Subset by continent
owideu = subset(owidall, continent=="Europe")
owidasia = subset(owidall, continent=="Asia")

options(scipen=999)
par(family = "Palatino")

# US data
format(owidus$new_cases, big.mark = ",", scientific = FALSE)
y = owidus$new_cases
x = as.Date(owidus$date)
plot(x,y, pch=20, col="steelblue", cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Cases in US (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)

# Europe data: label countries
format(owideu$new_deaths, big.mark = ",", scientific = FALSE)
y = owideu$new_deaths
x = as.Date(owideu$date)
plot(x,y, pch=20, col="#E7298A", cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Deaths in Europe (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)
identify(x,y,owideu$location, ps=8, atpen=TRUE)


# Asia data
y = owidasia$new_deaths
x = as.Date(owidasia$date)
plot(x,y, pch=20, col="#66A61E",  cex = .5, xaxt='n', xlab = "Date", ylab = "COVID Deaths in Asia (Daily)")
axis(1, x, format(x, "%Y-%m"), cex.axis = .7, las = 3 , gap.axis =1.5, tick = FALSE)
identify(x,y,owidasia$location, ps=8, atpen=TRUE)

# Clean up OWID*  cases
# Deselect cases/rows with OWID

owid2022 = owid2022[!grepl("^OWID", owid2022$iso_code), ] 
owid2021 = owid2021[!grepl("^OWID", owid2021$iso_code), ]
owid2020 = owid2020[!grepl("^OWID", owid2020$iso_code), ]
