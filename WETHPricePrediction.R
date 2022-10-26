library(dplyr)
library(ggplot2)

# disable scientific notation
options(scipen=999)

#Reading the csv into R
eth <- read.csv("C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\AdvancedDataAnalytics\\Project\\WETHDailyDayData.csv", header=TRUE, stringsAsFactors=TRUE)

#Data Cleaning using Dplyr
eth = select(eth, -X)
eth = select(eth, -token)

head(eth)

# Visualize data
p <- ggplot(eth, aes(x=date, y=priceUSD)) +
  geom_line() + 
  xlab("")
p
