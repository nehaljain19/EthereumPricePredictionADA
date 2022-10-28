library(dplyr)
library(ghql)
library(jsonlite)
library(ggplot2)

# disable scientific notation
options(scipen=999)

# Point to correct subgraph URL
#Uniswap V3 GraphQL endpoint link - https://thegraph.com/hosted-service/subgraph/uniswap/uniswap-v3
conn = GraphqlClient$new(
  url = "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v3")

# initialize a new query
graphql_request = Query$new()

# make request
graphql_request$query('mydata', '{
        tokenDayDatas(orderBy: date, orderDirection: desc, first: 1000, where: {token: "0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2"}) {
            id
            date
            priceUSD
            volume
            volumeUSD
            token {
              symbol
            }
            open
            high
            low
            close
            totalValueLockedUSD
            }
        }
')

# Run query (pull data)
wethToken = conn$exec(graphql_request$queries$mydata)
# convert results to JSON
wethToken = fromJSON(wethToken)
# extract result
wethToken = as.data.frame(wethToken$data$tokenDayDatas)

# remove row names
rownames(wethToken) = NULL

# convert date to datetime
wethToken = mutate(wethToken,
                   date = as.POSIXct(as.numeric(wethToken$date), origin="1970-01-01"))

wethToken$date <- as.Date(wethToken$date)

#Cleaning the data by dropping column - token
wethToken = select(wethToken, -token)

head(wethToken)

# write data 
write.csv(wethToken,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\AdvancedDataAnalytics\\Project\\R_NEW_WETHDailyDayData.csv")

# Visualize data
p <- ggplot(eth, aes(x=date, y=priceUSD)) +
  geom_line() + 
  xlab("")
p
