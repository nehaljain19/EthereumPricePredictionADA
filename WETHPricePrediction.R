#Code to fetch the data from The Graph's endpoint for analysis. The transactions are filtered with Token ID = 0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2
#This is the token ID for Wrapped ETH (WETH) : https://etherscan.io/token/0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2

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

# Initializing a new query
graphqlRequest = Query$new()

# Fetching top records ordered by date
# Set query (which uses text input to specify ETH wallet addresses)
graphqlRequest$query('newData', '{
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
wethToken = conn$exec(graphqlRequest$queries$newData)
# JSON adjustment
wethToken = fromJSON(wethToken)
# Extract JSON to convert to a dataframe
wethToken = as.data.frame(wethToken$data$tokenDayDatas)

# Remove row names
rownames(wethToken) = NULL

# Convert UNIX date to datetime
wethToken = mutate(wethToken,
                   date = as.POSIXct(as.numeric(wethToken$date), origin="1970-01-01"))
# Remove the timestamp and just keep date
wethToken$date <- as.Date(wethToken$date)

#Cleaning the data by dropping column - 'token'
wethToken = select(wethToken, -token)

head(wethToken)

# Write data into CSV
write.csv(wethToken,"WETHDailyDayData.csv")

# Visualize data
ethTS <- ts(wethToken, frequency = 12)
plot.ts(ethTS)
