#Code to fetch the data from The Graph's endpoint for analysis. The transactions are filtered with Token ID = 0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2
#This is the token ID for Wrapped ETH (WETH) : https://etherscan.io/token/0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2

library(dplyr)
library(ghql)
library(jsonlite)
library(plotly)

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
#Dropping 'volume' which is in ETH as we are already capturing Volume in USD #Dropping price in USD as it is duplicate of 'Close'
wethToken = select(wethToken, -c(volume,priceUSD))

#Cleaning the data by dropping discrepancy value = 0
wethToken = subset(wethToken, low> 0)
head(wethToken)

# Write data into CSV
write.csv(wethToken,"C:\\Users\\Nehal\\OneDrive\\Documents\\MSBA_StudyMaterial\\Fall2022\\AdvancedDataAnalytics\\Project\\WETHDailyDayData.csv")


# Visualize data
# Visualizing a timeseries - Date vs Low using Plotly
fig1 <- plot_ly(wethToken, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~date, y = ~low)%>%
  layout(showlegend = F, title='Time Series: Date vs Daily Low of WETH (in USD)',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))

fig1 <- fig1 %>%
  layout(
  xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    
    plot_bgcolor='#e5ecf6', width = 900)
fig1


# Visualizing a timeseries - Date vs volumeUSD using Plotly
fig2 <- plot_ly(wethToken, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~date, y = ~volumeUSD)%>%
  layout(showlegend = F, title='Time Series: Date vs Daily Volume of WETH (in USD)',
         xaxis = list(rangeslider = list(visible = T),
                      rangeselector=list(
                        buttons=list(
                          list(count=1, label="1m", step="month", stepmode="backward"),
                          list(count=6, label="6m", step="month", stepmode="backward"),
                          list(count=1, label="YTD", step="year", stepmode="todate"),
                          list(count=1, label="1y", step="year", stepmode="backward"),
                          list(step="all")
                        ))))

fig2 <- fig2 %>%
  layout(
    xaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    
    yaxis = list(zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor = 'ffff'),
    
    plot_bgcolor='#e5ecf6', width = 900)
fig2
