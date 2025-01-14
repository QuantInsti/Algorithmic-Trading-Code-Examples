# install.packages('TTR')
# install.packages('quantmod')
# install.packages('stats')
# install.packages('lubridate')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('forecast')
# install.packages('vars')

library('TTR')
library('quantmod')
library('stats')
library('lubridate')
library('dplyr')
library('ggplot2')
library('forecast')
library('vars')
library('openxlsx')
library('bvarsv')

# Set tickers
tickers <- c('MSFT', 'AAPL', 'TSLA', 'NFLX', 'META', 'AMZN','GOOGL')

# Set start and end dates
start = "1990-01-01"
end = "2024-08-01"

df <- new.env()

# Import the data
getSymbols(tickers,src='yahoo',env = df, from=start,to=end,auto.assign=TRUE)

# Create dataframe for stock close prices using the Apple date column
df2 <- data.frame(date = index(df[[ls(df)[1]]]))

# Loop to join all the stock close price data in a single dataframe
for (ticker in tickers) {
  # Save the adjusted close price in a temporary dataframe called temp_df
  temp_df <- data.frame(coredata(fortify.zoo(Ad(df[[ticker]]))))
  # Rename the columns
  colnames(temp_df) <- c('date',ticker)
  # Save the adjusted close price in df2
  df2 <- left_join(df2, temp_df[c('date',ticker)], by=c("date"))
}

# Drop the NaN values
df2 <- na.omit(df2)

# Create the returns dataframe
var_data <- data.frame(date=df2$date[-1])

# Loop to create the log returns
for(ticker in tickers) {
  # Compute the log returns for each asset
  var_data[[ticker]] <- log(df2[[ticker]][-1]/df2[[ticker]][-length(df2[[ticker]])])
}

# Create a dataframe to select the returns and not the date column
data <- var_data[,match(tickers,colnames(var_data))]

# Convert the dataframe into a matrix for estimation purposes
tvp_var_data <- as.matrix(var_data[,2:8])

# Set the seed to estimate the TVP-VAR-SV model
set.seed(2024)

# Estimate the TVP-VAR-SV model
bv <- bvar.sv.tvp(tvp_var_data, tau= 250, nf=1, nrep = 300, nburn=20)

# Obtain the forecasts of the model based on the mean of the posterior-distribution draws
forecast_ys <- rowMeans(bv$fc.ydraws[1:7,1,])
forecast_ys
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

# Function to open an XLSX file with error handling
open_xlsx <- function(file_path) {
  # Output the Excel file in case it exists
  result <- tryCatch({
    # Read the Excel file
    data <- read.xlsx(file_path)
    # Set the date column as datetime type
    data$date <- as.Date(data$date, origin = "1899-12-30")
    
    # Return the data
    return(data)
  },
  # Output the error in case the Excel file doesn't exist
  error = function(e) {
    # Print an error message
    message("Error reading file:", e)
    # Return a Null value
    return(NULL)
  })
  # Return the Excel file or a Null value
  return(result)
}

# Group the data dates by year and month
options(dplyr.summarise.inform = FALSE)
dates <- var_data %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(year, month)  %>% summarise(first_dates = first(date))

# Get the first date of Oct-2021
initial_date = subset(dates, (dates$year=='2019') & (dates$month=='01'))$first_dates

# Import the Excel file in case it exists
df_forecasts <- open_xlsx("df_results_tvp_var.xlsx")

# If data is a Null value
if (is.null(df_forecasts)) {
  # Subset the df2 dataframe to only our forecast data
  df_forecasts <- subset(var_data, var_data$date>=as.Date(initial_date))
  
  # Create names for each ticker's VAR forecasts
  ticker_var_forecasts <- lapply(tickers, paste0, "_var_signal")
  
  # Create names for each ticker's TVP-VAR-SV forecasts
  ticker_tvp_var_forecasts <- lapply(tickers, paste0, "_tvp_var_signal")
  
  # Create a new temp_df dataframe for the tickers' forecasts
  temp_df <- data.frame(matrix(ncol = 2*length(tickers), nrow = length(df_forecasts$date)))
  colnames(temp_df) <- c(ticker_var_forecasts,ticker_tvp_var_forecasts)
  
  # Join both dataframes in the df_forecasts dataframe
  df_forecasts <- cbind(df_forecasts,temp_df)
  
  df_forecasts$trade_done <- 0
  
  # Set the initial index number for the forecast data
  initial_iloc_to_forecast <- which(df_forecasts$date==as.Date(initial_date))

  # If data was obtained from the Excel file
} else {
  # Create names for each ticker's VAR forecasts
  ticker_var_forecasts <- lapply(tickers, paste0, "_var_signal")
  
  # Create names for each ticker's TVP-VAR-SV forecasts
  ticker_tvp_var_forecasts <- lapply(tickers, paste0, "_tvp_var_signal")
  
  # Set the initial index number for the forecast data
  initial_iloc_to_forecast <- which(df_forecasts$date == head(subset(df_forecasts, trade_done == 0) ,1)$date)
}

# Set the span to 6 years of data
span <- 1500

print(paste0(strrep('=',50)))
print(paste0(strrep('=',50)))
print(paste0(strrep('=',50)))
print(paste0('Estimation of basic-VAR forecasts'))

# Check if VAR forecasts have already been estimated
if (length(as.numeric(rownames(tail(subset(df_forecasts, trade_done == 1) ,1))))==0) {
  
  print(paste0(strrep('=',50)))
  print(paste0(strrep('=',50)))
  print(paste0(strrep('=',50)))
  print(paste0('Estimation of VAR forecasts'))
  
  if (length(initial_iloc_to_forecast<nrow(df_forecasts))!=0) {
    # The for loop to estimate the model each day
    for (i in initial_iloc_to_forecast:nrow(df_forecasts)) {
      
      # Set the current iteration date
      iter_date <- df_forecasts[i,'date']
      # Print the date
      print(paste0(strrep('=',50)))
      print(paste0("Date is ",iter_date))
  
      # Select the the 6 years of data to train the model
      data <- subset(var_data, var_data$date<iter_date)[,match(tickers,colnames(var_data))]
  
      # Estimate up to the 15-lag VAR with no constants or trends
      lagselect <- VARselect(data, lag.max = 15, type = "none")
      
      # Choose the best lag order as per the Bayesian Information Criteria
      lag_order <- lagselect$selection[3]
      
      # Estimate the best VAR model
      model <- VAR(data, p = lag_order, type = "none", season = NULL, exog = NULL) 
      
      # Forecast one step ahead
      var_forecast <- predict(model, n.ahead = 1)
      
      # We go long if the forecast return is positive, otherwise, we make no position.
      for (ticker in tickers) {
        df_forecasts[i,paste0(ticker,'_var_signal')] = if (var_forecast$fcst[[ticker]][1]>=0) 1 else next
      }
      
      # Print the signals
      for (ticker in tickers) {
        print(paste0(ticker," var signal is ",df_forecasts[i,paste0(ticker,'_var_signal')]))
      }
    }
  } else {
    print(paste0('Estimation of TVP-VAR-SV forecasts has been completed before...'))
  }
}

print(paste0(strrep('=',50)))
print(paste0(strrep('=',50)))
print(paste0(strrep('=',50)))
print(paste0('Estimation of TVP-VAR-SV forecasts'))

if (length(initial_iloc_to_forecast<nrow(df_forecasts))!=0) {
  # The for loop to estimate the model each day
  for (i in initial_iloc_to_forecast:nrow(df_forecasts)) {
    
    # Set the current iteration date
    iter_date <- df_forecasts[i,'date']
    # Print the date
    print(paste0(strrep('=',50)))
    print(paste0("Date is ",iter_date))
    
    # Select the the 6 years of data to train the model
    data <- as.matrix(subset(var_data, date<iter_date)[,match(tickers,colnames(var_data))])
  
    bv <- bvar.sv.tvp(data, nf=1, tau=40, nrep = 300, nburn=20)
    forecast_ys <- rowMeans(bv$fc.ydraws[1:7,1,])
    print(paste0('forecast_ys'))
    print(paste0(forecast_ys))
    
    # We go long if the forecast return is positive, otherwise, we make no position.
    for (c in 1:length(tickers)) {
      df_forecasts[i,paste0(tickers[c],'_tvp_var_signal')] = if (forecast_ys[c]>0) 1 else next
    }
    
    # Set the trade_done value as 1 to record up to which day we have progressed
    df_forecasts[i,'trade_done'] <- 1
    
    # Print the signals
    for (ticker in tickers) {
      print(paste0(ticker," tvp-var-sv signal is ",df_forecasts[i,paste0(ticker,'_tvp_var_signal')]))
    }
    # Save the df_forecasts dataframe
    dataframe <- data.frame(coredata(df_forecasts))
    write.xlsx(dataframe, 'df_results_tvp_var.xlsx')
  }
} else {
  print(paste0('Estimation of TVP-VAR-SV forecasts has been completed before...'))
}

# Create the VAR-based equally-weighted portfolio returns
df_forecasts$var_stra_returns <- rowMeans((df_forecasts[,match(tickers,colnames(df_forecasts))] *
                                        df_forecasts[,match(ticker_var_forecasts,colnames(df_forecasts))]), 
                                      na.rm=TRUE)

# Set the NaN values of the strategy returns to zero
df_forecasts$var_stra_returns[is.na(df_forecasts$var_stra_returns)] = 0.0

# Create the strategy cumulative returns
df_forecasts$var_stra_cum_returns <- exp(cumsum(df_forecasts$var_stra_returns))

# Create the TVP-VAR-SV-based equally-weighted portfolio returns
df_forecasts$tvp_var_sv_stra_returns <- rowMeans((df_forecasts[,match(tickers,colnames(df_forecasts))] *
                                             df_forecasts[,match(ticker_tvp_var_forecasts,colnames(df_forecasts))]), 
                                          na.rm=TRUE)

# Set the NaN values of the TVP-VAR-SV strategy returns to zero
df_forecasts$tvp_var_sv_stra_returns[is.na(df_forecasts$tvp_var_sv_stra_returns)] = 0.0

# Create the TVP-VAR-SV strategy cumulative returns
df_forecasts$tvp_var_sv_stra_cum_returns <- exp(cumsum(df_forecasts$tvp_var_sv_stra_returns))

# Create the equally-weighted benchmark portfolio returns
df_forecasts$bnh_returns <- rowMeans(df_forecasts[,match(tickers,colnames(df_forecasts))])

# Create the equally-weighted benchmark portfolio cumulative returns
df_forecasts$bnh_cum_returns <- exp(cumsum(df_forecasts$bnh_returns))

# Convert the date column in date type
df_forecasts$date <- as.Date(df_forecasts$date)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

# Create a 15-day moving average long signal
df_forecasts$ma_signal <- ifelse(df_forecasts$bnh_cum_returns>rollapply(df_forecasts$bnh_cum_returns,15,mean,fill=1,align='right'),1,0)

# Buy the assets only when the VAR predict a positive return and the 200-day moving average signal is long too
df_forecasts$stra_improved_returns <- c(0,df_forecasts$ma_signal[-length(df_forecasts$ma_signal)]*df_forecasts$tvp_var_sv_stra_returns[-1])

# Compute the improved strategy returns
df_forecasts$stra_improved_cum_returns <- exp(cumsum(df_forecasts$stra_improved_returns))

# Plot the both buy-and-hold and the strategy cumulative returns
ggplot(data=df_forecasts, aes(x = date)) +
  geom_line(aes(y = var_stra_cum_returns, color="VAR")) +
  geom_line(aes(y = tvp_var_sv_stra_cum_returns, color="TVP-VAR-SV")) +
  geom_line(aes(y = bnh_cum_returns,color="B&H")) +
  geom_line(aes(y = stra_improved_cum_returns, color="Improved TVP-VAR-SV")) +
  ggtitle("Buy and Hold and Strategies' Cumulative Returns") + xlab("Date") + ylab("Returns") + 
  theme(plot.title = element_text(hjust = 0.5, size=25), legend.position="bottom", legend.text = element_text(size=20)) + 
  scale_x_date(date_labels = "%b %y") +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 20),)
ggsave("strategies-cumulative-returns.png")

# Save the dataframe
dataframe <- data.frame(coredata(df_forecasts))
# Save the dataframe in an excel file
write.xlsx(dataframe, 'df_results_tvp_var.xlsx')