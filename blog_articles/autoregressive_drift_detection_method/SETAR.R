# install.packages('TTR')
# install.packages('quantmod')
# install.packages('lubridate')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('R.utils')
# install.packages('openxlsx')
# install.packages('tidyverse')
# install.packages('lmtest')
# install.packages('tsDyn')

# No warnings
options(warn=0)
options(xts.warn_dplyr_breaks_lag = FALSE)

# For data manipulations
library('openxlsx')
library('R.utils')
library('tsDyn')

# A wrapper function to capture longer-than-usual estimations
my_wrapper_func <- function(model_func, data) {
  result = tryCatch(expr = withTimeout(model_func(data, m=5, thDelay=2, nthresh=1), timeout = 10*60), 
                    # Make the function return FALSE in case there is an error while estimating the model
                    error=function( err ) FALSE)
  return(result)
}

# Set the working directory. Change the address as per where your folder is located in your PC
setwd("/home/josgt/Documents/EPAT/Blog Articles/Article 014")

# Read the model's errors dataframe
data <- read.xlsx(xlsxFile = "models_errors.xlsx", sheet = 1)

# Fit the best model
model <- my_wrapper_func(setar, data$models_errors)

# If the model was not fitted
if (isFALSE(model)) {
  # Set the last regime as 0
  data$regime[length(data$regime)] <- 0
  # Convert the data into a dataframe
  data_to_export <- data.frame(data)
  # Save the dataframe into an Excel file
  write.xlsx(data_to_export, file = "models_errors.xlsx", colNames = TRUE)
  
# If the model was fitted
} else {
  # Set the las regime as what the model outputs
  data$regime[length(data$regime)] <- tail(regime(model),2)[1]
  # Convert the data into a dataframe
  data_to_export <- data.frame(data)
  # Save the dataframe into an Excel file
  write.xlsx(data_to_export, file = "models_errors.xlsx", colNames = TRUE)
}
