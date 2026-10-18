library(quantmod)

load_stock_data <- function() {
  symbols <- readLines(file.choose())
  stock_data <- list()
  
  for (symbol in symbols) {
    stock_data[[symbol]] <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
  }
  
  return(stock_data)
}

# stocks <- load_stock_data()

# Step 6: Calculate Basic Statistics

calculate_statistics <- function(stock_data) {
  prices <- as.numeric(Cl(stock_data))
  
  # Moving Average (20 days)
  moving_average <- tail(
    TTR::SMA(prices, n = 20), 1
  )
  
  # Mean
  mean_price <- mean(prices, na.rm = TRUE)
  
  # Mode
  mode_price <- as.numeric(names(
    sort(table(prices), decreasing = TRUE)[1]
  ))
  
  # Median
  median_price <- median(prices, na.rm = TRUE)
  
  # Standard Deviation
  sd_price <- sd(prices, na.rm = TRUE)
  
  return(data.frame(
    Moving_Average = moving_average,
    Mean = mean_price,
    Mode = mode_price,
    Median = median_price,
    Standard_Deviation = sd_price
  ))
}

statistics <- lapply(stocks, calculate_statistics)
# Step 7: Display Stock Data

display_stock_data <- function(stocks) {
  for (symbol in names(stocks)) {
    cat("\nStock:", symbol, "\n")
    print(head(stocks[[symbol]]))
    cat("\nSummary:\n")
    print(summary(stocks[[symbol]]))
  }
}

display_stock_data(stocks)