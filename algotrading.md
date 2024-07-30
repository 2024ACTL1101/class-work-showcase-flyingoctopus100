
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Fill your code here
# Define the start and end dates for the trading period
start_date <- as.Date("2020-01-02")
end_date <- as.Date("2021-01-04")

# Filter the DataFrame to include only data within the specified trading period
amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
# Fill your code here
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA #Corrected column name
amd_df$accumulated_shares <- 0 #Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Fill your code here
for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  previous_price <- amd_df$close[i-1]
  if(i == 1) {
    # Buying shares on the first day and when previous price > current price
      amd_df$trade_type[i] <- "buy"
      amd_df$costs_proceeds[i] <- (-current_price) * share_size
      accumulated_shares <- accumulated_shares + share_size
  } else if (i == nrow(amd_df)) {
      # Sell shares on the last day
      amd_df$trade_type[i] <- "sell"
      amd_df$costs_proceeds[i] <- accumulated_shares * current_price
      accumulated_shares <- 0
  } else {
      if (current_price < previous_price) {
      # Buy if current price is less than previous price
      amd_df$trade_type[i] <- "buy"
      amd_df$costs_proceeds[i] <- (-current_price) * share_size
      accumulated_shares <- accumulated_shares + share_size
    }
  }
  
  amd_df$accumulated_shares[i] <- accumulated_shares
}
print(amd_df)
}
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Fill your code here
total_capital <- 0
total_PL <- 0
for (i in 1:nrow(amd_df)) {
  #Filter through the data and calculate total cost of shares
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
    total_capital <- total_capital + amd_df$costs_proceeds[i]
  }
}

#Total profit loss calculation
total_PL <- total_capital + amd_df$costs_proceeds[nrow(amd_df)]

#Applying the ROI formula
ROI = (total_PL / -total_capital) * 100

#Print the results
cat("ROI:", ROI, "\n")
cat("Total investment:", total_capital, "\n")
cat("Total profit/loss:", total_PL, "\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Fill your code here
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA #Corrected column name
amd_df$accumulated_shares <- 0 #Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

total_cost_shares <- 0
total_purchased_shares <- 0

for (i in 1:nrow(amd_df)) {
  current_price <- amd_df$close[i]
  previous_price <- amd_df$close[i-1]
  if(i == 1 || previous_price > current_price) {  
    # Buying shares on the first day and when previous price > current price
      amd_df$trade_type[i] <- "buy"
      amd_df$costs_proceeds[i] <- (-current_price) * share_size
      accumulated_shares <- accumulated_shares + share_size
      total_cost_shares <- total_cost_shares - amd_df$costs_proceeds[i]
  } else if (i == nrow(amd_df)) { 
    # Sell on the last day of trading
      amd_df$trade_type[i] <- "sell"
      amd_df$costs_proceeds[i] <- accumulated_shares * current_price
      accumulated_shares <- 0
  }
  
  #Condition for calculating average purchase price
  if (accumulated_shares > 0) {
    avg_purchase_price <- total_cost_shares/accumulated_shares
  } else {
    avg_purchase_price <- 0
  }
  
  #Profit-taking conditions
  profit_threshold <- avg_purchase_price*1.60
  
  if(current_price >= profit_threshold && is.na(amd_df$trade_type[i])) {
    amd_df$trade_type[i] <- "sell"
    amd_df$costs_proceeds[i] <- current_price*accumulated_shares*1/2
    accumulated_shares <- accumulated_shares/2
    total_cost_shares <- avg_purchase_price*accumulated_shares
  }
  
  #Update accumulated shares
  amd_df$accumulated_shares[i] <- accumulated_shares
}
print(amd_df)
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Fill your code here and Disucss
new_total_capital <- 0
new_total_PL <- 0
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
    new_total_capital <- new_total_capital + amd_df$costs_proceeds[i]
  }
}

#Total profit loss calculation
new_total_PL <- new_total_capital + amd_df$costs_proceeds[nrow(amd_df)]

#Profit-loss calculation
new_ROI = (new_total_PL / -new_total_capital) * 100

#Print the results
cat("ROI:", new_ROI, "\n")
cat("Total investment:", new_total_capital, "\n")
cat("Total profit/loss:", new_total_PL, "\n")
```

Explanation:
Over the trading period I used (Jan 2020 - Jan 2021), the initial trading strategy observed an ROI of 40.8% compared to an updated strategy which yielded -28.6%. The primary reason for this is due to the COVID-19 pandemic which choked global supply chains, resulting in AMD's stock price falling to a period-low of 39.12. 

However, after internal restructuring, AMD began to capitalize on the change brought about by the pandemic and the shift towards working from home. This placed heavy demand on computers and other forms of technology, meaning AMD's graphics cards and processors rose in demand. In August 2020, AMD's stock price spiked by $26 dollars, going from 52.58 (2020-07-01) to 85.04 (2020-08-04). Further, a 16% increase in quarter-over-quarter revenue was observed, mainly driven by sales in the Computing and Graphics department. 

The primary reason for the ROI producing a negative value is because the algorithm first sold the shares when the stock price initially rose, however, it would have been more profitable to sell these shares when the economy entered a state of recovery. Further, the algorithm also bought shares at a high price of 90.22, which was not profitable as these shares declined to 82.54 the following week. This sudden change in stock price would have negatively impacted our calculations for average share price.

Ultimately, the flaw with the updated trading algorithm is that it failed to contextually consider the stock prices, causing an inaccurate value for average share price, causing the algorithm to buy stocks at a sub-optimal time.





