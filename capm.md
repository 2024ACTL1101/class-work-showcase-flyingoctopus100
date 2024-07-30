
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
df$AMD_dr <- NA
df$GSPC_dr <- NA
for (i in 2:nrow(df)) {
df$AMD_dr[i] <- ((df$AMD[i]-df$AMD[i-1])/df$AMD[i-1])
df$GSPC_dr[i] <- ((df$GSPC[i]-df$GSPC[i-1])/df$GSPC[i-1])
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#fill the code
df$Daily_RF <- NA
for (i in 1:nrow(df)){
df$Daily_RF[i] <- (((1+df$RF[i]/100)ˆ(1/360))-1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#fill the code
df$AMD_excess <- NA
df$GSPC_excess <- NA
for(i in 2:nrow(df)){
df$AMD_excess[i] <- df$AMD_dr[i]-df$Daily_RF[i]
df$GSPC_excess[i] <- df$GSPC_dr[i]-df$Daily_RF[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#fill the code
capm_model <- lm(AMD_excess ~ GSPC_excess, data = df)
summary(capm_model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:** From the summary of the CAPM model, AMD’s β can be calculated to be 1.5699987. This showcases that for every 1% increase in the excess return of the S&P 500, the excess return of AMD is expected to rise by approximately 1.57%. Similarly, for every 1% decrease in the S&P 500, the AMD share price decreased by an average of 1.57%. This indicates that AMD fluctuates by a greater margin in response to market movements and is therefore more volatile than the market. Since investing into AMD carries greater risk compared to investing in the S&P 500, there is also expectation of greater returns. As a result, investors who are risk averse should opt to invest in the S&P 500, whilst those who are risk tolerant should purchase AMD shares.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
#fill the code
library(ggplot2)
ggplot(df, aes(x = GSPC_excess, y = AMD_excess)) +
geom_point() +
geom_smooth(method = 'lm', se = TRUE) +
labs(title = "CAPM Analysis: AMD vs S&P 500",
x = "S&P 500 Excess Return",
y = "AMD Excess Return")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.


```r
#fill the code
current_rate <- 0.05
expected_return <- 0.133
daily_error <- summary(capm_model)$sigma
annual_error <- daily_error *sqrt(252)
beta <- coef(capm_model)[2]
amd_returns <- current_rate + beta*(expected_return-current_rate)
z_score <- qnorm(1-0.1/2)
lower_bound <- amd_returns-z_score * annual_error
upper_bound <-amd_returns + z_score * annual_error
lower_bound <- format(lower_bound, digits = 3)
upper_bound <- format(upper_bound, digits = 3)
```

The expected annual return of 18.03% (2dp) suggests that AMD is projected to perform well in the market. However, a 90% prediction interval between -4.9% and 8.5% suggests that the expected returns will fall within this range 90% of the time. This broad interval demonstrates AMD’s significant stock volatility, further evidenced by its β value of 1.57. From an investors perspective, purchasing AMD stock is followed by a high level of risk and would best suit risk-tolerant investors.

