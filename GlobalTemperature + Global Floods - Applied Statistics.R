# Download Packages
#install.packages("tseries")
# Install e1071 package for the skewness() function
#install.packages("e1071")

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tseries)
library(e1071)

# Set Working Directory 
setwd("C:/Birmingham City University/Masters in Big Data Analytics/Applied Statistics/Assessment")

# 1. Load NASA Temperature Data (annual anomalies)
temperature_data <- read.csv("GLB.Ts+dSST - Copy.csv")  

# 2. Load Dartmouth Flood Data (Excel format)
flood_data <- read_excel("FloodArchive.xlsx")  

# Check the first few rows
head(flood_data)

# Data Preprocessing 
# NASA Temperature Data

# Check for null values

# Remove the 1st and last row of the dataframe
temperature_data <- temperature_data[-c(1,nrow(temperature_data)),]

# Convert chr columns to numeric columns 
cols <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
temperature_data[cols] <- lapply(temperature_data[cols], as.numeric)

# Create a new, clean dataframe with only Year and the annual anomaly
nasa_annual <- temperature_data %>%
  rename(Annual_Anomaly = J.D) %>%
  select(Year, Annual_Anomaly)

# Convert Annual_Anomaly column to numeric
nasa_annual$Annual_Anomaly <- as.numeric(nasa_annual$Annual_Anomaly)



# Pre-processing flood data

# Check for missing values in flood data
colSums(is.na(flood_data))

# Aggregate the data 

# Make year column from the Began date
flood_data$Year <- format(flood_data$Began, "%Y")
flood_data$Year <- as.numeric(flood_data$Year)


# Summarize flood data 

flood_summary <- flood_data %>%
  group_by(Year) %>%
  summarise(
    FloodCount = n(),                        # number of floods
    AvgSeverity = mean(Severity, na.rm = TRUE), # average severity
    TotalDeaths = sum(Dead, na.rm = TRUE),      # total deaths
    TotalDisplaced = sum(Displaced, na.rm = TRUE) # total displaced
  )


# Merge both dataset Nasa and temperature 
# Merge the datasets on the 'Year' column
combined_data <- inner_join(nasa_annual, flood_summary, by = "Year")

# View the structure of the final, merged dataset
str(combined_data)

# View the first and last rows to confirm the merge worked correctly
head(combined_data)
tail(combined_data)


# Exploratory Data Analysis

# Summary of the data
summary(combined_data)

# Histograms ( Visualizing distribution)
# Histogram for Temperature Anomaly
hist(combined_data$Annual_Anomaly,
     main="Distribution of Global Temperature Anomalies",
     xlab="Temperature Anomaly (°C)",
     breaks=10)

# Histogram for Flood Count
hist(combined_data$FloodCount,
     main="Distribution of Annual Flood Count",
     xlab="Number of Floods",
     breaks=10)

# Histogram for Total Deaths (likely highly skewed)
hist(combined_data$TotalDeaths,
     main="Distribution of Annual Flood Deaths",
     xlab="Total Deaths",
     breaks=15)

# Histograms for Total Displaced 
hist(combined_data$TotalDisplaced,
           main = "Distribution of Annual Total Displaced",
           xlab = "Total Displaced",
           breaks = 15)

# calculating skewness

# Calculate skewness for key variables
#Skewness ~ 0: Symmetrical distribution (normal).
#Skewness > 0: Positive (right) skew.
#Skewness < 0: Negative (left) skew.

skewness(combined_data$Annual_Anomaly)
skewness(combined_data$FloodCount)
skewness(combined_data$TotalDeaths)
skewness(combined_data$TotalDeaths)

# Boxplot
# Create a boxplot for each key variable
par(mfrow=c(2,2)) # This sets up a 2x2 grid for plots

boxplot(combined_data$Annual_Anomaly, main="Global Temperature Anomaly")
boxplot(combined_data$FloodCount, main="Annual Flood Count")
boxplot(combined_data$TotalDeaths, main="Annual Flood Deaths")
boxplot(combined_data$TotalDisplaced, main="Annual Displaced")

par(mfrow=c(1,1)) # This resets the plotting layout to default

# Temperature Trend (1985-2021)

# Temperature Anomaly Trend
temp_trend_plot <- ggplot(combined_data, aes(x = Year, y = Annual_Anomaly)) +
  geom_line(color = "red", linewidth = 1) +          # Add a red line
  geom_point(size = 1.5) +                           # Add points for each year
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") + # Add linear trend line
  labs(
    title = "Trend in Global Temperature Anomalies (1985-2021)",
    x = "Year",
    y = "Temperature Anomaly (°C)",
    caption = "Data Source: NASA GISTEMP"
  ) +
  theme_minimal()

# Display the plot
print(temp_trend_plot)


# Flood Metrics Trends
# For a multi-plot panel, we need to reshape the data to a 'long' format


flood_metrics_long <- combined_data %>%
  select(Year, FloodCount, TotalDeaths, TotalDisplaced) %>%
  pivot_longer(cols = -Year, names_to = "Metric", values_to = "Value")

# Now create the multi-trend plot
flood_trends_plot <- ggplot(flood_metrics_long, aes(x = Year, y = Value)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") + # Trend line for each
  facet_wrap(~ Metric, scales = "free_y", ncol = 1, # Arrange in one column, free Y-axis scales
             strip.position = "left", # Labels on the left
             labeller = as_labeller(c(FloodCount = "Flood Count",
                                      TotalDeaths = "Total Deaths",
                                      TotalDisplaced = "Total Displaced"))) +
  labs(
    title = "Trends in Flood Frequency and Impact (1985-2021)",
    x = "Year",
    y = NULL, # Y-label is handled by facet_wrap labeller
    caption = "Data Source: Dartmouth Flood Observatory"
  ) +
  theme_minimal() +
  theme(strip.placement = "outside") # Ensure the facet labels are outside the plots

# Display the plot
print(flood_trends_plot)

# Correlation Test

# Correlation between Temperature and Flood Count
cor_test_flood_count <- cor.test(combined_data$Annual_Anomaly,
                                 combined_data$FloodCount,
                                 method = "pearson") # Use "spearman" if data is not normal

# Print the result
print(cor_test_flood_count)

# Correlation between Temperature and Total Deaths
# Use Spearman due to high skewness and outliers in TotalDeaths
cor_test_deaths <- cor.test(combined_data$Annual_Anomaly,
                            combined_data$TotalDeaths,
                            method = "spearman")

# Print the result
print(cor_test_deaths)

# Correlation between Temperature and Total Displaced
# Use Spearman due to high skewness and outliers in TotalDisplaced
cor_test_displaced <- cor.test(combined_data$Annual_Anomaly,
                               combined_data$TotalDisplaced,
                               method = "spearman")

# Print the result
print(cor_test_displaced)

# Heatmap matrix plot



# Linear Regression

# Test Temperature Trend
# Fit linear model
temp_model <- lm(Annual_Anomaly ~ Year, data = combined_data)

# Select numeric variables for correlation matrix
corr_data <- combined_data %>% 
  select(Annual_Anomaly, FloodCount, TotalDeaths, TotalDisplaced)

# Calculate correlation matrix using Spearman method
cor_matrix <- cor(corr_data, method = "spearman", use = "complete.obs")

# Create correlation plot
corrplot(cor_matrix,
         method = "color",         # colored cells
         type = "upper",           # show upper triangle only
         order = "original",       # keep original order
         diag = FALSE,             # hide diagonal (1's)
         tl.col = "black",         # text label color
         addCoef.col = "black",    # correlation coefficient color
         number.cex = 0.8,         # coefficient text size
         title = "Spearman Correlation Matrix: Temperature and Flood Metrics",
         mar = c(0, 0, 2, 0))      # margins for title# Plot points + straight regression line
temp_trend_plot <- ggplot(combined_data, aes(x = Year, y = Annual_Anomaly)) +
  geom_point(color = "darkred", alpha = 0.7) +
  geom_abline(intercept = coef(temp_model)[1], 
              slope = coef(temp_model)[2], 
              color = "black", 
              linewidth = 0.8) +
  labs(
    title = "Linear Regression: Global Temperature Anomaly vs. Year",
    subtitle = "Strong significant warming trend (p < 0.001, R² = 0.86)",
    x = "Year",
    y = "Temperature Anomaly (°C)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(temp_trend_plot)

# Summarize the model results
summary(temp_model)



# Test Flood Count Trend
# Fit linear model
flood_trend_model <- lm(FloodCount ~ Year, data = combined_data)

# Plot points + straight regression line
flood_trend_plot <- ggplot(combined_data, aes(x = Year, y = FloodCount)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = coef(flood_trend_model)[1], 
              slope = coef(flood_trend_model)[2], 
              color = "black", 
              linewidth = 0.8) +
  labs(
    title = "Linear Regression: Annual Flood Count vs. Year",
    subtitle = "Suggestive but non-significant increasing trend (p = 0.063, R² = 0.10)",
    x = "Year",
    y = "Number of Reported Floods"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(flood_trend_plot)

# Summarize the model results
summary(flood_trend_model)


# Test linear regression between Temperature and Flood Count
# Fit linear model
temp_flood_model <- lm(FloodCount ~ Annual_Anomaly, data = combined_data)

# Plot points + straight regression line
core_relationship_plot <- ggplot(combined_data, aes(x = Annual_Anomaly, y = FloodCount)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_abline(intercept = coef(temp_flood_model)[1], 
              slope = coef(temp_flood_model)[2], 
              color = "black", 
              linewidth = 0.8) +
  labs(
    title = "Linear Regression: Flood Count vs. Temperature Anomaly",
    subtitle = "Significant positive relationship (p = 0.030, R² = 0.13)",
    x = "Global Temperature Anomaly (°C)",
    y = "Number of Reported Floods"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(core_relationship_plot)

# Summarize the model results
summary(temp_flood_model)



# Time series analysis

# Data Conversion to Time Series Objects
# Create time series objects
ts_temp <- ts(combined_data$Annual_Anomaly, start = 1985, frequency = 1)
ts_flood <- ts(combined_data$FloodCount, start = 1985, frequency = 1)
# Optional: Convert other variables if needed
ts_deaths <- ts(combined_data$TotalDeaths, start = 1985, frequency = 1)
ts_displaced <- ts(combined_data$TotalDisplaced, start = 1985, frequency = 1)

# Visual Inspection

# Set up a 2x1 plotting layout
par(mfrow = c(2, 1))

# Plot 1: Temperature Anomaly
plot(ts_temp,
     main = "Time Series of Global Temperature Anomaly (1985-2021)",
     ylab = "Temperature Anomaly (°C)",
     xlab = "Year",
     col = "red",
     lwd = 2,
     lty = 1)

# Plot 2: Flood Count
plot(ts_flood,
     main = "Time Series of Annual Flood Count (1985-2021)",
     ylab = "Number of Floods",
     xlab = "Year",
     col = "blue",
     lwd = 2,
     lty = 1)

# Reset the plotting layout to default (1x1)
par(mfrow = c(1, 1))


# Stationarity Testing

# Run Augmented Dickey-Fuller Test
adf_test_temp <- adf.test(ts_temp)
adf_test_flood <- adf.test(ts_flood)

# Print results
print(adf_test_temp)
print(adf_test_flood)

# Plot original vs differenced series for stationarity check
par(mfrow = c(2,2))

# Temperature anomaly
plot(ts_temp, main = "Original: Temperature Anomaly", ylab = "Temp Anomaly", xlab = "Year", col = "red")
plot(diff(ts_temp), main = "Differenced: Temperature Anomaly", ylab = "Diff Temp", xlab = "Year", col = "darkred")

# Flood count
plot(ts_flood, main = "Original: Flood Count", ylab = "Floods", xlab = "Year", col = "blue")
plot(diff(ts_flood), main = "Differenced: Flood Count", ylab = "Diff Floods", xlab = "Year", col = "darkblue")

par(mfrow = c(1,1))


# Cross-Correlation Analysis
# Calculate cross-correlation
ccf_result <- ccf(ts_temp, ts_flood, 
                  lag.max = 5,    # Look at lags of up to ±5 years
                  main = "Cross-Correlation: Temperature vs. Flood Count",
                  ylab = "Correlation Coefficient",
                  xlab = "Lag (Years)")

# Print the values to see exact correlations at each lag
print(ccf_result)
