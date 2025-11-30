# Rainfall Analysis in R
# Author: Divyanshi Mishra

# Load data
load("rain_small (4).RData")  # or the correct file name

# 2. Show first few rows
head(rain)

# 3. Remove NA values
rain_clean <- na.omit(rain)

# 4. Check duplicated rows
duplicated_rows <- duplicated(rain_clean)
duplicated_rows

# 5. Histogram for rainfall observation
hist(rain_clean$Expected,
     xlab = "Expected Rain (mm/hr)",
     ylab = "Frequency",
     main = "Histogram of Rainfall Expectation")

# 6. Find rainfall > 305 (world record)
high_rain_rows <- which(rain_clean$Expected > 305)
high_rain_rows

# Remove extreme rows
rain_clean <- rain_clean[-high_rain_rows, ]

# 7. Aggregate by ID using median
rain_agg <- aggregate(. ~ ID, data = rain_clean, FUN = median)

# 8. Create rain type category
rain_agg$rain_type <- cut(
  rain_agg$Expected,
  breaks = c(0, 4, 10, 305),
  labels = c("Moderate", "Heavy", "Violent"),
  include.lowest = TRUE
)

# 9. Bar chart for rain type distribution
barplot(table(rain_agg$rain_type),
        main = "Distribution of Rain Types",
        xlab = "Rain Type",
        ylab = "Frequency")

# 10. Boxplot rainfall vs rain type
boxplot(Expected ~ rain_type, data = rain_agg,
        main = "Rainfall Amount by Rain Type",
        xlab = "Rain Type",
        ylab = "Rainfall (mm/hr)")
