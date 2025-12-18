set.seed(123)

# 120 observations
n <- 120

# 1. One-sample t-test variable (Product A sales)
SalesTargetA <- round(rnorm(n, mean = 52, sd = 8))  # Not perfect, slight skew

# 2. Independent samples t-test variables
MarketingStrategy <- sample(c("Digital", "Traditional"), n, replace = TRUE)

WeeklySales <- ifelse(
  MarketingStrategy == "Digital",
  rnorm(n, mean = 75, sd = 10),
  rnorm(n, mean = 68, sd = 12)
)
WeeklySales <- round(WeeklySales)

# 3. Paired samples variables (before vs after training)
BeforeTraining <- round(rnorm(n, mean = 60, sd = 9))
AfterTraining  <- round(BeforeTraining + rnorm(n, mean = 5, sd = 6))  # Improvement but imperfect

# Combine into data frame
SalesPerformance <- data.frame(
  SalesTargetA,
  MarketingStrategy,
  WeeklySales,
  BeforeTraining,
  AfterTraining
)

# Preview
head(SalesPerformance)

# Optionally save:
write.csv(SalesPerformance, "./data/sales_performance.csv", row.names = FALSE)
