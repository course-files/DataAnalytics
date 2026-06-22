## ------------------------------------------------------------------
## Synthetic Dataset Generator: Retail Business Performance Data
## For teaching Pearson's r and Spearman's rho
## ------------------------------------------------------------------
## Context: a retail chain's store-level performance data. Each row is
## one store. The dataset is deliberately built with TWO different kinds
## of relationship, so that Pearson and Spearman can be taught and
## contrasted on the same dataset:
##
##  1. Marketing_Spend_USD -> Monthly_Sales_USD
##     A LINEAR relationship with roughly normal noise: the textbook
##     case Pearson's r is designed for.
##
##  2. Avg_Delivery_Time_Days -> Customer_Satisfaction_Score
##     A MONOTONIC but NON-LINEAR (diminishing-returns / hyperbolic
##     decay) relationship, with three deliberately injected outlier
##     stores. Pearson's r is still usable here but understates the
##     relationship and is more disturbed by the outliers; Spearman's
##     rho, which only requires a monotonic relationship and uses
##     ranks, captures it more faithfully. This is the case Spearman
##     is designed for.
## ------------------------------------------------------------------

set.seed(2026)
n_stores <- 40

## ---- Pair 1: Marketing Spend vs Monthly Sales (linear) ----
Marketing_Spend_USD <- round(runif(n_stores, 5000, 50000), 0)
Monthly_Sales_USD <- round(
  22000 + 1.7 * Marketing_Spend_USD + rnorm(n_stores, mean = 0, sd = 9000),
  0
)

## ---- Pair 2: Delivery Time vs Satisfaction (monotonic, non-linear) ----
Avg_Delivery_Time_Days <- round(runif(n_stores, 1, 10), 1)

# Hyperbolic decay: satisfaction drops sharply for the first day or two
# of delay, then levels off -- a realistic diminishing-returns shape,
# and a deliberately NON-linear one.
satisfaction_true <- 320 / (Avg_Delivery_Time_Days + 3) + 5
Customer_Satisfaction_Score <- satisfaction_true + rnorm(n_stores, mean = 0, sd = 2)

# Three stores get an extra service-recovery shock unrelated to delivery
# time itself (e.g., a refund, a loyalty gesture, or a public complaint),
# which moves their satisfaction score sharply off the underlying curve.
# This barely changes the RANK order of delivery time vs satisfaction
# (so the monotonic relationship survives), but adds real outliers that
# disturb the LINEAR fit -- exactly the contrast Pearson vs Spearman is
# meant to expose.
outlier_rows <- c(5, 18, 33)
Customer_Satisfaction_Score[outlier_rows] <- Customer_Satisfaction_Score[outlier_rows] +
  c(22, -20, 20)

# Clip to a valid 0-100 score range and round to one decimal place
Customer_Satisfaction_Score <- round(
  pmin(pmax(Customer_Satisfaction_Score, 1), 99),
  1
)

business_correlation_data <- data.frame(
  Store_ID                    = paste0("Store_", sprintf("%02d", 1:n_stores)),
  Marketing_Spend_USD         = Marketing_Spend_USD,
  Monthly_Sales_USD           = Monthly_Sales_USD,
  Avg_Delivery_Time_Days      = Avg_Delivery_Time_Days,
  Customer_Satisfaction_Score = Customer_Satisfaction_Score
)

dir.create("data", showWarnings = FALSE)
write.csv(business_correlation_data, "data/business_correlation_data.csv", row.names = FALSE)

cat("Dataset written to data/business_correlation_data.csv\n")
print(head(business_correlation_data))
print(dim(business_correlation_data))
