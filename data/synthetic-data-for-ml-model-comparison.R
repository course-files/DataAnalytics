# ----------------------------------------------------
# Synthetic ML Model Comparison Dataset
# Dataset size: 4 algorithms × 10 datasets × 5 runs = 200 observations
# Sample research question:
# Is there a statistically significant difference in mean classification
# accuracy across the Machine Learning algorithms?
# ----------------------------------------------------

set.seed(123)  # For reproducibility

# Define parameters
algorithms <- c(
  "Logistic Regression",
  "Random Forest",
  "SVM",
  "Gradient Boosting"
)

n_datasets <- 10
n_runs <- 5

# Create experimental grid
ml_results <- expand.grid(
  Dataset = paste0("Dataset_", 1:n_datasets),
  Run = 1:n_runs,
  Algorithm = algorithms
)

# Define realistic mean accuracies per algorithm
algorithm_means <- c(
  "Logistic Regression" = 0.72,
  "Random Forest"       = 0.78,
  "SVM"                 = 0.75,
  "Gradient Boosting"   = 0.81
)

# Generate accuracy scores with noise
ml_results$Accuracy <- rnorm(
  n = nrow(ml_results),
  mean = algorithm_means[ml_results$Algorithm],
  sd = 0.03
)

# Constrain accuracy to realistic bounds
ml_results$Accuracy <- pmin(pmax(ml_results$Accuracy, 0.60), 0.90)

# Convert Algorithm to factor
ml_results$Algorithm <- factor(ml_results$Algorithm)

# Inspect dataset
head(ml_results)


# Optionally save:
write.csv(ml_results, "./data/ml_results.csv", row.names = FALSE)