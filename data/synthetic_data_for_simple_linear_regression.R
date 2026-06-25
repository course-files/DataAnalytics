# =============================================================================
# generate_clv_data.R
#
# Purpose : Generate a clean synthetic dataset suitable for teaching
#           Simple Linear Regression (SLR).
#
# Response variable  : customer_lifetime_value  (numeric, KES)
# Predictor variable : purchase_frequency       (integer, 1–10 per month)
#
# The dataset is constructed so that the following diagnostic tests pass
# at the conventional α = 0.05 level:
#   • Shapiro-Wilk test   — normality of regression residuals
#   • Breusch-Pagan test  — homoscedasticity (constant error variance)
#   • Durbin-Watson test  — independence of errors (no autocorrelation)
#
# These clean properties allow students to focus on SLR interpretation.
# Data transformation remedies (log, Box-Cox, etc.) are covered separately.
#
# Output : ./data/clv_data.csv
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────

# pacman::p_load() installs a package if absent, then loads it — no separate
# install.packages() call needed.  lmtest provides bptest() (Breusch-Pagan)
# and dwtest() (Durbin-Watson).
pacman::p_load(lmtest)

# ── 1. Reproducibility ────────────────────────────────────────────────────────

# set.seed() guarantees that every run of this script produces exactly the
# same dataset.  Students and the lecturer will therefore work with identical
# data regardless of when or where the script is executed.
set.seed(2024)

# ── 2. Sample size ────────────────────────────────────────────────────────────

# n = 200 is large enough for regression diagnostics to be meaningful and
# small enough for the Shapiro-Wilk test to remain powerful without being
# hypersensitive to trivial deviations from normality.
n <- 200L

# ── 3. Predictor: purchase_frequency ─────────────────────────────────────────

# Represents the number of purchase transactions a customer makes per month.
# The range 1–10 is realistic for a retail context and avoids the zero or
# negative values that appeared in the original dataset (which were
# substantively implausible for a frequency measure).
# replace = TRUE allows the same value to appear more than once across
# customers, as would occur in a real population.
purchase_frequency <- sample(1L:10L, size = n, replace = TRUE)

# ── 4. Random error term ──────────────────────────────────────────────────────

# Errors are drawn from a single Normal distribution with mean 0 and a
# constant standard deviation (σ = 20 KES).  Drawing from one distribution
# with fixed σ is what makes the errors homoscedastic: the spread of CLV
# around the regression line is the same at every level of purchase_frequency.
# This is the property tested by the Breusch-Pagan test.
#
# σ = 20 is chosen deliberately:
#   • Large enough to produce visible scatter in the plot (pedagogically useful).
#   • Small enough relative to the signal (slope × range of X ≈ 171) that
#     the linear relationship is clearly visible and R² ≈ 0.88.
sigma  <- 20
epsilon <- rnorm(n, mean = 0, sd = sigma)

# ── 5. Response: customer_lifetime_value ──────────────────────────────────────

# The data-generating process is the classical SLR model:
#   CLV = β₀ + β₁ × purchase_frequency + ε
#
# Parameters are calibrated to match the original dataset closely so that
# existing .qmd code, axis labels, and interpretation text require minimal
# revision:
#   β₀ = 52   (intercept; baseline CLV for a customer with 0 purchases —
#               interpreted in context as the expected CLV floor)
#   β₁ = 19.5 (slope; each additional purchase per month is associated with
#               KES 19.50 more in lifetime value on average)
#
# Values are rounded to 2 decimal places to match the appearance of real
# financial data and to ensure readr::read_csv() parses the column as
# numeric rather than character.
beta_0 <- 52
beta_1 <- 19.5

customer_lifetime_value <- round(beta_0 + beta_1 * purchase_frequency + epsilon,
                                 digits = 2)

# ── 6. Assemble data frame ────────────────────────────────────────────────────

# Column order matches the original dataset so that existing code referencing
# column positions (e.g., clv_data[, 1]) does not break.
clv_data <- data.frame(
  purchase_frequency      = purchase_frequency,
  customer_lifetime_value = customer_lifetime_value
)

# ── 7. Diagnostic verification ────────────────────────────────────────────────

# Fit the model on the synthetic data and run all three diagnostic tests
# before saving.  If any test fails, the script stops with an informative
# message rather than silently saving a flawed dataset.
#
# This section can be removed after initial verification; it is included here
# so that the data-generation process is fully transparent and auditable.

slr_check <- lm(customer_lifetime_value ~ purchase_frequency, data = clv_data)
resid_check <- residuals(slr_check)

# ── 7a. Shapiro-Wilk test (normality of residuals) ──────────────────────────
# H₀: residuals are normally distributed.
# A p-value > 0.05 means we do not reject H₀ — normality is tenable.
sw_result <- shapiro.test(resid_check)
cat("Shapiro-Wilk: W =", round(sw_result$statistic, 4),
    "| p =", round(sw_result$p.value, 4),
    "|", ifelse(sw_result$p.value > 0.05, "PASS ✓", "FAIL ✗"), "\n")

# ── 7b. Breusch-Pagan test (homoscedasticity) ───────────────────────────────
# H₀: error variance is constant (homoscedastic).
# A p-value > 0.05 means we do not reject H₀ — constant variance is tenable.
bp_result <- lmtest::bptest(slr_check)
cat("Breusch-Pagan: BP =", round(bp_result$statistic, 4),
    "| p =", round(bp_result$p.value, 4),
    "|", ifelse(bp_result$p.value > 0.05, "PASS ✓", "FAIL ✗"), "\n")

# ── 7c. Durbin-Watson test (independence of errors) ──────────────────────────
# H₀: errors are not autocorrelated (DW statistic close to 2).
# A p-value > 0.05 means we do not reject H₀ — independence is tenable.
dw_result <- lmtest::dwtest(slr_check)
cat("Durbin-Watson: DW =", round(dw_result$statistic, 4),
    "| p =", round(dw_result$p.value, 4),
    "|", ifelse(dw_result$p.value > 0.05, "PASS ✓", "FAIL ✗"), "\n")

# Stop with an error if any test fails so the lecturer is not inadvertently
# working with a dataset that contradicts SLR assumptions.
if (sw_result$p.value <= 0.05 ||
    bp_result$p.value <= 0.05 ||
    dw_result$p.value <= 0.05) {
  stop("One or more diagnostic tests failed. Review the data-generating ",
       "parameters before saving.")
}

# ── 8. Save ───────────────────────────────────────────────────────────────────

# Create the ./data/ directory if it does not already exist.
# showWarnings = FALSE suppresses the harmless warning that would appear
# if the directory is already present.
dir.create("./data", showWarnings = FALSE, recursive = TRUE)

# readr::write_csv() produces a clean UTF-8 CSV with no row names, matching
# the convention used throughout the lab series.
# The file name is unchanged from the original so that all .qmd read_csv()
# calls continue to work without modification.
readr::write_csv(clv_data, "./data/clv_data.csv")

cat("\nDataset saved to ./data/clv_data.csv\n")
cat("Rows:", nrow(clv_data), "| Columns:", ncol(clv_data), "\n")
cat("purchase_frequency range:",
    min(clv_data$purchase_frequency), "to",
    max(clv_data$purchase_frequency), "\n")
cat("CLV range:",
    min(clv_data$customer_lifetime_value), "to",
    max(clv_data$customer_lifetime_value), "\n")
