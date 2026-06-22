# Common Categories of Statistical Tests

Statistical tests can be grouped according to the type of question they are designed to answer. In business analytics and data science, three of the most common categories are **regression tests**, **comparison tests**, and **correlation tests**.

The table below summarizes the primary purpose of each category.

| Category | Typical Outcome | Typical Question |
| ----------- | ----------- | ----------- |
| Regression | Predict or explain **Y** | How does X affect Y? |
| Comparison | Compare groups | Is there a **difference** between **groups**? |
| Correlation | Measure association | Is there a **relationship** between **variables**? |

---

## Regression Tests (Variable X → Variable Y; How does Variable X affect Variable Y?)

Regression tests are used to estimate the effect of one or more independent variables on a dependent variable. They are commonly used for prediction, forecasting, and identifying factors that influence business outcomes.

### Examples of Regression Tests

These are typically not grouped into parametric and non-parametric categories because regression tests can be applied to both types of data, depending on the assumptions and the nature of the variables.

- Simple Linear Regression (*see* [1_simple_linear_regression.qmd](1_simple_linear_regression.qmd))
- Multiple Linear Regression (*see* [2_multiple_linear_regression.qmd](2_multiple_linear_regression.qmd))
- Binary Logistic Regression (*see* [3_a_binary_logistic_regression.qmd](3_a_binary_logistic_regression.qmd) and [3_b_binary_logistic_regression_siwaka_dishes.qmd](3_b_binary_logistic_regression_siwaka_dishes.qmd))
- Multinomial Logistic Regression
- Ordinal Logistic Regression
- Poisson Regression
- Cox Proportional Hazards Regression
- Regularized Regression
  - Ridge Regression
  - LASSO Regression
  - Elastic Net Regression

### Example Research Question that Requires a Regression Test

*How do advertising expenditure, product price, and customer income influence sales revenue?*

---

## Comparison Tests (Group A versus Group B versus Group C; Is there a difference between the groups?)

Comparison tests are used to determine whether statistically significant differences exist between two or more groups. Depending on the nature of the variables, they may compare means, medians, proportions, or distributions.

### Examples of Comparison Tests

#### Parametric Comparison Tests

- One Sample t-Test (*see* [4_t_test.qmd](4_t_test.qmd))
- Independent Samples t-Test (*see* [4_t_test.qmd](4_t_test.qmd))
- Paired Samples t-Test (*see* [4_t_test.qmd](4_t_test.qmd))
- One-Way Analysis of Variance (ANOVA) (*see* [5_ANOVA.qmd](5_ANOVA.qmd))
- Two-Way Analysis of Variance (ANOVA) (*see* [5_ANOVA.qmd](5_ANOVA.qmd))
- Multivariate Analysis of Variance (MANOVA) (*see* [5_ANOVA.qmd](5_ANOVA.qmd))

#### Non-Parametric Comparison Tests

- Wilcoxon Signed-Rank Test
- Wilcoxon Rank-Sum Test (Mann-Whitney U Test)
- Kruskal-Wallis H Test
- Friedman Test (*see* [6_Friedman_Test.qmd](6_Friedman_Test.qmd))
- Mood's Median Test

### Example Research Question that Requires a Comparison Test

*Do customers in different age groups spend significantly different amounts on online shopping?*

---

## Correlation Tests (Variable A versus Variable B; Is there a relationship between the variables?)

Correlation tests are used to determine whether variables are associated with one another without assuming a cause-and-effect relationship.

### Examples of Correlation Tests

#### Parametric Correlation Tests

- Pearson's Correlation Coefficient (r) (*see* [7_correlation.qmd](7_correlation.qmd))

#### Non-Parametric Correlation Tests

- Spearman's Rank Correlation (ρ) (*see* [7_correlation.qmd](7_correlation.qmd))
- Kendall's Tau (τ)
- Partial Correlation

### Example Research Question that Requires a Correlation Test

*Is there a relationship between customer satisfaction and repeat purchase frequency?*

---

## Quick Guide

When deciding which category of statistical test to use, ask the following questions:

### 1. Do I want to predict or explain an outcome variable?

→ Use a **Regression Test**

### 2. Do I want to determine whether groups differ from one another?

→ Use a **Comparison Test**

### 3. Do I want to determine whether variables are associated with one another?

→ Use a **Correlation Test**
