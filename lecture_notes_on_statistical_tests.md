# Lecture Notes on Statistical Tests

Statistical tests are used to determine whether observed patterns, differences,
associations, or model effects are likely to be genuine or attributable to
random chance. They can be used to:

- Determine whether a dependent (outcome) variable has a statistically
  significant **direct or indirect relationship** with an independent
  (predictor) variable.
- Estimate the **difference or similarity** between two or more groups
  (**comparison**).
- Assess association between variables (**correlation**).
- Assess **goodness-of-fit**.
- Assess **independence** between categorical variables.
- Assess **model adequacy**.

A statistical test calculates a **test statistic** that measures how strongly
the observed data depart from what would be expected under the null hypothesis.
A **p-value (probability value)** is used in many statistical tests to estimate
how likely it is that the observed result, or a more extreme result, would occur
if the null hypothesis were true.

## Three Common Categories of Statistical Tests

### 1. Statistical Tests for Regression

Used to estimate the effect of one or more independent variables on another
dependent variable. It focuses on the relationship between variables (**Variable
X → Variable Y; How does Variable X affect Variable Y?**).

**Examples of business-related research questions:**

- How do different advertising channels (YouTube, TikTok, and Facebook)
  contribute to sales revenue, and which channel has the strongest effect?
- Which borrower factors (income, family size, credit score, loan amount) most
  strongly predict loan default risk?
- Do salary, job satisfaction, and remote-work flexibility predict employee
  turnover?

### 2. Statistical Tests for Comparison

Used to determine whether there are statistically significant differences
between two or more groups. A comparison test may compare **means, medians,
proportions, or categorical distributions** depending on the nature of the
variables. It focuses on the differences between groups (**Group A versus Group
B versus Group C; Is there a difference between the groups?**).

**Examples of business-related research questions:**

- Do ESG (Environmental, Social, Governance) portfolios yield different annual
  returns compared to traditional portfolios?
- Do technology stocks have significantly higher average monthly returns than
  energy stocks?
- Is there a difference in payment method preference (debit card, mobile money,
  cash) among age groups (Gen Z, Millennials, Boomers)?

### 3. Statistical Tests for Correlation

Used to test whether variables are related without hypothesizing a
cause-and-effect relationship, that is, without assuming that correlation equals
causation. It focuses on the question (**Variable A versus Variable B; Is there
a relationship between the variables?**).

**Examples of business-related research questions:**

- Is there a relationship between customer satisfaction scores and repeat
  purchase frequency?
- Are raw material costs correlated with manufacturing delays?
- Is there a link between hours of remote work and self-reported productivity?

### Example: Differentiating the Three Categories

Consider the relationship between advertisement expenditure and sales:

- **Regression:** How does advertisement expenditure affect sales?
- **Comparison:** Does advertisement expenditure increase sales more for Product
  Category A than Product Category B?
- **Correlation:** Is there a link between advertisement expenditure and sales?

## Parametric and Non-Parametric Statistical Tests

Statistical tests can be either **parametric** or **non-parametric**. Parametric
tests are often more statistically powerful when their assumptions are
satisfied. If the assumptions are violated and cannot be remedied by data
transformation, then non-parametric or robust alternatives should be used to
ensure valid statistical inference.

## Common Assumptions of Parametric Tests

### 1. Level of Measurement

The dependent variable should typically be measured on a quantitative continuous
scale (interval or ratio). Independent variables can be quantitative continuous
or categorical depending on the specific test.

Some parametric tests have been adapted for different data types. For example,
**logistic regression** uses categorical dependent variables.

### 2. Random Sampling

The sample should be randomly selected from the target population. Random
sampling helps ensure that the sample is representative of the population and
reduces the risk of bias.

### 3. Distributional Shape (Normality)

For many parametric tests, the residuals or errors should be approximately
normally distributed, particularly when sample sizes are small. Large sample
sizes are often less sensitive to violations of normality because of the
**Central Limit Theorem**.

**Example:** If a company is predicting the time it takes to deliver a package,
and some of the prediction errors are small (e.g., off by a few minutes), but
some are very large (e.g., off by several hours), then the errors may not be
normally distributed.

### 4. Homoscedasticity (Presence of Homogeneity of Variance)

The spread of the variance of the error term should be approximately equal
across all levels of the independent variable. In the case of comparison tests,
the variances of the groups being compared should be approximately equal.

**Example:** Consider a company using regression to predict customer spending
based on income level. If the model predicts well for low-income customers
(small errors), but poorly for high-income customers (very large errors), the
assumption of equal variance is violated.

### 5. Independence of Observations/Errors (No Autocorrelation)

Each observation should be independent of the others. This assumption is
violated when observations influence one another or when repeated measurements
are collected without appropriate statistical adjustments.

**Example 1:** Algorithm benchmarks run sequentially on the same machine, where
thermal throttling, caching, or background load could make consecutive runs
correlated.

**Example 2:** A retail store's daily revenue observations collected over
consecutive days may be correlated because a promotional campaign running today
continues to drive carry-over purchases tomorrow — meaning each day's revenue is
partly a function of the preceding day's revenue.

### 6. Minimal Outliers

The number of extreme outliers should be minimal because parametric tests often
rely on means and variances. Outliers can distort parameter estimates and affect
the validity of statistical conclusions.

**Example:** A retail company analyzes customer spending and finds that most
customers spend between KES 500 and KES 20,000 per month, while one observation
shows spending of KES 5 million. Such an extreme value may disproportionately
influence the analysis and should be investigated before drawing conclusions.

Possible approaches include:

- Verifying data-entry accuracy.
- Investigating unusual cases.
- Applying suitable data transformations.
- Using robust statistical methods.

### 7. Additional Assumption for Multiple Linear Regression Only: Absence of Multicollinearity

When multiple predictors are included in a regression model, the predictors
should not be highly correlated with one another. High multicollinearity makes
it difficult to determine the individual contribution of each predictor and can
lead to unstable coefficient estimates.

**Example:** A company trains a model to predict employee salaries using both
years of work experience and age. Since age and work experience are often
strongly correlated, multicollinearity may occur.

**Summary:**

| Assumption | Visual Plot | Statistical Test |
|----|----|----|
| Linearity | Residuals vs Fitted | Visual inspection |
| Autocorrelation | Residuals vs Observation Order | Durbin–Watson Test |
| Normality of the Distribution of Errors | Q-Q Plot | Shapiro–Wilk Test |
| Homoscedasticity | Scale-Location Plot | Breusch–Pagan Test |
