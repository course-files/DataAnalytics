---
title: "t-Test"
author: "Allan Omondi"
date: "2025-12-20"
output:
  html_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    fig_height: 6
    self_contained: false
    keep_md: true
  word_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    keep_md: true
  html_notebook:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    self_contained: false
  pdf_document: 
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    fig_height: 6
    fig_crop: false
    keep_tex: true
    latex_engine: xelatex
---


``` r
knitr::opts_chunk$set(echo = TRUE)
# `installed.packages()` retrieves a matrix of all installed packages
# `[, "Package"]` extracts on the "Package" column from the matrix of all
# packages
# The %in% operator is used to test if the specified package is in the matrix of
# all packages
# `character.only = TRUE` ensures that the quoted name of the package is not treated
# as a symbol
# `dependencies = TRUE` instructs R to install not only the specified package but
# also its dependencies
# `pacman::p_load("here")` installs the package called "here". This package is used
# in the next line.
# `knitr::opts_knit$set(root.dir = here::here())` is used to ensure that the "knitr"
# utility in R knows where to find the files required to create the HTML, Word, or
# PDF version of the notebook.

if (!"pacman" %in% installed.packages()[, "Package"]) {
  install.packages("pacman", dependencies = TRUE)
  library("pacman", character.only = TRUE)
}

pacman::p_load("here")

knitr::opts_knit$set(root.dir = here::here())
```

# Load the Dataset

The following **synthetic dataset** contains a record of the number of sales for Product A. Variables include:

-   `SalesTargetA` — monthly sales for Product A

-   `MarketingStrategy` — Either a “Digital” or “Traditional” marketing strategy

-   `WeeklySales` — weekly sales under the assigned marketing strategy

-   `BeforeTraining` — sales of the salesperson before the training program

-   `AfterTraining` — the same salesperson’s sales after the training program


``` r
# `pacman::p_load()` is designed to both install and load packages
pacman::p_load("readr")

sales_performance_data <- read_csv("./data/sales_performance.csv")
head(sales_performance_data)
```

```
## # A tibble: 6 × 5
##   SalesTargetA MarketingStrategy WeeklySales BeforeTraining AfterTraining
##          <dbl> <chr>                   <dbl>          <dbl>         <dbl>
## 1           48 Digital                    64             76            79
## 2           50 Traditional                59             63            65
## 3           64 Digital                    72             61            70
## 4           53 Traditional                55             71            69
## 5           53 Digital                    73             54            64
## 6           66 Digital                    73             56            66
```

# Initial EDA

[**View the Dimensions**]{.underline}

The number of observations and variables.


``` r
dim(sales_performance_data)
```

```
## [1] 120   5
```

[**View the Data Types**]{.underline}


``` r
sapply(sales_performance_data, class)
```

```
##      SalesTargetA MarketingStrategy       WeeklySales    BeforeTraining 
##         "numeric"       "character"         "numeric"         "numeric" 
##     AfterTraining 
##         "numeric"
```


``` r
str(sales_performance_data)
```

```
## spc_tbl_ [120 × 5] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ SalesTargetA     : num [1:120] 48 50 64 53 53 66 56 42 47 48 ...
##  $ MarketingStrategy: chr [1:120] "Digital" "Traditional" "Digital" "Traditional" ...
##  $ WeeklySales      : num [1:120] 64 59 72 55 73 73 86 71 83 92 ...
##  $ BeforeTraining   : num [1:120] 76 63 61 71 54 56 82 60 75 47 ...
##  $ AfterTraining    : num [1:120] 79 65 70 69 64 66 80 69 95 49 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   SalesTargetA = col_double(),
##   ..   MarketingStrategy = col_character(),
##   ..   WeeklySales = col_double(),
##   ..   BeforeTraining = col_double(),
##   ..   AfterTraining = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

[**Descriptive Statistics**]{.underline}

Understanding your data can lead to:

-   **Data cleaning:** To remove extreme outliers or impute missing data.

-   **Data transformation:** To reduce skewness

-   **Hypothesis formulation:** Formulate a hypothesis based on the patterns you identify

-   **Choosing the appropriate statistical test:** You may notice properties of the data such as distributions or data types that suggest the use of parametric or non-parametric statistical tests and algorithms

Descriptive statistics can be used to understand your data. Typical descriptive statistics include:

1.  **Measures of frequency:** count and percent

2.  **Measures of central tendency:** mean, median, and mode

3.  **Measures of distribution/dispersion/spread/scatter/variability:** minimum, quartiles, maximum, variance, standard deviation, coefficient of variation, range, interquartile range (IQR) [includes a box and whisker plot for visualization], kurtosis, skewness [includes a histogram for visualization]).

4.  **Measures of relationship:** covariance and correlation

## [**Measures of Frequency**]{.underline}


``` r
sales_performance_data_freq <- sales_performance_data$MarketingStrategy
cbind(frequency = table(sales_performance_data_freq),
      percentage = prop.table(table(sales_performance_data_freq)) * 100)
```

```
##             frequency percentage
## Digital            72         60
## Traditional        48         40
```

## [**Measures of Central Tendency**]{.underline}

The median and the mean of each numeric variable:


``` r
summary(sales_performance_data)
```

```
##   SalesTargetA   MarketingStrategy   WeeklySales    BeforeTraining 
##  Min.   :34.00   Length:120         Min.   :36.00   Min.   :36.00  
##  1st Qu.:47.00   Class :character   1st Qu.:63.75   1st Qu.:56.00  
##  Median :52.00   Mode  :character   Median :72.50   Median :61.00  
##  Mean   :52.10                      Mean   :72.12   Mean   :61.05  
##  3rd Qu.:56.25                      3rd Qu.:79.25   3rd Qu.:67.00  
##  Max.   :69.00                      Max.   :97.00   Max.   :82.00  
##  AfterTraining  
##  Min.   :37.00  
##  1st Qu.:58.75  
##  Median :66.00  
##  Mean   :65.42  
##  3rd Qu.:72.25  
##  Max.   :95.00
```

The first 5 rows in the dataset:


``` r
head(sales_performance_data, 5)
```

```
## # A tibble: 5 × 5
##   SalesTargetA MarketingStrategy WeeklySales BeforeTraining AfterTraining
##          <dbl> <chr>                   <dbl>          <dbl>         <dbl>
## 1           48 Digital                    64             76            79
## 2           50 Traditional                59             63            65
## 3           64 Digital                    72             61            70
## 4           53 Traditional                55             71            69
## 5           53 Digital                    73             54            64
```

The last 5 rows in the dataset:


``` r
tail(sales_performance_data, 5)
```

```
## # A tibble: 5 × 5
##   SalesTargetA MarketingStrategy WeeklySales BeforeTraining AfterTraining
##          <dbl> <chr>                   <dbl>          <dbl>         <dbl>
## 1           54 Traditional                36             67            67
## 2           53 Digital                    94             70            81
## 3           47 Digital                    61             63            71
## 4           45 Digital                    75             55            65
## 5           44 Traditional                61             58            64
```

## [**Measures of Distribution**]{.underline}

Measuring the variability in the dataset is important because the amount of variability determines **how well you can generalize** results from the sample to a new observation in the population.

Low variability is ideal because it means that you can better predict information about the population based on the sample data. High variability means that the values are less consistent, thus making it harder to make predictions.

The syntax `dataset[rows, columns]` can be used to specify the exact rows and columns to be considered. `dataset[, columns]` implies all rows will be considered. For example, specifying `BostonHousing[, -4]` implies all the columns except column number 4. This can also be stated as `BostonHousing[, c(1,2,3,5,6,7,8,9,10,11,12,13,14)]`. This allows us to perform calculations on only columns that are numeric, thus leaving out the columns termed as “factors” (categorical) or those that have a string data type.

### **Variance**


``` r
# `sapply()` is designed to apply a function to a variable in a dataset
# In this case, we use `sapply()` to apply the `var()` function used to compute the variance.
sapply(sales_performance_data[,c(1,3,4,5)], var)
```

```
##   SalesTargetA    WeeklySales BeforeTraining  AfterTraining 
##       50.64538      142.22794       74.87143      108.42997
```

### **Standard Deviation**


``` r
sapply(sales_performance_data[,c(1,3,4,5)], sd)
```

```
##   SalesTargetA    WeeklySales BeforeTraining  AfterTraining 
##       7.116557      11.925936       8.652828      10.412971
```

### **Kurtosis (Pearson)**

The Kurtosis informs us of how often outliers occur in the results. There are different formulas for calculating kurtosis. Specifying “type = 2” allows us to use the 2nd formula which is the same kurtosis formula used in other statistical software like SPSS and SAS. It is referred to as "Pearson's definition of kurtosis".

In “type = 2” (used in SPSS and SAS):

1.  Kurtosis \< 3 implies a low number of outliers → platykurtic

2.  Kurtosis = 3 implies a medium number of outliers → mesokurtic

3.  Kurtosis \> 3 implies a high number of outliers → leptokurtic

High kurtosis (leptokurtic) affects models that are sensitive to outliers. Estimates of the variance are also inflated. Low kurtosis (platykurtic) implies a possible underestimation of real-world variability. The typical remedy includes trimming outliers or using robust statistical methods that are less affected by outliers.


``` r
pacman::p_load("e1071")
sapply(sales_performance_data[,c(1,3,4,5)],  kurtosis, type = 2)
```

```
##   SalesTargetA    WeeklySales BeforeTraining  AfterTraining 
##     -0.1610855      0.2929290      0.1016499      0.1276711
```

### **Skewness**

The skewness is used to identify the asymmetry of the distribution of results. Similar to kurtosis, there are several ways of computing the skewness.

Using “type = 2” (common in other statistical software like SPSS and SAS) can be interpreted as:

1.  Skewness between -0.4 and 0.4 (inclusive) implies that there is no skew in the distribution of results; the distribution of results is symmetrical; it is a normal distribution; a Gaussian distribution.

2.  Skewness above 0.4 implies a positive skew; a right-skewed distribution.

3.  Skewness below -0.4 implies a negative skew; a left-skewed distribution.

Skewed data results in misleading averages and potentially biased model coefficients. The typical remedy to skewed data involves applying data transformations such as logarithmic, square-root, or Box–Cox, etc. to reduce skewness.


``` r
sapply(sales_performance_data[,c(1,3,4,5)], skewness, type = 2)
```

```
##   SalesTargetA    WeeklySales BeforeTraining  AfterTraining 
##      0.1261366     -0.2417535     -0.2593415     -0.2408826
```

As a data analyst, you need to confirm if the distortion in kurtosis or skewness is a data problem or it is a real-world insight. For example, a real-world insight could be that few customers drive most of the value. This is as opposed to always looking it at it as a distortion that needs to be corrected.

## [**Measures of Relationship**]{.underline}

### **Covariance**

Covariance is a statistical measure that indicates the direction of the linear relationship between two variables. It assesses whether increases in one variable correspond to increases or decreases in another.​

-   **Positive Covariance:** When one variable increases, the other tends to increase as well.

-   **Negative Covariance:** When one variable increases, the other tends to decrease.

-   **Zero Covariance:** No linear relationship exists between the variables.

While covariance indicates the direction of a relationship, it does not convey the strength or consistency of the relationship. The correlation coefficient is used to indicate the strength of the relationship.


``` r
cov(sales_performance_data[,c(1,3,4,5)], method = "spearman")
```

```
##                SalesTargetA WeeklySales BeforeTraining AfterTraining
## SalesTargetA     1207.13025    75.19538     -178.52941     -71.07353
## WeeklySales        75.19538  1208.49580      -10.41176      27.58824
## BeforeTraining   -178.52941   -10.41176     1207.88235     963.97269
## AfterTraining     -71.07353    27.58824      963.97269    1208.34034
```

### **Correlation**

A strong correlation between variables enables us to better predict the value of the dependent variable using the value of the independent variable. However, a weak correlation between two variables does not help us to predict the value of the dependent variable from the value of the independent variable. This is useful only if there is a linear association between the variables.

We can measure the statistical significance of the correlation using Spearman's rank correlation *rho*. This shows us if the variables are significantly monotonically related. A monotonic relationship between two variables implies that as one variable increases, the other variable either consistently increases or consistently decreases. The key characteristic is the preservation of the direction of change, though the rate of change may vary.


``` r
cor.test(
  sales_performance_data$BeforeTraining,
  sales_performance_data$AfterTraining,
  method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  sales_performance_data$BeforeTraining and sales_performance_data$AfterTraining
## S = 58196, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.7979171
```

To view the correlation of all variables


``` r
cor(sales_performance_data[,c(1,3,4,5)], method = "spearman")
```

```
##                SalesTargetA  WeeklySales BeforeTraining AfterTraining
## SalesTargetA     1.00000000  0.062257476   -0.147849680   -0.05884861
## WeeklySales      0.06225748  1.000000000   -0.008617662    0.02283004
## BeforeTraining  -0.14784968 -0.008617662    1.000000000    0.79791710
## AfterTraining   -0.05884861  0.022830042    0.797917105    1.00000000
```

## [**Basic Visualizations**]{.underline}

### **Histogram**


``` r
# `par(mfrow = c(1, 2))` This is used to divide the area used to plot
# the visualization into a 1 row by 2 columns grid
# `for (i in 1:2)` This is used to identify the variable (column) 
# that is being processed
# `sales_performance_data[[i]]` This is used to extract the i-th column as a vector
# `hist()` This is the function used to plot the histogram
par(mfrow = c(1, 2))
for (i in 1:5) {
  if (is.numeric(sales_performance_data[[i]])) {
    hist(sales_performance_data[[i]],
         main = names(sales_performance_data)[i],
         xlab = names(sales_performance_data)[i])
  } else {
    message(paste("Column", names(sales_performance_data)[i],
                  "is not numeric and will be skipped."))
  }
}
```

![](4_t-test_files/figure-html/visualization_histogram-1.png)<!-- -->![](4_t-test_files/figure-html/visualization_histogram-2.png)<!-- -->

### **Box and Whisker Plot**


``` r
# `boxplot()` This is the function used to plot the box and whisker plot visualization
par(mfrow = c(1, 2))
for (i in 1:5) {
  if (is.numeric(sales_performance_data[[i]])) {
    boxplot(sales_performance_data[[i]], main = names(sales_performance_data)[i])
  } else {
    message(paste("Column", names(sales_performance_data)[i],
                  "is not numeric and will be skipped."))
  }
}
```

![](4_t-test_files/figure-html/visualization_boxplot-1.png)<!-- -->![](4_t-test_files/figure-html/visualization_boxplot-2.png)<!-- -->

### **Missing Data Plot**


``` r
pacman::p_load("Amelia")

missmap(sales_performance_data, col = c("red", "grey"), legend = TRUE)
```

![](4_t-test_files/figure-html/missing_data_plot-1.png)<!-- -->

### **Correlation Plot**


``` r
pacman::p_load("ggcorrplot")

ggcorrplot(cor(sales_performance_data[,c(1,3,4,5)]))
```

![](4_t-test_files/figure-html/correlation_plot-1.png)<!-- -->

### **Scatter Plot**


``` r
pacman::p_load("ggplot2")
ggplot(sales_performance_data,
       aes(x = BeforeTraining, y = AfterTraining)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "Relationship between Before Training and After Training",
    x = "Before Training",
    y = "After Training"
  )
```

![](4_t-test_files/figure-html/scatter_plot_2-1.png)<!-- -->

# Statistical Test: Welch's t-test (unequal variances)

A t-test is used to determine if there is a significant difference between the means of two groups. A t-test compares both the sample mean and the standard deviations while considering the sample size and the degree of variability in the data.

Three types of t-test:

-   **One sample t-test:** To compare the mean of one sample with a known reference, e.g., comparing the sample mean versus the business' claimed mean.

-   **Independent samples t-test:** To compare the mean of two independent samples, e.g., comparing the mean of clients who bought product X versus those who did not buy product X.

-   **Paired samples t-test:** To compare the mean of two related measures for the same observation, e.g., a before-and-after comparison of sales for the same salesperson.

The larger the difference in means, the larger the t-value. Also, the larger the variance in the data, the less meaningful the t-value is.

| **Value of the `alternative` Parameter** | **Test Type** | **Alternative Hypothesis (Ha​)** | **Practical Interpretation** |
|------------------|------------------|------------------|------------------|
| **`"two.sided"`** (The default) | **Two-Tailed** | $H_a: \mu \neq \mu_0$ (or $\mu_1 \neq \mu_2$) | Tests if the true mean (or mean difference) is **not equal to** the hypothesized value. You are interested in a significant difference in *either* the positive or negative direction. |
| **`"greater"`** | **One-Tailed** (Upper-Tailed) | $H_a: \mu > \mu_0$ (or $\mu_1 > \mu_2$) | Tests if the true mean (or mean difference) is **strictly greater than** the hypothesized value. You are *only* interested in a significant result in the positive direction. |
| **`"less"`** | **One-Tailed** (Lower-Tailed) | $H_a: \mu < \mu_0$ (or $\mu_1 < \mu_2$) | Tests if the true mean (or mean difference) is **strictly less than** the hypothesized value. You are *only* interested in a significant result in the negative direction. |

## One Sample t-Test

**Question 1.a.:** *Is Product A meeting the monthly sales target of 50 units?*


``` r
t.test(
  sales_performance_data$SalesTargetA,
  mu = 50,
  alternative = "two.sided"
)
```

```
## 
## 	One Sample t-test
## 
## data:  sales_performance_data$SalesTargetA
## t = 3.2325, df = 119, p-value = 0.001588
## alternative hypothesis: true mean is not equal to 50
## 95 percent confidence interval:
##  50.81363 53.38637
## sample estimates:
## mean of x 
##      52.1
```

**Question 1.b.:** *Is Product A greater than the monthly sales target of 50 units?*


``` r
t.test(
  sales_performance_data$SalesTargetA,
  mu = 50,
  alternative = "greater"
)
```

```
## 
## 	One Sample t-test
## 
## data:  sales_performance_data$SalesTargetA
## t = 3.2325, df = 119, p-value = 0.0007939
## alternative hypothesis: true mean is greater than 50
## 95 percent confidence interval:
##  51.02304      Inf
## sample estimates:
## mean of x 
##      52.1
```

**Question 1.c.:** *Is Product A less than the monthly sales target of 50 units?*


``` r
t.test(
  sales_performance_data$SalesTargetA,
  mu = 50,
  alternative = "less"
)
```

```
## 
## 	One Sample t-test
## 
## data:  sales_performance_data$SalesTargetA
## t = 3.2325, df = 119, p-value = 0.9992
## alternative hypothesis: true mean is less than 50
## 95 percent confidence interval:
##      -Inf 53.17696
## sample estimates:
## mean of x 
##      52.1
```

## Independent Samples t-Test

**Question 2.a.:** *Does digital marketing outperform traditional marketing in weekly sales?*


``` r
t.test(
  WeeklySales ~ MarketingStrategy,
  data = sales_performance_data,
  levels = c("Digital", "Traditional"),
  alternative = "greater"   # Digital > Traditional
)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  WeeklySales by MarketingStrategy
## t = 3.8264, df = 81.065, p-value = 0.0001273
## alternative hypothesis: true difference in means between group Digital and group Traditional is greater than 0
## 95 percent confidence interval:
##  4.788165      Inf
## sample estimates:
##     mean in group Digital mean in group Traditional 
##                  75.51389                  67.04167
```

**Question 2.b.:** *Is there a difference in weekly sales between digital marketing and traditional marketing?*


``` r
t.test(
  WeeklySales ~ MarketingStrategy,
  data = sales_performance_data,
  alternative = "two.sided"
)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  WeeklySales by MarketingStrategy
## t = 3.8264, df = 81.065, p-value = 0.0002546
## alternative hypothesis: true difference in means between group Digital and group Traditional is not equal to 0
## 95 percent confidence interval:
##   4.066809 12.877636
## sample estimates:
##     mean in group Digital mean in group Traditional 
##                  75.51389                  67.04167
```

## Paired Samples t-Test

**Question 3.a.:** *Was the sales performance before training lower than the sales performance after training for each salesperson?*


``` r
t.test(
  sales_performance_data$BeforeTraining,
  sales_performance_data$AfterTraining,
  paired = TRUE,
  alternative = "less"   # Before < After
)
```

```
## 
## 	Paired t-test
## 
## data:  sales_performance_data$BeforeTraining and sales_performance_data$AfterTraining
## t = -7.9027, df = 119, p-value = 7.687e-13
## alternative hypothesis: true mean difference is less than 0
## 95 percent confidence interval:
##      -Inf -3.45067
## sample estimates:
## mean difference 
##       -4.366667
```

**Question 3.b.:** *Was there a difference in sales performance before training versus after training for each salesperson?*


``` r
t.test(
  sales_performance_data$BeforeTraining,
  sales_performance_data$AfterTraining,
  paired = TRUE,
  alternative = "two.sided"
)
```

```
## 
## 	Paired t-test
## 
## data:  sales_performance_data$BeforeTraining and sales_performance_data$AfterTraining
## t = -7.9027, df = 119, p-value = 1.537e-12
## alternative hypothesis: true mean difference is not equal to 0
## 95 percent confidence interval:
##  -5.460773 -3.272560
## sample estimates:
## mean difference 
##       -4.366667
```

# Model Diagnostic (Diagnostic EDA)

A t-test has no predictive structure and therefore cannot generate:

1.  Model-based residuals

2.  Fitted values

3.  Leverage points

4.  Influence measures

5.  RMSE or R²

Thus, it does not require model diagnostics.

## [**Test of Linearity - NA**]{.underline}

Linearity only exists between a predictor and outcome in a regression model. A t-test compares means, not linear relationships.

There is no fitted line, no slope, and no regression function. Therefore, linearity is irrelevant.

## [**Test of Independence of Errors (Autocorrelation) - NA**]{.underline}

Unlike statistical tests of regression, we cannot confirm this statistically. Independence of observations is justified through the study design.

## [**Test of Normality of the Distribution of the Errors**]{.underline}

The Shapiro–Wilk test assesses whether a sample of data could reasonably have come from a normally distributed population. It enables you to answer the question, "Is this data “normal enough” for methods that assume normality?"

It tests the following hypothesis:

H₀: The data comes from a normal distribution.

H₁: The data does not come from a normal distribution.

The test compares your ordered data values to the values you would expect if the data were perfectly normal.

The more they disagree, the lower the Shapiro–Wilk statistic (W), and the smaller the p-value.

It uses the same logic as a Q–Q plot, but expressed mathematically.

**Interpretation**

If p-value \> 0.05

You fail to reject the null hypothesis. There is no evidence that the data deviates from a normal distribution. However, this does not mean the data is truly normal. It simply means it is normal enough for a t-test, ANOVA, or regression.

**Sample Size Matters**

Small samples (n \< 30): Shapiro–Wilk has low power. It often fails to detect non-normality when it actually exists.

Large samples (n \> 50): Shapiro–Wilk has too much power. It will declare almost anything “non-normal,” even slight deviations.


``` r
shapiro.test(sales_performance_data$SalesTargetA)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  sales_performance_data$SalesTargetA
## W = 0.99264, p-value = 0.7805
```

``` r
shapiro.test(
  sales_performance_data$WeeklySales[
    sales_performance_data$MarketingStrategy == "Digital"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  sales_performance_data$WeeklySales[sales_performance_data$MarketingStrategy == "Digital"]
## W = 0.96343, p-value = 0.0346
```

``` r
shapiro.test(
  sales_performance_data$WeeklySales[
    sales_performance_data$MarketingStrategy == "Traditional"])
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  sales_performance_data$WeeklySales[sales_performance_data$MarketingStrategy == "Traditional"]
## W = 0.99086, p-value = 0.9689
```

``` r
shapiro.test(
  sales_performance_data$AfterTraining - sales_performance_data$BeforeTraining)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  sales_performance_data$AfterTraining - sales_performance_data$BeforeTraining
## W = 0.99083, p-value = 0.6106
```

## [**Test of Homoscedasticity**]{.underline}

Homoscedasticity requires that the spread of residuals should be constant across all levels of the independent variable. This can be done by comparing group variances.

The following function in R performs an F-test for equality of variances:

``` r
var.test() 
```

The underlying hypotheses are:

H₀: The two population variances are equal.

H₁: The two population variances are not equal.

In this case, we answer the question, "Do digital and traditional marketing have equal weekly sales variability?"

This matters depending on the type of t-test.

**Student's t-test (equal variances):** Assumes the two groups have the same variance.

``` r
t.test(WeeklySales ~ MarketingStrategy, var.equal = TRUE)
```

**Welch's t-test (unequal variances):** Does not assume equal variances and adjusts the degrees of freedom.

``` r
t.test(WeeklySales ~ MarketingStrategy)
```

**Interpretation**

**If p-value \> 0.05**

You fail to reject the null hypothesis. There is no evidence that the variances differ. → Use Student’s t-test (`var.equal=TRUE`).

**If p-value \< 0.05**

You reject the null hypothesis. There is evidence that the variances are different. → Use Welch’s t-test (the default in R).


``` r
var.test(WeeklySales ~ MarketingStrategy, data = sales_performance_data)
```

```
## 
## 	F test to compare two variances
## 
## data:  WeeklySales by MarketingStrategy
## F = 0.55833, num df = 71, denom df = 47, p-value = 0.02585
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.3250642 0.9320395
## sample estimates:
## ratio of variances 
##          0.5583278
```

The real purpose of checking variances is not to satisfy a statistical rule. It is to understand whether your groups behave differently, not just whether their means differ.

Variance carries business meaning:

1.  Highly variable digital campaigns may be riskier.

2.  Traditional campaigns may be more consistent but weaker.

## [**Quantitative Validation of Assumptions - NA**]{.underline}

A t-test does not generate a regression model and therefore does not generate residuals.\

No residuals → no residual diagnostics → no quantitative validation of assumptions using `gvlma` package (Global Validation of Linear Models Assumptions).

# Interpretation of the Results

The presentation of the results and its subsequent interpretation is based on the following notes.

**t-Statistic—t(d.f.):** In a one-sample t-test, the t-statistic measures how far the sample mean is from the hypothesised population mean.

-   A t-value close to 0 indicates that the sample mean is very close to the hypothesised value.

-   A larger absolute t-value indicates that the observed mean is many standard errors away from the hypothesised mean, providing stronger evidence against the null hypothesis.

The t-statistic has an associated p-value, which quantifies how likely it is to observe a t-value this extreme (or more extreme) if the null hypothesis were true.

**Degrees of Freedom—(d.f.):** Degrees of freedom refers to the number of values in a calculation that are free to vary. It is essentially a measure of how much independent information is available for estimating a statistical parameter.

For example: Imagine you need to calculate the average height of 5 people, and you know the sum of all their heights is 340 inches. If you know the heights of 4 of these people (65, 70, 68, and 72 inches), you can automatically determine the height of the fifth person without measuring them: 340 - (65 + 70 + 68 + 72) = 65 inches In this example, even though there are 5 people, you only have 4 degrees of freedom because once you know 4 heights and the total, the 5th height is no longer “free to vary” – it is determined by the other values.

**Confidence Interval**

A 95% confidence interval (CI) for a parameter provides a range that, under repeated sampling, would contain the true (but unknown) population parameter 95% of the time. Analogy: Imagine shooting arrows at a target. If you drew a circle around where 95% of your arrows landed, that circle is like a confidence interval—it captures the region in which your “shots” (i.e., estimates from different samples) tend to fall.

**Uncertainty quantification:** A CI communicates your estimate’s precision—narrower intervals imply more precise estimates (often due to larger samples or less variability), whereas wider intervals indicate greater uncertainty about the true value.

**Academic Reporting (based on the APA 7th Edition Style)**

Below are some key considerations to note when reporting statistical analysis using the APA style:\

1.  The type of statistical test must be stated.

2.  Although not mandatory, the dependent variable is usually stated first followed by the independent variable when describing relationships, e.g., “…to examine whether advertising expenditures on YouTube, TikTok, and Facebook collectively predict Sales” such that Sales is the dependent variable that depends on advertising expenditures on YouTube, TikTok, and Facebook.

3.  Test statistic and parameters: Report the appropriate test statistic (*t*-Statistic, *F*-Statistic, $\chi^2$ , etc.) with the degrees of freedom in parentheses. The italicized statistical symbol is immediately followed by the degrees of freedom in parentheses without a space, e.g., *t*(498) and not t (498).

4.  Exact p-values: Report exact p-values, when possible (e.g., *p* = .032), unless they are less than .001, then report as *p* \< .001.

5.  Effect sizes: Include appropriate effect size measures (e.g., R²) to indicate practical significance.

6.  Standard errors: Report standard errors of estimates when relevant. The standard error tells you how much your estimate might vary if you were to repeat your study with different random samples from the same population. A smaller standard error indicates a more precise estimate.

7.  Confidence Intervals (CI): The confidence level should be clearly stated whenever you report point estimates (e.g., means, regression coefficients, correlations, etc.). The 95% confidence interval is the most common, and if another level is used (e.g., 90% CI, 99% CI), it should be explicitly mentioned. Confidence intervals are typically enclosed in square brackets [], with the lower and upper limits separated by a comma. For example: 95% CI [-.03, .04]. They are usually reported directly after the statistic they describe, often within the same sentence or in parentheses.

8.  Two decimal places: Report to two decimal places, except p-values which may need three or more decimal places.

9.  Descriptive statistics: Report relevant means, standard deviations, and sample sizes, e.g., The sample size included 500 observations (M = 25.43, SD = 4.62).

10. Italicize statistical symbols: Use italics for statistical symbols (*t*, *F*, *p*, etc.) but not for Greek letters ( $\mu$, $\sigma$, $\alpha$), subscripts, or parenthetical information, e.g., R² = .45, *F*(2, 97) = 15.62, *p* \< .001, The participants (*N* = 120) had an average score (*M* = 25.43, *SD* = 4.62) on the cognitive test.

Further reading: <https://apastyle.apa.org/jars>

## Limitations and Diagnostic Findings

One limitation of the analysis is that the assumption of normality was not fully met for weekly sales under the digital marketing strategy. A Shapiro–Wilk test indicated a statistically significant deviation from normality, W = 0.96, p = .035. Although the t-test is generally robust to moderate departures from normality, particularly with larger samples, this violation may affect the precision of the estimated confidence intervals.

It is also worth noting that sales data are often skewed by promotions, seasonality, or occasional irregularities. In other words, the deviation from normality of the weekly sales under digitial marketing may be structural, not statistical noise.

While non-parametric alternatives are available, e.g., the Wilcoxon signed-rank test, the t-test was retained due to its robustness under moderate non-normality and its direct interpretability in terms of mean differences, which is central to the business questions under investigation.

## One-Sample t-Test

### Academic Statement (APA)—Academic-Ready Language

A one-sample t-test was conducted to determine whether the mean monthly sales of Product A differed from the target of 50 units. Based on a sample of 120 observations, results indicated that the mean sales (M = 52.10, SD = 7.12, 95% CI [50.81, 53.39]) were significantly higher than the target, *t*(119) = 3.23, p = .002.

### Business Analysis—Boardroom-Ready Language

#### **Executive Summary**

An analysis of 120 monthly sales observations shows that Product A is consistently exceeding its sales target. Average monthly sales are approximately 52 units, compared to a target of 50 units. This difference is statistically reliable and unlikely to be due to random fluctuation.

While performance exceeds target, sales variability remains moderately high, indicating that not all periods perform equally well. Overall, Product A is meeting expectations, but performance stability can still be improved.

#### **Recommendations**

The management should treat the current sales target as achievable under existing conditions. However, the observed variability suggests opportunities to stabilize performance through demand forecasting, inventory planning, or targeted sales interventions during weaker periods.

If the strategic objective is growth rather than maintenance, the management may consider a gradual upward revision of the sales target, supported by operational safeguards to manage variability.

## Independent Samples t-Test

### Academic Statement (APA)—Academic-Ready Language

A Welch two-sample t-test was conducted to examine whether mean weekly sales differed between digital and traditional marketing strategies. Results indicated that mean weekly sales under the digital strategy (M = 75.51) were significantly higher than those under the traditional strategy (M = 67.04), *t*(81.07) = 3.83, p \< .001, 95% CI [4.07, 12.88]. These findings suggest a statistically significant difference in sales performance between the two marketing strategies, with digital marketing associated with higher average weekly sales.

### Business Analysis—Boardroom-Ready Language

#### **Executive Summary**

An analysis comparing weekly sales performance across marketing strategies shows that digital marketing consistently outperforms traditional marketing. On average, digital campaigns generate between 4 and 13 more units in weekly sales than traditional approaches. This difference is statistically robust and highly unlikely to be due to random variation.

In practical terms, digital marketing is delivering meaningfully higher sales outcomes under current operating conditions.

#### **Recommendations**

1.  **Prioritise Digital Marketing Investment:** Current evidence strongly supports allocating a larger share of marketing resources to digital channels, as they deliver consistently higher sales returns.

2.  **Re-evaluate the Role of Traditional Marketing:** Traditional marketing should be reviewed for strategic relevance. It may still serve brand awareness or niche segments, but it should no longer be treated as the primary sales driver.

3.  **Optimise, Do Not Eliminate (Yet):** Rather than an abrupt withdrawal, management should assess whether Traditional marketing can be:

    -   Integrated with digital campaigns, or

    -   Targeted more selectively where it performs best

# Knitting the Notebook

The “Knit” utility in R can be used to convert the R Notebook into either a:

1.  HTML document that can be opened using a browser

2.  HTML notebook that can also be opened using a browser and has basic interactive features

3.  Word Document

4.  PDF document

The conversion to PDF requires the installation of the following free software:

-   For Windows: MiKTeX - <https://miktex.org/download>

-   For MacOS: MacTeX - <https://www.tug.org/mactex/mactex-download.html>

-   For Linux: TeX Live - <https://www.tug.org/texlive/quickinstall.html>

Also, you need to install the `tinytex` package. The `tinytex` package helps RStudio to find and use MikTex, MacTeX, or TeXLive. Execute the following **in the console section of RStudio** to install TinyTex:

`install.packages("tinytex")`

`tinytex::install_tinytex()`

If you are using MiKTeX for Windows, you should also enable the installation of packages on-the-fly. This is found in “Settings \> General \> Package Installation”

Lastly, set the LaTeX Engine to `xelatex`. This is found in "Output Options \> Advanced" in R Studio.

# References and Further Reading

American Psychological Association. (2025, February). *Journal Article Reporting Standards (JARS)*. APA Style. Retrieved April 28, 2025, from <https://apastyle.apa.org/jars>

Hodeghatta, U. R., & Nayak, U. (2023). *Practical Business Analytics Using R and Python: Solve Business Problems Using a Data-driven Approach* (2nd ed.). Apress. <https://link.springer.com/book/10.1007/978-1-4842-8754-5>
