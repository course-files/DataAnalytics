---
title: "Multiple Linear Regression"
author: "Allan Omondi"
date: "2025-11-13"
output:
  html_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 5
    fig_height: 5
    self_contained: false
    keep_md: true
  word_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 5
    keep_md: true
  html_notebook:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 5
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
if (!"pacman" %in% installed.packages()[, "Package"]) {
  install.packages("pacman", dependencies = TRUE)
  library("pacman", character.only = TRUE)
}

pacman::p_load("here")

knitr::opts_knit$set(root.dir = here::here())
```

# Load the Dataset


``` r
pacman::p_load("readr")

advertising_data <- read_csv("./data/sme_socialmedia_advertising_kenya.csv")
head(advertising_data)
```

```
## # A tibble: 6 × 4
##   YouTube TikTok Facebook  Sales
##     <dbl>  <dbl>    <dbl>  <dbl>
## 1   56907 27432.        0 250000
## 2   44234 44750.        0 250000
## 3   40335 87006.        0 250000
## 4   10556 13648.        0 250000
## 5  102578 12991.        0 250000
## 6   11209 34982.        0 250000
```

# Initial EDA

[**View the Dimensions**]{.underline}

The number of observations and the number of variables.


``` r
dim(advertising_data)
```

```
## [1] 250000      4
```

[**View the Data Types**]{.underline}


``` r
sapply(advertising_data, class)
```

```
##   YouTube    TikTok  Facebook     Sales 
## "numeric" "numeric" "numeric" "numeric"
```


``` r
str(advertising_data)
```

```
## spc_tbl_ [250,000 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ YouTube : num [1:250000] 56907 44234 40335 10556 102578 ...
##  $ TikTok  : num [1:250000] 27432 44750 87006 13648 12991 ...
##  $ Facebook: num [1:250000] 0 0 0 0 0 ...
##  $ Sales   : num [1:250000] 250000 250000 250000 250000 250000 250000 250000 250000 250000 250000 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   YouTube = col_double(),
##   ..   TikTok = col_double(),
##   ..   Facebook = col_double(),
##   ..   Sales = col_double()
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

This is applicable in cases where you have categorical variables, e.g., 60% of the observations are male and 40% are female (2 categories).

## [**Measures of Central Tendency**]{.underline}

The median and the mean of each numeric variable:


``` r
summary(advertising_data)
```

```
##     YouTube           TikTok          Facebook          Sales        
##  Min.   : 10000   Min.   :  1466   Min.   :     0   Min.   : 250000  
##  1st Qu.: 45125   1st Qu.: 31021   1st Qu.: 33818   1st Qu.:1246319  
##  Median : 80145   Median : 54067   Median : 62571   Median :1763254  
##  Mean   : 80052   Mean   : 54064   Mean   : 62534   Mean   :1768575  
##  3rd Qu.:114997   3rd Qu.: 77130   3rd Qu.: 91299   3rd Qu.:2278333  
##  Max.   :150000   Max.   :106652   Max.   :128212   Max.   :5000000
```

The first 5 rows in the dataset:


``` r
head(advertising_data, 5)
```

```
## # A tibble: 5 × 4
##   YouTube TikTok Facebook  Sales
##     <dbl>  <dbl>    <dbl>  <dbl>
## 1   56907 27432.        0 250000
## 2   44234 44750.        0 250000
## 3   40335 87006.        0 250000
## 4   10556 13648.        0 250000
## 5  102578 12991.        0 250000
```

The last 5 rows in the dataset:


``` r
tail(advertising_data, 5)
```

```
## # A tibble: 5 × 4
##   YouTube TikTok Facebook   Sales
##     <dbl>  <dbl>    <dbl>   <dbl>
## 1  139474 56121.  109408. 4883726
## 2  131806 89937.   83064. 4948997
## 3  147931 58946.   78191. 4981242
## 4  130879 85472.  108876. 5000000
## 5  142755 88549.  120065. 5000000
```

## [**Measures of Distribution**]{.underline}

Measuring the variability in the dataset is important because the amount of variability determines **how well you can generalize** results from the sample to a new observation in the population.

Low variability is ideal because it means that you can better predict information about the population based on the sample data. High variability means that the values are less consistent, thus making it harder to make predictions.

The syntax `dataset[rows, columns]` can be used to specify the exact rows and columns to be considered. `dataset[, columns]` implies all rows will be considered. For example, specifying `BostonHousing[, -4]` implies all the columns except column number 4. This can also be stated as `BostonHousing[, c(1,2,3,5,6,7,8,9,10,11,12,13,14)]`. This allows us to perform calculations on only columns that are numeric, thus leaving out the columns termed as “factors” (categorical) or those that have a string data type.

### **Variance**


``` r
sapply(advertising_data[,], var)
```

```
##      YouTube       TikTok     Facebook        Sales 
##   1630374942    709958483   1106898309 557135353501
```

### **Standard Deviation**


``` r
sapply(advertising_data[,], sd)
```

```
##   YouTube    TikTok  Facebook     Sales 
##  40377.90  26645.05  33270.08 746415.00
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
sapply(advertising_data[,],  kurtosis, type = 2)
```

```
##    YouTube     TikTok   Facebook      Sales 
## -1.1974100 -1.1900912 -1.1864690 -0.2928934
```

### **Skewness**

The skewness is used to identify the asymmetry of the distribution of results. Similar to kurtosis, there are several ways of computing the skewness.

Using “type = 2” (common in other statistical software like SPSS and SAS) can be interpreted as:

1.  Skewness between -0.4 and 0.4 (inclusive) implies that there is no skew in the distribution of results; the distribution of results is symmetrical; it is a normal distribution; a Gaussian distribution.

2.  Skewness above 0.4 implies a positive skew; a right-skewed distribution.

3.  Skewness below -0.4 implies a negative skew; a left-skewed distribution.

Skewed data results in misleading averages and potentially biased model coefficients. The typical remedy to skewed data involves applying data transformations such as logarithmic, square-root, or Box–Cox, etc. to reduce skewness.


``` r
sapply(advertising_data[,], skewness, type = 2)
```

```
##      YouTube       TikTok     Facebook        Sales 
## -0.003603533 -0.003310514 -0.001325236  0.116540143
```

As a data analyst, you need to confirm if the distortion in kurtosis or skewness is a data problem or it is a real-world insight. For example, a real-world insight could be that the sales were exceptionally high because of a viral marketing campaign.

## [**Measures of Relationship**]{.underline}

### **Covariance**

Covariance is a statistical measure that indicates the direction of the linear relationship between two variables. It assesses whether increases in one variable correspond to increases or decreases in another.​

-   **Positive Covariance:** When one variable increases, the other tends to increase as well.

-   **Negative Covariance:** When one variable increases, the other tends to decrease.

-   **Zero Covariance:** No linear relationship exists between the variables.

While covariance indicates the direction of a relationship, it does not convey the strength or consistency of the relationship. The correlation coefficient is used to indicate the strength of the relationship.


``` r
cov(advertising_data, method = "spearman")
```

```
##               YouTube     TikTok     Facebook      Sales
## YouTube  5208354166.0   -8082342     971861.4 2254631446
## TikTok     -8082341.7 5208354167   -3578040.4 1327755027
## Facebook     971861.4   -3578040 5208354166.7 1796720125
## Sales    2254631445.7 1327755027 1796720125.0 5208285742
```

### **Correlation**

A strong correlation between variables enables us to better predict the value of the dependent variable using the value of the independent variable. However, a weak correlation between two variables does not help us to predict the value of the dependent variable from the value of the independent variable. This is useful only if there is a linear association between the variables.

We can measure the statistical significance of the correlation using Spearman's rank correlation *rho*. This shows us if the variables are significantly monotonically related. A monotonic relationship between two variables implies that as one variable increases, the other variable either consistently increases or consistently decreases. The key characteristic is the preservation of the direction of change, though the rate of change may vary.

**Option 1:** Conduct a correlation test between the dependent variable and each independent variable one at a time.


``` r
cor.test(advertising_data$Sales, advertising_data$YouTube, method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  advertising_data$Sales and advertising_data$YouTube
## S = 1.4768e+15, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.4328903
```

``` r
cor.test(advertising_data$Sales, advertising_data$TikTok, method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  advertising_data$Sales and advertising_data$TikTok
## S = 1.9403e+15, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.2549296
```

``` r
cor.test(advertising_data$Sales, advertising_data$Facebook, method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  advertising_data$Sales and advertising_data$Facebook
## S = 1.7058e+15, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.3449712
```

**Option 2:** To view the correlation of all variables at the same time


``` r
cor(advertising_data, method = "spearman")
```

```
##                YouTube       TikTok      Facebook     Sales
## YouTube   1.0000000000 -0.001551803  0.0001865966 0.4328903
## TikTok   -0.0015518034  1.000000000 -0.0006869810 0.2549296
## Facebook  0.0001865966 -0.000686981  1.0000000000 0.3449712
## Sales     0.4328903496  0.254929620  0.3449711502 1.0000000
```

## [**Basic Visualizations**]{.underline}

### **Histogram**


``` r
par(mfrow = c(1, 2))
for (i in 1:4) {
  if (is.numeric(advertising_data[[i]])) {
    hist(advertising_data[[i]],
         main = names(advertising_data)[i],
         xlab = names(advertising_data)[i])
  } else {
    message(paste("Column", names(advertising_data)[i], "is not numeric and will be skipped."))
  }
}
```

![](2_multiple_linear_regression_files/figure-html/visualization_histogram-1.png)<!-- -->![](2_multiple_linear_regression_files/figure-html/visualization_histogram-2.png)<!-- -->

### **Box and Whisker Plot**


``` r
par(mfrow = c(1, 2))
for (i in 1:4) {
  if (is.numeric(advertising_data[[i]])) {
    boxplot(advertising_data[[i]], main = names(advertising_data)[i])
  } else {
    message(paste("Column", names(advertising_data)[i], "is not numeric and will be skipped."))
  }
}
```

![](2_multiple_linear_regression_files/figure-html/visualization_boxplot-1.png)<!-- -->![](2_multiple_linear_regression_files/figure-html/visualization_boxplot-2.png)<!-- -->

### **Missing Data Plot**


``` r
pacman::p_load("Amelia")

missmap(advertising_data, col = c("red", "grey"), legend = TRUE)
```

![](2_multiple_linear_regression_files/figure-html/missing_data_plot-1.png)<!-- -->

### **Correlation Plot**


``` r
pacman::p_load("ggcorrplot")

ggcorrplot(cor(advertising_data[,]))
```

![](2_multiple_linear_regression_files/figure-html/correlation_plot-1.png)<!-- -->

### **Scatter Plot**


``` r
pacman::p_load("corrplot")

pairs(advertising_data$Sales ~ ., data = advertising_data)
```

![](2_multiple_linear_regression_files/figure-html/scatter_plot_1-1.png)<!-- -->


``` r
pacman::p_load("ggplot2")
ggplot(advertising_data,
       aes(x = YouTube, y = Sales)) + 
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "Relationship between Sales Revenue and \nExpenditure on YouTube Marketing (KES)",
    x = "Expenditure",
    y = "Sales"
  )
```

![](2_multiple_linear_regression_files/figure-html/scatter_plot_2-1.png)<!-- -->


``` r
pacman::p_load("dplyr")
advertising_data_composite <- advertising_data %>%
  mutate(Total_Expenditure = YouTube + TikTok + Facebook)

ggplot(advertising_data_composite,
       aes(x = Total_Expenditure, y = Sales)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(
    title = "Relationship between Sales Revenue and \nTotal Marketing Expenditure (KES)",
    x = "Total Marketing Expenditure",
    y = "Sales"
  )
```

![](2_multiple_linear_regression_files/figure-html/scatter_plot_3-1.png)<!-- -->

# Statistical Test

We then apply a simultaneous multiple linear regression as a statistical test for regression. The term "simultaneous" refers to how the predictor variables are entered and considered in the statistical test. It means that all the predictor variables included in the model are entered and evaluated at the same time.


``` r
mlr_test <- lm(Sales ~ YouTube + TikTok + Facebook, data = advertising_data)
```

View the summary of the model.


``` r
summary(mlr_test)
```

```
## 
## Call:
## lm(formula = Sales ~ YouTube + TikTok + Facebook, data = advertising_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2275929  -408789    -6477   398490  2757261 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.368e+05  4.170e+03   56.78   <2e-16 ***
## YouTube     8.076e+00  2.910e-02  277.54   <2e-16 ***
## TikTok      7.303e+00  4.409e-02  165.63   <2e-16 ***
## Facebook    7.844e+00  3.531e-02  222.12   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 587400 on 249996 degrees of freedom
## Multiple R-squared:  0.3806,	Adjusted R-squared:  0.3806 
## F-statistic: 5.121e+04 on 3 and 249996 DF,  p-value: < 2.2e-16
```

To obtain a 95% confidence interval:


``` r
confint(mlr_test, level = 0.95)
```

```
##                    2.5 %       97.5 %
## (Intercept) 2.286081e+05 2.449535e+05
## YouTube     8.018530e+00 8.132589e+00
## TikTok      7.216730e+00 7.389574e+00
## Facebook    7.774366e+00 7.912791e+00
```

# Diagnostic EDA

## [**Test of Linearity**]{.underline}

For the model to pass the test of linearity, there should be no pattern in the distribution of residuals and the residuals should be randomly placed around the 0.0 residual line, i.e., the residuals should randomly vary around the mean of the value of the response variable.


``` r
plot(mlr_test, which = 1)
```

![](2_multiple_linear_regression_files/figure-html/test_of_linearity-1.png)<!-- -->

## [**Test of Independence of Errors (Autocorrelation)**]{.underline}

This test is necessary to confirm that each observation is independent of the other. It helps to identify autocorrelation that is introduced when the data is collected over a close period of time or when one observation is related to another observation. Autocorrelation leads to underestimated standard errors and inflated t-statistics. It can also make findings appear more significant than they actually are. The "Durbin-Watson Test" can be used as a test of independence of errors (test of autocorrelation). A Durbin-Watson statistic close to 2 suggests no autocorrelation, while values approaching 0 or 4 indicate positive or negative autocorrelation, respectively.

For the Durbin-Watson test:

-   The null hypothesis, H~0~, is that there is no autocorrelation (no autocorrelation = there is no correlation between residuals across time or across observations).

-   The alternative hypothesis, H~a~, is that there is autocorrelation (autocorrelation = there is a correlation between residuals across time or across observations)

If the p-value of the Durbin-Watson statistic is greater than 0.05 then there is no evidence to reject the null hypothesis that "there is no autocorrelation".


``` r
pacman::p_load("lmtest")
dwtest(mlr_test)
```

```
## 
## 	Durbin-Watson test
## 
## data:  mlr_test
## DW = 0.75605, p-value < 2.2e-16
## alternative hypothesis: true autocorrelation is greater than 0
```

With a Durbin-Watson test statistic of 0.76 and a *p* \< 0.05 in this case, we reject the null hypothesis that states that there is no autocorrelation. In other words, we conclude that the data contains autocorrelation.

## [**Test of Normality**]{.underline}

The test of normality of the distribution of the errors assesses whether the errors (residuals) are approximately normally distributed, i.e., most errors are close to zero and large errors are rare. A Q-Q plot can be used to conduct the test of normality.

A Q-Q plot is a scatterplot of the quantiles of the errors against the quantiles of a normal distribution. Quantiles are statistical values that divide a dataset or probability distribution into equal-sized intervals. They help in understanding how data is distributed by marking specific points that separate the data into groups of equal size. Examples of quantiles include: quartiles (4 equal parts), percentiles (100 equal parts), deciles (10 equal parts), etc.

If the points in the Q-Q plot fall along a straight line, then the normality assumption is satisfied. If the points in the Q-Q plot do not fall along a straight line, then the normality assumption is not satisfied.


``` r
plot(mlr_test, which = 2)
```

![](2_multiple_linear_regression_files/figure-html/test_of_normality-1.png)<!-- -->

## [**Test of Homoscedasticity**]{.underline}

Homoscedasticity requires that the spread of residuals should be constant across all levels of the independent variable. A scale-location plot (a.k.a. spread-location plot) can be used to conduct a test of homoscedasticity.

The x-axis shows the fitted (predicted) values from the model and the y-axis shows the square root of the standardized residuals. The red line is added to help visualize any patterns.

In a model with homoscedastic errors (equal variance across all predicted values):

-   Points should be randomly scattered around a horizontal line

-   The smooth line should be approximately horizontal

-   The vertical spread of points should be roughly equal across all fitted values

-   No obvious patterns, funnels, or trends should be visible

Points forming a cone shape that widens from left to right suggests heteroscedasticity with increasing variance for larger fitted values.


``` r
plot(mlr_test, which = 3)
```

![](2_multiple_linear_regression_files/figure-html/test_of_homoscedasticity-1.png)<!-- -->

**Breusch-Pagan Test**

The Breusch-Pagan Test can also be used in addition to the visual inspection of a Scale-Location plot.

Formally:

-   Null hypothesis (H₀): The residuals are homoscedastic (equal variance).

-   Alternative hypothesis (H₁): The residuals are heteroscedastic (non-constant variance).

p-Value:

-   p-value ≥ 0.05: Fail to reject H₀ → no evidence of heteroscedasticity → good, model passes.

-   p-value \< 0.05: Reject H₀ → evidence of heteroscedasticity → bad, model fails.

Interpretation: If the p-value is less than 0.05, then we reject the null hypothesis that states that “the residuals are homoscedastic”

With a p-value \< 0.01, there is statistically significant evidence of heteroscedasticity in the residuals in this case (which is bad).


``` r
pacman::p_load("lmtest")
lmtest::bptest(mlr_test)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  mlr_test
## BP = 304.78, df = 3, p-value < 2.2e-16
```

## [**Quantitative Validation of Assumptions**]{.underline}

The graphical representations of the various tests of assumptions should be accompanied by quantitative values. The `gvlma` package (Global Validation of Linear Models Assumptions) is useful for this purpose.


``` r
pacman::p_load("gvlma")
gvlma_results <- gvlma(mlr_test)
summary(gvlma_results)
```

```
## 
## Call:
## lm(formula = Sales ~ YouTube + TikTok + Facebook, data = advertising_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2275929  -408789    -6477   398490  2757261 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.368e+05  4.170e+03   56.78   <2e-16 ***
## YouTube     8.076e+00  2.910e-02  277.54   <2e-16 ***
## TikTok      7.303e+00  4.409e-02  165.63   <2e-16 ***
## Facebook    7.844e+00  3.531e-02  222.12   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 587400 on 249996 degrees of freedom
## Multiple R-squared:  0.3806,	Adjusted R-squared:  0.3806 
## F-statistic: 5.121e+04 on 3 and 249996 DF,  p-value: < 2.2e-16
## 
## 
## ASSESSMENT OF THE LINEAR MODEL ASSUMPTIONS
## USING THE GLOBAL TEST ON 4 DEGREES-OF-FREEDOM:
## Level of Significance =  0.05 
## 
## Call:
##  gvlma(x = mlr_test) 
## 
##                     Value   p-value                   Decision
## Global Stat        864.54 0.000e+00 Assumptions NOT satisfied!
## Skewness           351.14 0.000e+00 Assumptions NOT satisfied!
## Kurtosis           132.65 0.000e+00 Assumptions NOT satisfied!
## Link Function       55.12 1.131e-13 Assumptions NOT satisfied!
## Heteroscedasticity 325.63 0.000e+00 Assumptions NOT satisfied!
```

## Test of Multicollinearity

Multicollinearity arises when two or more independent variables (predictors) are highly intercorrelated. The **Variance Inflation Factor (VIF)** quantifies how much the variance of a coefficient estimate is “inflated” due to multicollinearity.

A VIF of 1 indicates no collinearity; values above 5 suggest problematic levels of collinearity. High VIF values (VIF \> 5) suggest that the coefficient estimates are less reliable due to the correlations between predictors.


``` r
pacman::p_load("car")
vif(mlr_test)
```

```
##  YouTube   TikTok Facebook 
## 1.000002 1.000003 1.000001
```

# Interpretation of the Results

## Limitations and Diagnostic Findings

**Methodological Limitation:** Lack of experimental variation in advertisement expenditure limits causal attribution to any single platform.

**Diagnostic Findings:** The regression residuals exhibited evidence of heteroscedasticity, as indicated by a significant Breusch–Pagan test, $\chi^2$(3) = 304.78, p \< .001. Additionally, the Durbin–Watson test revealed evidence of positive autocorrelation in the residuals, *D* = 0.76, *p* \< .01. These results suggest that the assumptions of constant error variance and independence were not fully met, thereby limiting the validity of the standard error estimates and the inferential tests (e.g., t-tests and F-tests).

## Academic Statement (APA) - without data transformation

A simultaneous multiple linear regression analysis was conducted on data from 250,000 observations (*N*=250,000) to examine whether advertising expenditures on YouTube, TikTok, and Facebook collectively predict Sales. The results indicated that expenses on YouTube ($\beta$ = 8.08, 95% CI [8.02, 8.13], SE = 0.02, *t*(249,996) = 277.54, *p* \< 0.01), TikTok ($\beta$ = 7.30, 95% CI [7.22, 7.39], SE = 0.04, *t*(249,996) = 165.63, *p* \< 0.01), and Facebook ($\beta$ = 7.84, 95% CI [7.77, 7.91], SE = 0.35, *t*(249,996) = 222.12, *p* \< 0.01) significantly predicted Sales (all large t-values and *p* \< 0.05). The model explained 38.06% of the variance in Sales (Multiple R^2^ = 0.38, Adjusted R^2^ = 0.38, *F*(3, 249,996) = 51,210, *p* \< 0.01). The intercept was 236,800, 95% CI [228,608.10, 244,953.50], SE = 4,170, *t*(249,996) = 56.78, *p* \< 0.01. The residual standard error was 587,400. This inidcates that the predictors significantly contributed towards explaining Sales, however, diagnostic findings relieved that violations of homoscedasticity and independence of residuals (autocorrelation), suggesting that the model's robustness and reliability of standard errors may be limited. The results are presented in the table below.

|  Predictor  | $\beta$ |          95% CI          |  SE   | *t*(249,996) |  *p*   |
|:-----------:|:-------:|:------------------------:|:-----:|:------------:|:------:|
| (Intercept) | 236,800 | [228,608.10, 244,953.50] | 4,170 |    56.78     | \< .01 |
|   YouTube   |  8.08   |       [8.02, 8.13]       | 0.02  |    277.54    | \< .01 |
|   TikTok    |  7.30   |       [7.22, 7.39]       | 0.04  |    165.63    | \< .01 |
|  Facebook   |  7.84   |       [7.77, 7.91]       | 0.35  |    222.12    | \< .01 |

: Regression Coefficients Predicting Sales from Multiple Advertising Channels

***Note.*** *N* = 250,000; *SE* = standard error; *CI* = confidence interval.

Although all advertising channels significantly predicted Sales, YouTube expenditure had a slightly stronger effect ($\beta$ = 8.08) compared to Facebook ($\beta$ = 7.84) and TikTok ($\beta$ = 7.30). This suggests that, among the three platforms, YouTube contributes marginally more to Sales when holding other expenditures constant.

## Business Analysis (Boardroom-Ready Language)

For every one Kenyan Shilling (KES) increase in YouTube advertising expenditure, Sales are predicted to increase by approximately 8.08 shillings, holding TikTok and Facebook expenditure constant. This is slightly higher than the corresponding effects for Facebook (7.84) and TikTok (7.30).

Recommendation:

-   Management should continue to treat YouTube, TikTok, and Facebook as complementary pillars of a unified digital marketing strategy. However, a modest increase in YouTube advertising expenditure is advisable, given its slightly stronger association with Sales performance. Continuous monitoring and ROI analysis should be maintained to ensure sustained effectiveness across platforms.

# References and Further Reading

American Psychological Association. (2025, February). *Journal Article Reporting Standards (JARS)*. APA Style. Retrieved April 28, 2025, from <https://apastyle.apa.org/jars>

Hodeghatta, U. R., & Nayak, U. (2023). *Practical Business Analytics Using R and Python: Solve Business Problems Using a Data-driven Approach* (2nd ed.). Apress. <https://link.springer.com/book/10.1007/978-1-4842-8754-5>
