---
title: "Simple Linear Regression"
author: "<Type your Student ID and Name here>"
date: "2025-05-14"
output:
  html_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    fig_height: 6
    self_contained: false
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
  word_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    keep_md: true
---



# Load the Dataset


``` r
pacman::p_load("readr")

clv_data <- read_csv("./data/clv_data.csv")
head(clv_data)
```

```
## # A tibble: 6 × 2
##   purchase_frequency customer_lifetime_value
##                <dbl>                   <dbl>
## 1                  3                   110. 
## 2                  7                   190. 
## 3                  6                   160. 
## 4                  2                    94.4
## 5                  4                   133. 
## 6                  8                   223.
```

# Initial EDA


``` r
dim(clv_data)
```

```
## [1] 500   2
```


``` r
 sapply(clv_data, class)
```

```
##      purchase_frequency customer_lifetime_value 
##               "numeric"               "numeric"
```


``` r
str(clv_data)
```

```
## spc_tbl_ [500 × 2] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ purchase_frequency     : num [1:500] 3 7 6 2 4 8 0 4 8 3 ...
##  $ customer_lifetime_value: num [1:500] 110.3 190.2 160 94.4 133.2 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   purchase_frequency = col_double(),
##   ..   customer_lifetime_value = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

[**Descriptive Statistics**]{.underline}

## Measures of Frequency

## Measures of Central Tendency


``` r
summary(clv_data)
```

```
##  purchase_frequency customer_lifetime_value
##  Min.   :-1.000     Min.   : 26.13         
##  1st Qu.: 4.000     1st Qu.:122.04         
##  Median : 5.000     Median :148.21         
##  Mean   : 4.914     Mean   :148.25         
##  3rd Qu.: 6.000     3rd Qu.:175.88         
##  Max.   :11.000     Max.   :262.04
```

## Measures of Distribution


``` r
sapply(clv_data[,], var)
```

```
##      purchase_frequency customer_lifetime_value 
##                4.146898             1642.315996
```


``` r
 sapply(clv_data[,], sd)
```

```
##      purchase_frequency customer_lifetime_value 
##                2.036393               40.525498
```


``` r
pacman::p_load("e1071")
sapply(clv_data[,], kurtosis, type = 2)
```

```
##      purchase_frequency customer_lifetime_value 
##              -0.1220038              -0.1484811
```


``` r
sapply(clv_data[,], skewness, type = 2)
```

```
##      purchase_frequency customer_lifetime_value 
##             -0.04021915             -0.01608242
```


``` r
cov(clv_data, method = "spearman")
```

```
##                         purchase_frequency customer_lifetime_value
## purchase_frequency                20409.91                20235.73
## customer_lifetime_value           20235.73                20874.99
```


``` r
cor.test(clv_data$customer_lifetime_value, clv_data$purchase_frequency, method = "spearman")
```

```
## 
## 	Spearman's rank correlation rho
## 
## data:  clv_data$customer_lifetime_value and clv_data$purchase_frequency
## S = 409190, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.9803588
```


``` r
cor(clv_data, method = "spearman")
```

```
##                         purchase_frequency customer_lifetime_value
## purchase_frequency               1.0000000               0.9803588
## customer_lifetime_value          0.9803588               1.0000000
```

# Basic Visualization

## Histograms


``` r
 par(mfrow = c(1, 2))
 for (i in 1:2) {
 if (is.numeric(clv_data[[i]])) {
 hist(clv_data[[i]],
 main = names(clv_data)[i],
 xlab = names(clv_data)[i])
 } else {
 message(paste("Column", names(clv_data)[i], "is not numeric and will be skipped."))
 }
 }
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


``` r
 par(mfrow = c(1, 2))
 for (i in 1:2) {
 if (is.numeric(clv_data[[i]])) {
 boxplot(clv_data[[i]], main = names(clv_data)[i])
 } else {
 message(paste("Column", names(clv_data)[i], "is not numeric and will be skipped."))
 }
 }
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


``` r
pacman::p_load("Amelia")
missmap(clv_data, col = c("red", "grey"), legend = TRUE)
```

```
## Warning: Unknown or uninitialised column: `arguments`.
## Unknown or uninitialised column: `arguments`.
```

```
## Warning: Unknown or uninitialised column: `imputations`.
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


``` r
 pacman::p_load("ggcorrplot")
 ggcorrplot(cor(clv_data[,]))
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


``` r
 pacman::p_load("corrplot")
  pairs(customer_lifetime_value ~ ., data = clv_data, col = clv_data$customer_lifetime_value)
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


``` r
pacman::p_load("ggplot2")
 ggplot(clv_data,
 aes(x= purchase_frequency, y= customer_lifetime_value)) +
 geom_point()+
 geom_smooth(method= lm)+
 labs(
 title= "RelationshipbetweenCustomerLifetimeValueandPurchaseFrequency",
 x= "PurchaseFrequency",
 y= "CustomerLifetimeValue"
 )
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](sample-1_simple_linear_regression_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
