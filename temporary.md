---
title: "Simple Linear Regression"
author: "Allan Omondi, student ID, Group D"
date: "2025-05-19"
output:
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
  html_document:
    toc: true
    toc_depth: 4
    number_sections: true
    fig_width: 6
    fig_height: 6
    self_contained: false
    keep_md: true
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

clv_data <- read_csv("./data/clv_data.csv")
```

```
## Rows: 500 Columns: 2
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (2): purchase_frequency, customer_lifetime_value
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
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
