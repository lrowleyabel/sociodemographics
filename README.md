
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sociodemographics

<!-- badges: start -->
<!-- badges: end -->

This is an R package collecting a few functions for dealing with
socio-demographic variables. It is currently under development.

## Installation

You can install the development version of sociodemographics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lrowleyabel/sociodemographics")
```

## Example

This is a basic example which shows how to use the `order_age_groups`
function:

``` r
library(sociodemographics)
library(dplyr)
library(ggplot2)

# Example data with age group counts
df<- data.frame(age_group = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 14"), n = c(21, 15, 34))

# Plotting the data results in incorrect order of age groups
ggplot(df)+
  geom_col(aes(x = n, y = age_group), width = 0.5)+
  theme_minimal()+
  theme(plot.title.position = "plot")+
  labs(x = "Count", y = "", title = "Plot with Incorrect Order")
```

<img src="man/figures/README-example-1.png" width="75%" />

``` r

# Order the age groups correctly
df<- df%>%
  mutate(age_group = order_age_groups(age_group))

# Re-plot the data to see the correctly ordered age groups
ggplot(df)+
  geom_col(aes(x = n, y = age_group), width = 0.5)+
  theme_minimal()+
  theme(plot.title.position = "plot")+
  labs(x = "Count", y = "", title = "Plot with Correct Order")
```

<img src="man/figures/README-example-2.png" width="75%" />
