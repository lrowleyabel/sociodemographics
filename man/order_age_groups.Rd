% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_age_groups.R
\name{order_age_groups}
\alias{order_age_groups}
\title{Order age group variables from youngest to oldest}
\usage{
order_age_groups(x)
}
\arguments{
\item{x}{A character vector containing the age groups}
}
\value{
A factor ordered from youngest to oldest age group
}
\description{
Turns a character vector of age groups into an factor with levels ordered by the lowest age in each age group. This is useful to prevent age groups being sorted alphabetically in plots and tables.
Groups such as "Aged 5 or under" are treated as having a lowest age of 0.
}
\examples{
df<- data.frame(age_group = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 14"), n = c(21, 15, 34))

df$age_group<- order_age_groups(df$age_group)
}
