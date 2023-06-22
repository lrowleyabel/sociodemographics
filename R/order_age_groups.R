#' Order age group variables from youngest to oldest
#'
#' Turns a character vector of age groups into an factor with levels ordered by the lowest age in each age group. This is useful to prevent age groups being sorted alphabetically in plots and tables.
#' Groups such as "Aged 5 or under" are treated as having a lowest age of 0.
#'
#' @param x A character vector containing the age groups
#'
#' @return A factor ordered from youngest to oldest age group
#' @importFrom dplyr %>%
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' df<- data.frame(age_group = c("Aged 0 to 4", "Aged 5 to 9", "Aged 10 to 14"), n = c(21, 15, 34))
#'
#' df$age_group<- order_age_groups(df$age_group)

order_age_groups<- function(x){

  # Get lowest age for each group
  lower_bounds<- lapply(x, FUN = function(age_group){
    if (str_detect(age_group, "under|less|lower|younger")){
      return(0)
    } else {
      return(as.numeric(str_extract(age_group, "[0-9]{1,3}")))
    }
  })%>%
    unlist()

  # Create vector of ordered age groups to use as levels
  age_group_levels<- unique(x[order(lower_bounds)])

  # Return original vector as factor with new levels
  return(factor(x, levels = age_group_levels))

}
