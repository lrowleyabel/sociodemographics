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
