#' Calculate expected change in count from population growth
#'
#' This is designed for internal use by the [sociodemographics::decompose_population_change()] function. Calculates the change we would expect to see in a count (eg: count of deaths) between two time points based on the population growth over that period.
#'
#' @keywords internal
#' @param age_specific_interest_counts A dataframe of age-specific counts of the event of interest at each time point/
#' @param age_specific_population A dataframe of the population size in each age group at each time point.
#' @param count_variable A variable providing the counts in `age_specific_interest_counts`.
#' @param age_variable A variable providing the age groups in `age_specific_interest_counts` and `age_specific_population`.
#' @param population_variable A variable providing the population sizes in `age_specific_population`.
#' @param time_variable A variable providing the time points in `age_specific_interest_counts` and `age_specific_population`. This variable must only have two distinct values in each dataframe.
#' @param start The value of `time_variable` which indicates the first time point.
#'
#' @return A numeric value indicating expected change
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom rlang :=
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @export
#'
#' @examples
#' # Add example
population_growth_component<- function(age_specific_interest_counts, age_specific_population, count_variable, age_variable, population_variable, time_variable, start){

  # Calculate total population at each time point
  total_populations<- age_specific_population%>%
    group_by({{ time_variable }})%>%
    summarise(total = sum({{population_variable}}, na.rm = T))%>%
    ungroup()


  total_population_t1<- total_populations%>%
    filter({{ time_variable }} == start)

  total_population_t1<- total_population_t1$total[1]

  total_population_t2<- total_populations%>%
    filter({{ time_variable }} != start)

  total_population_t2<- total_population_t2$total[1]

  # Calculate total event count for time point 1
  total_count_t1<- age_specific_interest_counts%>%
    filter({{ time_variable }} == start)%>%
    summarise(n = sum(n, na.rm = T))

  total_count_t1<- total_count_t1$n[1]

  growth_component<- ((total_population_t2-total_population_t1)/total_population_t1)*total_count_t1

  return(growth_component)

}
