#' Calculate change in count attributable to change in the underlying rate
#'
#' This is designed for internal use by the [sociodemographics::decompose_population_change()] function. Calculates the change in a count (eg: count of deaths) between two time points that we can attribute to a change in the underlying likelihood of the event occuring.
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
#' @return A numeric value indicating the change attributable to the rate change
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
rate_change_component<- function(age_specific_interest_counts, age_specific_population, count_variable, age_variable, population_variable, time_variable, start){

  # Calculate total event count for each time point
  total_count_t1<- age_specific_interest_counts%>%
    filter({{ time_variable }} == start)%>%
    summarise(n = sum(n, na.rm = T))

  total_count_t1<- total_count_t1$n[1]

  total_count_t2<- age_specific_interest_counts%>%
    filter({{ time_variable }} != start)%>%
    summarise(n = sum(n, na.rm = T))

  total_count_t2<- total_count_t2$n[1]

  # Calculate change in count of event
  count_change<- total_count_t2 - total_count_t1

  # Calculate growth component
  growth_component<- population_growth_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

  # Calculate ageing component
  ageing_component<- population_ageing_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

  # Calculate rate change component
  rate_component<- count_change - (growth_component + ageing_component)

  return(rate_component)
}
