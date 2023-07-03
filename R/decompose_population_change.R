#' Decompose the change in a count of interest between two time points
#'
#' For an observed change between two time points in a count of interest in the population (eg: count of deaths), calculates the population growth component, the population ageing component and the rate change component.
#'   - **Population growth component**: the change in the count that we would expect to see from the overall change in the size of the population.
#'   - **Population ageing component**: the change in the count that we would expect to see from the change in the age structure of the population.
#'   - **Rate change component**: the change in the count that is attributable to a change in the underlying likelihood of the event occurring, independent of population growth and ageing.
#'
#' @param age_specific_interest_counts A dataframe of age-specific counts of the event of interest at each time point/
#' @param age_specific_population A dataframe of the population size in each age group at each time point.
#' @param count_variable A variable providing the counts in `age_specific_interest_counts`.
#' @param age_variable A variable providing the age groups in `age_specific_interest_counts` and `age_specific_population`.
#' @param population_variable A variable providing the population sizes in `age_specific_population`.
#' @param time_variable A variable providing the time points in `age_specific_interest_counts` and `age_specific_population`. This variable must only have two distinct values in each dataframe.
#' @param start The value of `time_variable` which indicates the first time point.
#'
#' @return A dataframe containing the raw counts attributable to each component and the observed change. An additional column indicates this raw change as a proportion (not a percent) of the observed change.
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

decompose_population_change<- function(age_specific_interest_counts, age_specific_population, count_variable, age_variable, population_variable, time_variable, start){

  # Calculate growth component
  growth_component<- population_growth_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

  # Calculate ageing component
  ageing_component<- population_ageing_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

  # Calculate rate change component
  rate_component<- rate_change_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

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

  # Return dataframe of components as raw count changes and as proportions of the observed change
  out<- data.frame(component = factor(c("Observed change", "Population growth component", "Population ageing component", "Rate change component"), levels = c("Observed change", "Population growth component", "Population ageing component", "Rate change component")),
                   raw_change = c(count_change, growth_component, ageing_component, rate_component),
                   proportion = c(count_change/count_change, growth_component/abs(count_change), ageing_component/abs(count_change), rate_component/abs(count_change)))



  return(out)

}
