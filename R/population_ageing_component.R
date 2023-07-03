#' Calculate expected change in a count from population ageing
#'
#' This is designed for internal use by the [sociodemographics::decompose_population_change()] function. Calculates the change we expect to see in a count (eg: count of deaths) between two time points based on the ageing of the population over that period.
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

population_ageing_component<- function(age_specific_interest_counts, age_specific_population, count_variable, age_variable, population_variable, time_variable, start){

  # Calculate rates for event at time point 1
  df1<- age_specific_interest_counts%>%
    mutate(age_var = {{age_variable}},
           time_var = {{time_variable}})

  df2<- age_specific_population%>%
    rename(age_var = {{age_variable}},
           time_var = {{time_variable}})

  rates_t1<- left_join(df1, df2, by = c("age_var", "time_var"))

  rates_t1<- rates_t1%>%
    select(-c(age_var, time_var))

  rates_t1<- rates_t1%>%
    mutate(rate = {{count_variable}}/{{population_variable}})%>%
    filter({{time_variable}} == start)%>%
    select({{age_variable}}, rate)

  # Calculate expected count at time point 2 assuming rates from time point 1
  age_specific_population_t2<- age_specific_population%>%
    filter({{time_variable}} != start)%>%
    select(-{{time_variable}})

  df3<- rates_t1%>%
    rename(age_var = {{age_variable}})

  df4<- age_specific_population_t2%>%
    rename(age_var = {{age_variable}})

  expected_count_t2<- left_join(df3, df4, by = "age_var")

  expected_count_t2<- expected_count_t2%>%
    mutate(expected_count = rate*{{population_variable}})

  expected_count_t2<- sum(expected_count_t2$expected_count, na.rm = T)

  # Calculate growth component
  growth_component<- population_growth_component(age_specific_interest_counts = age_specific_interest_counts, age_specific_population = age_specific_population, count_variable = {{ count_variable }}, age_variable = {{ age_variable }}, population_variable = {{ population_variable }}, time_variable = {{ time_variable }}, start = start)

  # Calculate ageing component
  ageing_component<- expected_count_t2 - growth_component

  return(ageing_component)


}
