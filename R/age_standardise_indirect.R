#' Calculate indirectly age-standardised rates
#'
#' Calculates indirectly age-standardised rates of an event for a population of interest, based on the age-specific rates in a standard population.
#'
#'
#' @param interest_counts Count of the event in the population of interest
#' @param interest_age_specific_population Population size for each age group in the population of interest
#' @param standard_age_specific_counts Count of event the for each age group in the standard population
#' @param standard_age_specific_population Population size for each age group in the standard population
#' @param count_variable Variable providing counts of the event. This must be the same in each dataset provided.
#' @param population_variable Variable providing population sizes. This must be the same in each dataset provided.
#' @param age_variable Variable providing age groups. This must be the same in each dataset provided.
#' @param interest_grouping_variable Optional variable providing groups within the population of interest. If provided, rates will be calculated separately for each group. This is useful if you wish to look at how rates vary across a variable of interest in an age-standardised way (eg: look at the rate of the event in each ethnic group while standardising to the general populaiton's age structure).
#'
#' @return A tibble containing observed counts, expected counts and indirectly age-standardised rates of the event for the population of interest.
#'
#' @details
#' This type of age standardisation requires the following four datasets:
#'
#'  - counts of the event for the population of interest
#'  - population sizes in each age group for the population of interest
#'  - age-specific counts of the event for the standard population
#'  - population sizes in each age group for the standard population
#'
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @export
#'
#' @examples
#' # Add Example
age_standardise_indirect<- function(interest_counts, interest_age_specific_population, standard_age_specific_counts, standard_age_specific_population, count_variable, population_variable, age_variable, interest_grouping_variable){

  # Check all data is ungrouped
  interest_counts<- ungroup(interest_counts)
  interest_age_specific_population<- ungroup(interest_age_specific_population)
  standard_age_specific_counts<- ungroup(standard_age_specific_counts)
  standard_age_specific_population<- ungroup(standard_age_specific_population)

  # Join standard population age group sizes to counts
  standard_age_specific_counts<- left_join(standard_age_specific_counts, standard_age_specific_population, by = as.character(substitute(age_variable)))

  # Calculate standard population age-specific rates
  standard_age_specific_rates<- standard_age_specific_counts%>%
    mutate(standard_age_specific_rate = {{ count_variable }}/{{ population_variable }})%>%
    select({{ age_variable }}, standard_age_specific_rate)

  # Join standard age-specific rates to interest population age group sizes
  interest_age_specific_population<- left_join(interest_age_specific_population, standard_age_specific_rates, by = as.character(substitute(age)))

  # Calculate expected count for each age group in the interest population
  interest_age_specific_expected<- interest_age_specific_population%>%
    mutate(age_specific_expected_count = {{ population_variable }}*standard_age_specific_rate)

  # Sum expected counts across age groups
  interest_expected_count<- interest_age_specific_expected%>%
    group_by({{ interest_grouping_variable }})%>%
    summarise(expected_count = sum(age_specific_expected_count, na.rm = T))

  # Join expected counts to observed counts
  interest_counts<- left_join(interest_counts, interest_expected_count, by = as.character(substitute(interest_grouping_variable)))

  # Calculate age-standardised rate
  interest_counts<- interest_counts%>%
    mutate(age_standardised_rate = {{ count_variable }}/expected_count)

  return(interest_counts)

}
