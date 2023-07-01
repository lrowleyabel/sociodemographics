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
#' @param sum_counts_across Optional variable providing categories across which to sum the event counts.
#' @param exclude_counts Optional character string or vector providing of categories in the `sum_counts_across` variable which should be exlcuded from the counts. Only applies is `sum_counts_across` is not `NULL`.
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
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # Add Example
age_standardise_indirect<- function(interest_counts, interest_age_specific_population, standard_age_specific_counts, standard_age_specific_population, count_variable, population_variable, age_variable, interest_grouping_variable = NULL, sum_counts_across = NULL, exclude_counts = NULL){

  # Check all data is ungrouped
  interest_counts<- ungroup(interest_counts)
  interest_age_specific_population<- ungroup(interest_age_specific_population)
  standard_age_specific_counts<- ungroup(standard_age_specific_counts)
  standard_age_specific_population<- ungroup(standard_age_specific_population)

  if (!is.null(as.character(substitute(sum_counts_across)))){

    # Filter out counts to be excluded if given
    if(!is.null(exclude_counts)){

      interest_counts<- interest_counts%>%
        dplyr::filter(!{{ sum_counts_across }} %in% exclude_counts)

      standard_age_specific_counts<- standard_age_specific_counts%>%
        dplyr::filter(!{{ sum_counts_across }} %in% exclude_counts)

    }

    # Sum counts
    if (is.null(as.character(substitute(interest_grouping_variable)))){

      interest_counts<- interest_counts%>%
        summarise({{ count_variable }} := sum({{ count_variable }}, na.rm = T))

      standard_age_specific_counts<- standard_age_specific_counts%>%
        group_by({{ age_variable }})%>%
        summarise({{ count_variable }} := sum({{ count_variable }}, na.rm = T))

    } else {

      interest_counts<- interest_counts%>%
        group_by({{ interest_grouping_variable }})%>%
        summarise({{ count_variable }} := sum({{ count_variable }}, na.rm = T))

      standard_age_specific_counts<- standard_age_specific_counts%>%
        group_by({{ age_variable }})%>%
        summarise({{ count_variable }} := sum({{ count_variable }}, na.rm = T))

    }



  }



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

  # Sum expected counts across age groups and join to observed counts
  if (is.null(as.character(substitute(interest_grouping_variable)))){

    interest_expected_count<- interest_age_specific_expected%>%
      summarise(expected_count = sum(age_specific_expected_count, na.rm = T))

    interest_counts<- cbind(interest_counts, interest_expected_count)

  } else {

    interest_expected_count<- interest_age_specific_expected%>%
      group_by({{ interest_grouping_variable }})%>%
      summarise(expected_count = sum(age_specific_expected_count, na.rm = T))

    interest_counts<- left_join(interest_counts, interest_expected_count, by = as.character(substitute(interest_grouping_variable)))

  }


  # Calculate age-standardised rate
  interest_counts<- interest_counts%>%
    mutate(age_standardised_rate = {{ count_variable }}/expected_count)

  return(interest_counts)

}
