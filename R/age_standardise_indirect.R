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
#' @param grouping_variable Optional variable providing groups within the population. If provided, rates will be calculated separately for each group. This is useful if you wish to look at how rates vary across a variable of interest in an age-standardised way (eg: look at the rate of the event in each ethnic group while standardising to the general populaiton's age structure).
#' @param single_standard_population Logical value indicating whether to standardise each population group (as provided by `grouping_variable`) should be standardised to a single standard population. If `TRUE`, the groups will be standardised to the same standard population. If `FALSE`, the groups will be standardised to group-specific populations and the separate event counts and age group sizes must be provided for each group in the dataframes provided to `standard_age_specific_counts` and `standard_age_specific_population`.
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
#' @importFrom dplyr bind_rows
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # Add Example
age_standardise_indirect<- function(interest_counts, interest_age_specific_population, standard_age_specific_counts, standard_age_specific_population, count_variable, population_variable, age_variable, grouping_variable, single_standard_population){

  # Check required arguments are provided
  if (base::missing(interest_counts)) stop("Interest population counts data missing")
  if (base::missing(interest_age_specific_population)) stop("Interest population age-group size data missing")
  if (base::missing(standard_age_specific_counts)) stop("Standard population age-specific counts data missing")
  if (base::missing(standard_age_specific_population)) stop("Standard population age-group size data missing")

  # Check if grouping by a variable
  grouping<- !base::missing(grouping_variable)

  # Check all data is ungrouped
  interest_counts<- ungroup(interest_counts)
  interest_age_specific_population<- ungroup(interest_age_specific_population)
  standard_age_specific_counts<- ungroup(standard_age_specific_counts)
  standard_age_specific_population<- ungroup(standard_age_specific_population)


  if (grouping){

    # If grouping...

    # Check if standardising each group to the same standard population
    if (single_standard_population){

      # Replicate the standard population data for each group

      # Get the unique groups
      groups<- interest_counts[[as.character(substitute(grouping_variable))]]%>%
        unique()

      # Get the number of age groups
      n_age_groups<- nrow(standard_age_specific_population)

      # Create a vector of the groups, each repeated by the number of age groups
      repeated_groups<- unlist(groups)%>%
        rep(each = n_age_groups)%>%
        unname()

      standard_age_specific_counts<- replicate(standard_age_specific_counts, n = length(groups), simplify = FALSE)%>%
        bind_rows()%>%
        mutate({{ grouping_variable }} := repeated_groups)

      standard_age_specific_population<- replicate(standard_age_specific_population, n = length(groups), simplify = FALSE)%>%
        bind_rows()%>%
        mutate({{ grouping_variable }} := repeated_groups)

    }

    # Join standard population age group sizes to standard counts
    standard_age_specific_counts<- left_join(standard_age_specific_counts, standard_age_specific_population, by = c(as.character(substitute(age_variable)), as.character(substitute(grouping_variable))))

    # Calculate standard population age-specific rates
    standard_age_specific_rates<- standard_age_specific_counts%>%
      mutate(standard_age_specific_rate = {{ count_variable }}/{{ population_variable }})%>%
      select({{ age_variable }}, {{ grouping_variable }}, standard_age_specific_rate)

    # Join standard age-specific rates to interest population age group sizes
    interest_age_specific_population<- left_join(interest_age_specific_population, standard_age_specific_rates, by = c(as.character(substitute(age)), as.character(substitute(grouping_variable))))

    # Calculate expected count for each age group in the interest population
    interest_age_specific_expected<- interest_age_specific_population%>%
      mutate(age_specific_expected_count = {{ population_variable }}*standard_age_specific_rate)

    # Sum expected counts across age groups and join to observed counts
    interest_expected_count<- interest_age_specific_expected%>%
      group_by({{ grouping_variable }})%>%
      summarise(expected_count = sum(age_specific_expected_count, na.rm = T))

    interest_counts<- left_join(interest_counts, interest_expected_count, by = as.character(substitute(grouping_variable)))

    # Calculate age-standardised rate
    interest_counts<- interest_counts%>%
      mutate(age_standardised_rate = {{ count_variable }}/expected_count)

    return(interest_counts)


  } else {

    # If not grouping...

    # Join standard population age group sizes to standard counts
    standard_age_specific_counts<- left_join(standard_age_specific_counts, standard_age_specific_population, by = c(as.character(substitute(age_variable))))

    # Calculate standard population age-specific rates
    standard_age_specific_rates<- standard_age_specific_counts%>%
      mutate(standard_age_specific_rate = {{ count_variable }}/{{ population_variable }})%>%
      select({{ age_variable }}, standard_age_specific_rate)

    # Join standard age-specific rates to interest population age group sizes
    interest_age_specific_population<- left_join(interest_age_specific_population, standard_age_specific_rates, by = c(as.character(substitute(age))))

    # Calculate expected count for each age group in the interest population
    interest_age_specific_expected<- interest_age_specific_population%>%
      mutate(age_specific_expected_count = {{ population_variable }}*standard_age_specific_rate)

    # Sum expected counts across age groups and join to observed counts
    interest_expected_count<- interest_age_specific_expected%>%
      summarise(expected_count = sum(age_specific_expected_count, na.rm = T))

    interest_counts<- cbind(interest_counts, interest_expected_count)

    # Calculate age-standardised rate
    interest_counts<- interest_counts%>%
      mutate(age_standardised_rate = {{ count_variable }}/expected_count)

    return(interest_counts)

  }

}
