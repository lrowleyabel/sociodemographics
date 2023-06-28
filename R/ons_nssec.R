#' Convert NS-SEC variable to correctly ordered ONS categories
#'
#' Convert NS-SEC values to the closest ONS NS-SEC category and return them as an appropriately ordered factor. The ONS categories used are the 8 class or 5 class versions given in Section 7 of the \href{https://www.ons.gov.uk/methodology/classificationsandstandards/otherclassifications/thenationalstatisticssocioeconomicclassificationnssecrebasedonsoc2010#classes-and-collapses}{ONS methodology page}. If `exact` is `FALSE` (the default) fuzzy matching will be used and the user will be asked to confirm matches.
#'
#' @param x Character vector of NS-SEC values to be converted.
#' @param exact Logical value indicating whether to use exact matching to the ONS categories. Default is `FALSE`.
#' @param detailed Logical value indicating whether to use detailed ONS categories (8 class version). If `FALSE` the broad ONS categories (5 class version) will be used. Default is `TRUE`.
#' @param keep_original Logical value indicating whether to return the original NS-SEC categories provided. If `TRUE` the function will simply order the provided categories by the order of the matched ONS categories. Default is `FALSE`.
#' @param match_distance Maximum allowed distance for fuzzy matching. Default is `0.3`.
#'
#' @return A factor of NS-SEC values ordered by the order of the ONS categories.
#' @importFrom utils data
#' @importFrom dplyr left_join
#' @importFrom fuzzyjoin stringdist_join
#' @importFrom dplyr group_by
#' @importFrom dplyr slice_min
#' @importFrom cli cli_div
#' @importFrom cli cli_text
#' @importFrom cli cli_end
#' @importFrom cli cli_h1
#' @export
#'
#' @examples
#' # Add example
ons_nssec<- function(x, exact = FALSE, detailed = TRUE, keep_original = FALSE, match_distance = 0.3){

  # Load in ONS categories
  data("nssec_lookup")

  # Select detailed or broad ONS categories
  ons_col<- ifelse(detailed, 1, 2)
  nssec_lookup<- unique(nssec_lookup[,ons_col])
  colnames(nssec_lookup)<- "ons_nssec"

  # Match provided categories to ONS categories
  provided_df<- data.frame(provided = x)

  if (exact){

    match<- left_join(provided_df, nssec_lookup, by = c("provided_df" = "ons_nssec"))

  } else {

    # Fuzzy match provided categories to ONS categories
    match<- stringdist_join(provided_df, nssec_lookup, mode = "left", by = c("provided" = "ons_nssec"), method = "jw", max_dist = match_distance, distance_col = "dist")%>%
      group_by(provided)%>%
      slice_min(order_by = dist, n = 1, with_ties = F)

    # Print matches to the console
    for (i in 1:nrow(unique(match))){
      cli_h1("Matching to ONS Categories")
      cli_div(theme = list(span.var1 = list(color = "green"), span.var2 = list(color = "blue")))
      cli_text("{.var1 {unique(match[i,1])}} matched to {.var2 {unique(match[i,2])}}")
      cli_text()
      cli_end()

    }

    # Confirm matches with user
    user_confirmation<- readline("Are you happy with these matches? (y/n)")
    if (user_confirmation != "y") stop("Aborting")

  }

  # Return a factor ordered by the ONS categories' order
  if (keep_original){

    levels_df<- left_join(nssec_lookup, match, by = "ons_nssec")

    return(factor(match$ons_nssec, levels = unique(levels_df$provided)))

  } else {

    return(factor(match$ons_nssec, levels = unique(nssec_lookup$ons_nssec)))

  }

}
