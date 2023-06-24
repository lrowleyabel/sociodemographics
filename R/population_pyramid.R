#' Plot a population pyramid
#'
#' @param data A dataframe containing counts for each age group within two subpopulations. If a reference population is also being plotted, there must also be a variable distinguishing between the main population of interest and the reference population. The dataframe must be in tidy format (ie: one row per age group-subpopulation combination, with additional rows for the reference population if being used).
#' @param count A variable providing population counts
#' @param age A variable providing age groups
#' @param side A variable that will define whether counts are plotted on the left or the right of the pyramid
#' @param left A character string which is a value of the variable provided to ```side```. This category of ```side``` will be plotted on the left of the pyramid.
#' @param proportions A logical value indicating whether proportions of the total population should be plotted instead of the raw counts. If ```TRUE``` the proportion within the total population (including both subpopulations) will be plotted. If a reference population is also being plotted, the proportion is calculated within the main population and the reference population separately. Default is ```FALSE```.
#' @param reference_indicator An optional variable indicating whether counts are for the main population or the reference population (only used if ```reference_population``` is not ```NULL```). Default is `NULL`.
#' @param reference_population If not `NULL`, the age-structure for a reference population will be plotted on top of the pyramid bars. This should be a character string which is a value of the variable provided to `reference_indicator` which gives the name of the reference population. Default is `NULL`.
#' @param pal A colour palette used to distinguish the subpopulations.
#' @param colour A colour for the pyramid bar and reference point outlines. Default is `"black"`.
#' @param linewidth A line width for the pyramid bars outline. Default is `1`.
#' @param width A width for the pyramid bars. Default is `0.5`
#' @param reference_point_size A size for the reference points. Default is `3`.
#' @param reference_point_linewidth A line width for the reference points outline. Default is `1`.
#' @param centre_zero A logical value indicating whether to centre the x-axis around zero. Default is `TRUE`.
#'
#' @return A population pyramid as a `ggplot` object
#' @importFrom dplyr summarise
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom rlang  :=
#' @export
#'
#' @examples
#' df<- data.frame(age_group = rep(c("Aged 0 to 19", "Aged 20 to 39", "Aged 40 to 59", "Aged 60 or higher"), 4),
#'                 sex = rep(c("A", "A", "A", "A", "B", "B", "B", "B"), 2),
#'                 n = c(rep(c(900,1100,1400,1000),2), rep(c(600,1200,1300,800),2)),
#'                 population_type = c(rep("Population of interest", 8), rep("Reference population", 8)))
#'
#' population_pyramid(data = df,
#'                    count = n,
#'                    age = age_group,
#'                    side = sex,
#'                    left = "A",
#'                    reference_indicator = population_type,
#'                    reference_population = "Reference population")
#'
population_pyramid<- function(data, count, age, side, left, proportions = FALSE, reference_indicator = NULL, reference_population = NULL, pal = NULL, colour = "black", linewidth = 1, width = 0.5, reference_point_size = 3, reference_point_linewidth = 1, centre_zero = TRUE){

  # Check value of left is in the side variable
  if (!left %in% data[[substitute(side)]]){
    stop("Value provided to left is not in variable provided to side")
  }

  # Get count range
  count_range<- data%>%
    summarise(count_min = min({{ count }}, na.rm = TRUE), count_max = max({{ count }}, na.rm = T))

  # Calculate per-capita values if these are being plotted
  if (proportions){

    if (is.null(reference_population)){

      # If no reference population, calculate proportion of total dataframe count
      total_population<- data%>%
        summarise(total = sum({{ count }}, na.rm = T))

      data<- data%>%
        mutate(total := total_population$total[1])

      data<- data%>%
        mutate({{ count }} := {{ count }}/total)

    } else {

      # If reference population, calculate proportion within population groups
      total_population<- data%>%
        group_by({{ reference_indicator }})%>%
        summarise(total = sum({{ count }}, na.rm = T))

      data<- left_join(data, total_population, by = as.character(substitute(reference_indicator)))

      data<- data%>%
        mutate({{ count }} := 100*{{ count }}/total)


    }

  }

  # Negate the count for the left category
  data<- data%>%
    mutate({{ count }} := case_when({{ side }} == left ~ -1*{{ count }},
                                   TRUE ~ {{ count }}))

  # Order age groups correctly
  data<- data%>%
    mutate({{ age }} := order_age_groups({{ age }}))

  # Define x-axis limits
  if (centre_zero){
    x_limits<- c(-1*count_range$count_max[1], count_range$count_max[1])
  } else {
    x_limits<- c(NA,NA)
  }

  # Define default palette if none provided
  if (is.null(pal)){
    pal<- c("#F1BB7B", "#FD6467")
  }

  # Create plot without or with the reference count
  if (is.null(reference_population)){

    # Create plot
    p<- ggplot(data)+
      geom_col(aes(x = {{ count }}, y = {{ age }}, fill = {{ side }}), colour = colour, linewidth = linewidth, width = width)+
      scale_x_continuous(limits = x_limits, labels = function(x)format(abs(x), big.mark = ","))+
      scale_fill_manual(values = {{ pal }})+
      theme_minimal()

  } else {

    # Separate main and reference data
    main_data<- data%>%
      filter({{ reference_indicator }} != reference_population)

    reference_data<- data%>%
      filter({{ reference_indicator }} == reference_population)

    # Get name of non-reference population
    non_reference_population<- data[[substitute(reference_indicator)]][data[[substitute(reference_indicator)]] != reference_population][1]

    # Create plot
    p<- ggplot()+
      geom_col(data = main_data, aes(x = {{ count }}, y = {{ age }}, fill = {{ side }}), colour = colour, linewidth = linewidth, width = width)+
      scale_fill_manual(values = {{ pal }}, name = non_reference_population)+
      guides(fill = guide_legend(keywidth = 1, keyheight = 0.5))+
      ggnewscale::new_scale_fill()+
      geom_point(data = reference_data, aes(x = {{ count }}, y = {{ age }}, fill = {{ side }}), shape = 21, colour = colour, size = reference_point_size, stroke = reference_point_linewidth)+
      scale_x_continuous(limits = x_limits, labels = function(x)format(abs(x), big.mark = ","))+
      scale_fill_manual(values = {{ pal }}, name = reference_population)+
      guides(fill = guide_legend(keyheight = 0.5, override.aes = list(size = 3)))+
      theme_minimal()


  }

  # Set axis labels

  if (proportions){
    p<- p+labs(x = "Proportion of total population (%)", y = "")
  } else {
    p<- p+labs(x = "Count", y = "")
  }



  return(p)

}
