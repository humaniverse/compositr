#' Calculate the 'extent' scores when aggregating up small areas
#'
#' "Extent" is the proportion of the local population that live in areas
#' classified as among the most deprived in the higher geography.
#'
#' @section IMD technical report:
#'
#' The calculation of extent is taken from the IMD technical report Appendix N:
#'
#' "The population living in the most deprived 11 to 30 per cent of Lower-layer
#' Super Output Areas receive a sliding weight, ranging from 0.95 for those in
#' the most deprived eleventh percentile, to 0.05 for those in the most deprived
#' thirtieth percentile. In practice this means that the weight starts from 0.95
#' in the most deprived eleventh percentile, and then decreases by
#' (0.95-0.05)/19 for each of the subsequent nineteen percentiles until it
#' reaches 0.05 for the most deprived thirtieth percentile, and zero for areas
#' outside the most deprived 30 per cent"
#'
#' The direction of the scale of data inputted by the functioin always matches
#' that of the data outputted. For example if data is inputted into the function
#' where high scores equals high vulnerability, the outputted data set will hold
#' this relationship true.
#'
#' @param data A dataframe containing a variable to be aggregated, lower level
#'   geography population estimates, and a higher level geographical grouping
#'   variable.
#' @param x Name of the variable in the dataframe containing the variable to be
#'   aggregated (e.g., score) for the lower level geography.
#' @param higher_id Name of the variable in the dataframe containing the higher
#'  level geography names/codes.
#' @param lower_pop Name of the variable in the dataframe containing the
#'   population estimates of the lower level geography.
#' @param weight_high_scores If TRUE higher scores are weighted, else lower
#'   scores are weighted. For indicators like 'Alchol Misuse' and 'Ambulance
#'   Wait Time' this should be set to TRUE. This is because higher values in
#'   these  outcomes indicate worse outcomes (higher vulnerability and lower
#'   capacity) and this is where the weighting should be focused. For indicators
#'   like 'Physical Activity' and 'Bed Availability' it should be set to FALSE.
#'   This is because lower values in these outcomes indicate worse outcomes
#'   (higher vulnerability and lower capacity) and this is where the weighting
#'   should be focused.
#'
#' @export
#' @autoglobal
#'
#' @examples
#' \dontrun{
#' aggregate_extent(
#'   example_aggregate_data,
#'   score,
#'   higher_geography_code,
#'   lower_geography_population
#' )
#' }
aggregate_extent <- function(data,
                             x,
                             higher_id,
                             lower_pop,
                             weight_high_scores = TRUE) {
  data <- data |>
    dplyr::mutate(percentile = quantise({{ x }}, 100))

  if (weight_high_scores) {
    data <- dplyr::mutate(data, percentile = invert_this(percentile))
  }

  data <- data |>
    dplyr::mutate(
      extent = dplyr::case_when(
        percentile <= 10 ~ {{ lower_pop }},
        percentile == 11 ~ {{ lower_pop }} * 0.95,
        percentile > 11 & percentile <= 30 ~ {{ lower_pop }} *
          (0.95 - ((0.9 / 19) * (percentile - 11))),
        TRUE ~ 0
      )
    ) |>
    dplyr::group_by({{ higher_id }}) |>
    dplyr::summarise(extent = sum(extent) / sum({{ lower_pop }}))

  if (!weight_high_scores) {
    data <- dplyr::mutate(data, extent = extent * -1)
  }

  return(data)
}

#' Calculate the 'proportion' scores when aggregating up small areas
#'
#' Calculate proportion of small areas in the higher-level geography that are
#' within the 10% most deprived areas in the nation.
#'
#' @param data A dataframe containing a variable to be aggregated and a higher
#'   level geographical grouping variable.
#' @param x Name of the variable in the dataframe for which you want to
#'   calculate proportions. It must have only two possible values.
#' @param higher_id Name of the variable in the data frame containing the higher
#'  level geography names/codes.
#'
#' @examples
#' \dontrun{
#' aggregate_proportion()
#' }
#'
#' @export
aggregate_proportion <- function(data, x, higher_id) {
  x_values <- unique(data[[rlang::ensym(x)]])

  if (length(x_values) != 2) {
    stop("data$var must contain two possible values")
  }

  prop_column_1 <- paste0("proportion_", x_values[1])
  prop_column_2 <- paste0("proportion_", x_values[2])

  data |>
    dplyr::group_by({{ higher_id }}, {{ x }}) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = {{ x }},
      values_from = count,
      values_fill = 0
    ) |>
    dplyr::mutate(!!prop_column_1 := .data[[x_values[1]]] / (.data[[x_values[1]]] + .data[[x_values[2]]])) |>
    dplyr::mutate(!!prop_column_2 := .data[[x_values[2]]] / (.data[[x_values[1]]] + .data[[x_values[2]]])) |>
    dplyr::select({{ higher_id }}, .data[[prop_column_1]], .data[[prop_column_2]])
}

# #' Population-weighted scores
# #'
# #' Calculate population-weighted scores within small areas.
# #'
# #' @param data Data frame containing a variable to be aggregated, lower level
# #'   geography population estimates, and a higher level geographical grouping
# #'   variable.
# #' @param var Name of the variable in the data frame containing the variable to
# #'   be aggregated (e.g. rank) for the lower level geography.
# #' @param higher_level_geography Name of the variable in the data frame
# #'   containing the higher level geography names/codes.
# #' @param population Name of the variable in the data frame containing
# #'   the population estimates of the lower level geography.
# #'
# #' @export
# calculate_pop_weighted_score <-
#   function(data,
#            var,
#            higher_level_geography,
#            population) {
#     data |>
#       dplyr::mutate(score = {{ var }} * {{ population }}) |>
#       dplyr::group_by({{ higher_level_geography }}) |>
#       dplyr::summarise(score = sum(score) / sum({{ population }}))
#   }
