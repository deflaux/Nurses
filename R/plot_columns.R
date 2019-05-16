# Nurses' Health Study started in 1976.
# NHS1 is on even years, NHS2 is on odd years.
ORDERED_YEARS <- as.character(seq(1976, 2020, by = 1))
ORDERED_2_DIGIT_YEARS <- stringr::str_trunc(ORDERED_YEARS,
                                            width = 2,
                                            side = 'left',
                                            ellipsis = '')
ORDERED_PERIODS <- ORDERED_2_DIGIT_YEARS

# When a numeric-valued column has this many or fewer unique values, we'll
# assume its categorical data.
MAX_NUM_CATEGORIES <- 20

#' Given a set of columns of data in a time series plot the data over time.
#'
#' @param data_ The dataframe containing the columns to plot.
#' @param col_names Character vector of columns to plot.
#' @param col_prefix The prefix all the columns have in common. Defaults to the
#'     first column name with the final two characters removed.
#' @return a ggplot2 object for the columns that were checked
#'
#' @importFrom magrittr %>%
#'
#' @export
#' @md
#'
#' @examples
#' data_to_plot <- data.frame(smk80 = c('never', 'past', 'never'),
#'                            smk82 = c('past', 'never', 'past'),
#'                            smk84 = c('never', 'never', 'past'))
#' plot_columns(data_to_plot, generate_column_names('smk', from = 1980, to = 1984))
#'
#' # You can also capture the plot returned and modify it.
#' numeric_data_to_plot <- data.frame(
#'                            bmi80 = rnorm(200, mean = 30, sd = 3),
#'                            bmi82 = rnorm(200, mean = 31, sd = 2.8),
#'                            bmi84 = rnorm(200, mean = 32, sd = 2.5))
#' p <- plot_columns(numeric_data_to_plot,
#'                   generate_column_names('bmi', from = 1980, to = 1984))
#' # Show default plot
#' p
#' # Modify default plot to zoom in by changing the limits
#' library(ggplot2)
#' p + ylim(27, 35)
#'
#' # Modify the default plot to invert the X and Y axes and use
#' # a different theme.
#' p + coord_flip() + theme_minimal()
#'
plot_columns <- function(data_,
                         col_names,
                         col_prefix = stringr::str_sub(col_names[1], end = -3)) {

  periods_from_col_names <- stringr::str_replace(col_names, col_prefix, '')
  period_col_name <- 'period'

  # See https://tidyeval.tidyverse.org/
  period_col_name_sym <- dplyr::sym(period_col_name)
  col_prefix_sym <- dplyr::sym(col_prefix)

  # Pivot the data from wide to long.
  long_data = data_ %>%
    dplyr::select(col_names) %>%
    tidyr::gather(key = !!period_col_name_sym, value = !!col_prefix_sym, col_names) %>%
    dplyr::mutate(
      # Use the ordering from ORDERED_PERIODS to put a nice order on this factor.
      period = forcats::fct_relevel(stringr::str_replace(.data[[period_col_name]], col_prefix, ""),
                                    intersect(ORDERED_PERIODS, periods_from_col_names))
    )

  plot_column(long_data, stem_col_name = col_prefix, period_col_name = period_col_name)
}

plot_column <- function(data_, stem_col_name, period_col_name = 'period', sort_by_frequency = FALSE) {
  ## TODO assert expected columns and types

  # See https://tidyeval.tidyverse.org/
  stem_col_name_sym <- dplyr::sym(stem_col_name)
  period_col_name_sym <- dplyr::sym(period_col_name)

  num_categories <- length(unique(data_[[stem_col_name]]))

  if (is.character(data_[[stem_col_name]])
      || MAX_NUM_CATEGORIES >= num_categories) {
    # Set a good size for the notebook display of this.
    options(repr.plot.width = 14, repr.plot.height = min(max(6, .5 * num_categories), 20))

    # Create a count of instances per categorical value and time period and then display a heatmap.
    counts <- data_ %>%
      dplyr::group_by(!!period_col_name_sym, !!stem_col_name_sym) %>%
      dplyr::summarize(cnt = dplyr::n())

    if(sort_by_frequency) {
      counts <- counts %>%
        dplyr::arrange(!!period_col_name_sym, dplyr::desc(cnt), !!stem_col_name_sym)
    }

    p <- plot_categorical_counts(counts, stem_col_name)
  } else {
    # Set a good size for the notebook display of this.
    options(repr.plot.width = 14, repr.plot.height = 10)
    p <- plot_numerical_data(data_, stem_col_name)
  }

  return(p)
}

plot_categorical_counts <- function(data_, stem_col_name, period_col_name = 'period') {
  ## TODO assert expected columns and types

  # See https://tidyeval.tidyverse.org/
  stem_col_name_sym <- dplyr::sym(stem_col_name)
  period_col_name_sym <- dplyr::sym(period_col_name)

  # Display a heatmap for the counts of the categorical variable.
  p <- data_ %>%
    ggplot2::ggplot(ggplot2::aes(x = !!period_col_name_sym,
                                 y = forcats::as_factor(!!stem_col_name_sym),
                                 fill = cnt, label = cnt)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(color = 'white', fontface = 2, angle = 30, size = 4) +
    viridis::scale_fill_viridis() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1),
                   legend.position = 'none') +
    ggplot2::ggtitle(stringr::str_c(stem_col_name_sym, ' by time period'))

  return(p)
}

plot_numerical_data <- function(data_, stem_col_name, period_col_name = 'period') {
  ## TODO assert expected columns and types

  # See https://tidyeval.tidyverse.org/
  stem_col_name_sym <- dplyr::sym(stem_col_name)
  period_col_name_sym <- dplyr::sym(period_col_name)

  # Display a box plot of the continuous variable.
  p <- data_ %>%
    ggplot2::ggplot(ggplot2::aes(x = !!period_col_name_sym, y = !!stem_col_name_sym)) +
    ggplot2::geom_boxplot() +
    ggplot2::stat_summary(fun.data = get_boxplot_fun_data, geom = 'text', size = 4, vjust = -0.4) +
    ggplot2::coord_flip() +
    ggplot2::ggtitle(stringr::str_c(stem_col_name_sym, ' by time period'))

  return(p)
}

#' Returns a data frame with a y position and a label, for use annotating ggplot boxplots.
#'
#' @param d A data frame.
#' @return A data frame with column y as max and column label as length.
#'
get_boxplot_fun_data <- function(d) {
  return(data.frame(y = max(d), label = stringr::str_c("N = ", length(d))))
}
