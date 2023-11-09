#' Calculate descriptive stats for lipidomics
#'
#' @param data lipidomics dataset
#'
#' @return "A data.frame/tibble"

descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ round(.x, digits = 1)))
}

#' Visualizing the lipidomics data
#'
#' @param lipidomics dataset
#'
#' @return plots of each lipid
#'
plot_distributions <- function(data) {
  data %>% ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}


#' Convert column value strings into snakecase
#'
#' @param Data with string columns
#' @param cols The column to be converted into snakecase
#'
#' @return a dataframe
#'
column_values_to_snakecase <- function(data, cols) {
  data %>%
    dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}


#' Convert the metabolite dataframe into wider format
#'
#' @param the lipidomics dataset
#'
#' @return a wide dataframe

metabolites_to_wider <-  function(data) {
    data %>%
    tidyr::pivot_wider(
        names_from = metabolite,
        values_from = value,
        values_fn = mean,
        names_prefix = "metabolite_"
    )
}

#' A transformation recipe to pre-process the data
#'
#' @param Lipidomics dataset
#' @param the column of the metabolite variable
#'
#' @return a dataframe

create_recipe_spec <- function(data, metabolite_variable) {
    recipes::recipe(data) %>%
        recipes::update_role({{ metabolite_variable}}, age, gender, new_role = "predictor") %>%
        recipes::update_role(class, new_role =  "outcome") %>%
        recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}
