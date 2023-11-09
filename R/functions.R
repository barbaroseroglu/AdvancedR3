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
    ggplot(data,
           aes(x = value)) +
        geom_histogram() +
        facet_wrap(vars(metabolite), scales = "free")
}
