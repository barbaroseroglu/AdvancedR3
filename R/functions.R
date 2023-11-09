#' Calculate descriptive stats for lipidomics
#'
#' @param data lipidomics dataset
#'
#' @return "A data.frame/tibble"

descriptive_stats <- function(data) {
    data %>%
        group_by(metabolite) %>%
        summarise(across(value, list(mean = mean, sd = sd))) %>%
        mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
}
