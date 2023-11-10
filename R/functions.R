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
column_values_to_snake_case <- function(data, cols) {
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

#' Create a workflow object of the model and the transformations
#'
#' @param model_specs the model specifications
#' @param recipe_specs the recipe specifications
#'
#' @return a workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
    workflows::workflow() %>%
        workflows::add_model(model_specs) %>%
        workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model the model workflow object that has been fitted
#'
#' @return a dataframe
#'
tidy_model_output <- function(workflow_fitted_model) {
    workflow_fitted_model %>%
        workflows::extract_fit_parsnip() %>%
        broom::tidy(exponentiate = TRUE)
}

#' Convert the long form dataset into a list of wide form dataframes
#'
#' @param data lipidomics
#'
#' @return a list of data frames
#'
split_by_metabolite <- function(data) {
    data %>%
        column_values_to_snake_case(metabolite) %>%
        dplyr::group_split(metabolite) %>%
        purrr::map(metabolites_to_wider)
}

#' Generate the results of the model
#'
#' @param data The lipidomics dataset
#'
#' @return A data frame
generate_model_results <- function(data) {
    create_model_workflow(
        parsnip::logistic_reg() %>%
            parsnip::set_engine("glm"),
        data %>%
            create_recipe_spec(tidyselect::starts_with("metabolite_"))
    ) %>%
        parsnip::fit(data) %>%
        tidy_model_output()
}


#' Presenting model results for each metabolite in one table
#'
#' @param model_results
#' @param data the lipidomics dataset
#'
#' @return a data frame
add_original_metabolite_names <- function(model_results, data) {
    data %>%
        dplyr::select(metabolite) %>%
        dplyr::mutate(term = metabolite) %>%
        column_values_to_snake_case() %>%
        dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
        dplyr::distinct(term, metabolite) %>%
        dplyr::right_join(model_results, by = "term" )
}
