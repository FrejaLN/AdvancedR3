#' Descriptive statistics
#'
#' @param data dataframe
#'
#' @return “A data.frame/tibble.”
#'
descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(
      value,
      base::list(
        mean = mean,
        sd = sd
      )
    )) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::where(is.numeric),
      ~ base::round(.x, digits = 1)
    ))
}


#' Plot for basic distribution of metabolite data.
#'
#' @param data The lipidomics dataset.
#'
#' @return A ggplot2 graph.
#'
plot_distributions <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}


#' Convert a column's character values to snakecase
#'
#' @param data The lipdomics dataset
#' @param columns The column you want to use snakecase for
#'
#' @return Dataframe
#'
column_values_to_snake_case <-
  function(data, columns) {
    data %>% dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
  }



#' Convert the metabolite long format into a wider one
#'
#' @param data The lipidomics dataset
#'
#' @return A wide data frame
#'
metabolites_to_wider <- function(data) {
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
#' @param data The lipidomics dataset
#' @param metabolite_variable The column of the metabolite variable
#'
#' @return Recipe object
#'
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role(
      {{ metabolite_variable }}, age, gender,
      new_role = "predictor"
    ) %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}


#' Create a workflow object of the model
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
#'
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}


#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model The model workflow object that has been fitted
#'
#' @return Dataframe
#'
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}


#' Convert the long form dataset into a list of wide form dataframe
#'
#' @param data The lipidomics dataset
#'
#' @return A list of dataframes
#'
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}


#' Generate the results of a model
#'
#' @param data The lipidomics dataset
#'
#' @return Dataframe
#'
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


#' Function to calculate the model estimates
#'
#' @param data The lipidomics dataset
#'
#' @return A dataframe
#'
calculate_estimates <- function(data) {
  data %>%
    split_by_metabolite() %>%
    purrr::map(generate_model_results) %>% # Add the function and apply to all datasets we just split
    purrr::list_rbind() %>% # to combine datasets on rows
    dplyr::filter(stringr::str_detect(term, "metabolite_")) # Filter dataset to only keep metabolites
}


#' Calculate the estimates for the model for each metabolite
#'
#' @param data The lipidomics dataset
#'
#' @return A dataframe
#'
calculate_estimates <- function(data) {
  model_estimates <- data %>%
    split_by_metabolite() %>%
    purrr::map(generate_model_results) %>%
    purrr::list_rbind() %>%
    dplyr::filter(stringr::str_detect(term, "metabolite_"))
  data %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snake_case(term) %>%
    dplyr::mutate(term = str_c("metabolite_", term)) %>%
    dplyr::distinct(metabolite, term) %>% # to remove duplicate metabolites and terms |>
    dplyr::right_join(model_estimates, by = "term")
}
