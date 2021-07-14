sd_bfs <- function(tbl, params_bf, scale) {
  #' @name sd_bfs
  #' @title Bayes factors using Savage Dickey density ratio
  #' @description calculate BFs for a given set of parameters
  #' @param tbl the posterior samples for the model parameters in long format
  #' @param params_bf a vector with the names of the parmaeters to calculate the BFs
  #' @param scale the scaling of the Student's t distribution
  #' @return the BFs for the required parameters
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom tibble as_tibble
  #' @importFrom reshape melt
  #' @importFrom dplyr mutate select group_by ungroup
  #' @importFrom purrr map
  #' @importFrom tidyr pivot_wider
  #'
  #' @export
  tbl_chains <- tbl %>%
    filter(
      parameter %in% params_bf
    ) %>% select(-chain) %>%
    group_by(parameter) %>%
    mutate(rwn = row_number()) %>%
    pivot_wider(names_from = parameter, values_from = value) %>%
    select(-rwn) %>%
    ungroup()

  kdes <- tbl_chains %>%
    map(kde1d) %>%
    map_dbl(~ (dt(0, 1, 1) * scale) / dkde1d(0, .))

  return(kdes)
}
