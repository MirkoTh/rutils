estimate_kd <- function(tbl, params_req) {
  #' @name estimate_kd
  #' @title calculate kernel density estimates for univariate (posterior) samples
  #' @description kde estimates for univariate (posterior) samples
  #' @param tbl the (posterior) samples for the model parameters in long format
  #' @param params_req a vector with the names of the parameters of interest
  #' @return the kdes
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr mutate select group_by ungroup
  #' @importFrom purrr map
  #' @importFrom tidyr pivot_wider
  #' @importFrom kde1d kde1d
  #'
  #' @export

  tbl_chains <- tbl %>%
    filter(
      parameter %in% params_req
    ) %>% select(-chain) %>%
    group_by(parameter) %>%
    mutate(rwn = row_number()) %>%
    pivot_wider(names_from = parameter, values_from = value, names_sort = TRUE) %>%
    select(-rwn) %>%
    ungroup()

  kdes <- tbl_chains %>% map(kde1d)

  return(kdes)

}
