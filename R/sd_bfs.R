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
  #' @importFrom purrr map map_df
  #' @importFrom tidyr pivot_wider pivot_longer
  #' @importFrom kde1d qkde1d dkde1d
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

  kdes <- tbl_chains %>% map(kde1d)

  par_lims <- kdes %>% map_df(~ qkde1d(c(.0025, .9975), .))
  par_lims$variable <- c("thxlo_x", "thxhi_x")
  max_d <- kdes %>% map_df(~ dkde1d(qkde1d(.5, .), .))
  max_d$variable <- "max_dens"
  zero_d <- kdes %>% map_df(~ dkde1d(0, .))
  zero_d$variable <- "zero_dens"
  par_lims <- rbind(par_lims, max_d, zero_d)

  par_lims <- par_lims %>%
    pivot_longer(
      cols = names(.)[names(.) != "variable"],
      names_to = "parameter"
    )

  bfs <- kdes %>%
    map_dbl(~ (dt(0, 1, 1) * scale) / dkde1d(0, .))

  return(list(bfs, par_lims))
}
