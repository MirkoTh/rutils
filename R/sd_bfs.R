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
  #' @importFrom purrr map_dbl
  #' @importFrom kde1d dkde1d
  #'
  #' @export

  kdes <- estimate_kd(tbl, params_bf)
  par_lims <- limit_axes(kdes)

  bfs <- kdes %>%
    map_dbl(~ (dt(0, 1, 1) * scale) / dkde1d(0, .))

  return(list(bfs, par_lims))

}
