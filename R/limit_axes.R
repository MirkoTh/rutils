limit_axes <- function(kdes) {
  #' @name limit_axes
  #' @title calculate enveloping coordinates for uni-variate (posterior) samples
  #' @description calculate space around a posterior distribution to be plotted nicely
  #' @param kdes kde objects
  #' @return the four required x and y coordinates
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom purrr map_df
  #' @importFrom tidyr pivot_longer
  #' @importFrom kde1d qkde1d dkde1d
  #'
  #' @export

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

  return(par_lims)
}
