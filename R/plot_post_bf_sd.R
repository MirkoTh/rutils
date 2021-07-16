plot_post_bf_sd <- function(param, tbl, tbl_thx, bfs) {
  #' @name plot_post_bf_sd
  #' @title plot posterior density and Bayes factor
  #' @description plot posterior density and Bayes factor; adapt coordinate system to posterior
  #' @param param a string stating which parameter of tbl should be plotted
  #' @param tbl a tibble with the posterior samples containing columns chain (-nr), parameter, and value in long format
  #' @param tbl_thx a tibble with the thresholds for plotting into nicely fitting coordinates
  #' @param bfs the Bayes factors for the corresponding posteriors
  #' @return the ggplot object
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr filter
  #' @importFrom ggplot2 ggplot geom_density geom_segment aes geom_label coord_cartesian theme_bw labs
  #'
  #' @export

  tbl <- tbl %>% filter(parameter == param)
  thxs <- tbl_thx %>% filter(parameter == param)
  max_dens <- thxs$value[thxs$variable == "max_dens"]
  zero_dens <- thxs$value[thxs$variable == "zero_dens"]
  pl_width <- abs(
    max(.01, thxs$value[thxs$variable == "thxhi_x"]) -
      min(-.01, thxs$value[thxs$variable == "thxlo_x"])
  )
  bf <- bfs[param]

  pl <- ggplot() +
    geom_density(data = tbl, aes(value)) +
    geom_segment(aes(
      x = 0, xend = 0,
      y = 0, yend = zero_dens
    ), color = "red") +
    geom_label(aes(
      x = min(0, thxs$value[thxs$variable == "thxlo_x"]) +
        (pl_width / 4), y = max_dens / 2,
      label = str_c(
        "BF10 = ", format(round(bf, 3), big.mark = "'"),
        "\nBF01 = ", format(round(1/bf, 3), big.mark = "'")
      )
    )) +
    coord_cartesian(
      xlim = c(
        min(-.01, thxs$value[thxs$variable == "thxlo_x"]),
        max(.01, thxs$value[thxs$variable == "thxhi_x"])
      )
    ) +
    theme_bw() +
    labs(
      title = str_c("Posterior Distribution: ", param),
      x = "Parameter Value",
      y = "Posterior Density"
    )

  pl
}
