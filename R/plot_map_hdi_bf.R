plot_map_hdi_bf <- function(tbl_thx, bfs, ttl) {
  #' @name plot_map_hdi_bf
  #' @title plot posterior density as line segment with annotated MAP and BF
  #' @description plot posterior density, maximum a posteriori estimate and Bayes factor,
  #' note. does not plot the posterior of an intercept parameter
  #' note. assumes a symmetric posterior distribution
  #' @param tbl_thx a tibble with upper and lower thxs of the HDI
  #' @param bfs the Bayes factors for the corresponding posteriors; default to NULL
  #' @param ttl title of the plot
  #' @return the ggplot object
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr filter mutate left_join
  #' @importFrom ggplot2 ggplot geom_vline geom_point geom_text scale_x_continuous scale_y_discrete geom_segment aes theme_bw labs
  #' @importFrom tidyr pivot_wider
  #' @importFrom stringr str_detect str_trim
  #'
  #' @export

  tbl_segments <- tbl_thx %>%
    filter(!str_detect(parameter, "Intercept")) %>%
    pivot_wider(id_cols = c(parameter), names_from = variable, values_from = value) %>%
    mutate(
      mn = (thxlo_x + thxhi_x) / 2,
      parameter = factor(parameter)
    )

  tbl_bf <- tibble(parameter = names(bfs), bf = bfs) %>% filter(!str_detect(parameter, "Intercept"))
  tbl_bf$bf <- format(round(tbl_bf$bf, 2), big.mark = "'", big.interval = 3L)
  tbl_bf$bf[str_detect(tbl_bf$bf, "Inf")] <- "Decisive"
  tbl_bf <- tbl_bf %>% left_join(tbl_segments, by = "parameter")
  tbl_bf$bf <- str_trim(tbl_bf$bf, "left")

  ggplot(tbl_segments) +
    geom_vline(xintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = 1) +
    geom_segment(
      aes(x = thxlo_x, xend = thxhi_x, y = fct_rev(parameter), yend = fct_rev(parameter)),
      linewidth = 1.25, lineend = "round"
    ) +
    geom_point(aes(mn, parameter), size = 3, color = "skyblue2") +
    geom_text(data = tbl_bf, aes(mn, parameter, label = str_c("BF = ", bf)), vjust = -.75) +
    theme_bw() +
    scale_x_continuous(expand = c(0.03, 0)) +
    scale_y_discrete(expand = c(0.1, 0)) +
    labs(x = "Posterior Value", y = "Parameter", title = ttl) +
    theme(
      strip.background = element_rect(fill = "white"),
      text = element_text(size = 22)
    )
}
