grouped_agg <- function(data, g_vars, d_vars) {
  #' @name grouped_agg
  #' @title summarize d_vars in data in groups g_vars
  #' @description n, nuniqe, mean, and se of grouped tbl
  #' @param data tbl with g_vars and d_vars as columns
  #' @param g_vars grouping variables
  #' @param d_vars variables to be aggregated aka dependent variables
  #' @return the aggregated tbl
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom dplyr across summarise group_by n
  #' @importFrom purrr map_dbl
  #' @importFrom kde1d dkde1d
  #'
  #' @export
  data %>%
    group_by(across({{ g_vars }})) %>%
    summarise(
      n = n(),
      across({{d_vars}}, ~length(unique(.)), .names = "nunique_{.col}"),
      across({{d_vars}}, mean, .names = "mean_{.col}"),
      across({{d_vars}}, ~ sd(.)/sqrt(length(.)), .names = "se_{.col}")
    )
}
