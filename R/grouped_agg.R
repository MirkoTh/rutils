grouped_agg <- function(data, g_vars, i_vars) {
  #' @name grouped_agg
  #' @title summarize i_vars in data in groups g_vars
  #' @description n, nuniqe, mean, and se of grouped tbl
  #' @param data tbl with g_vars and i_vars as columns
  #' @param g_vars grouping variables
  #' @param i_vars variables to be aggregated aka independent variables
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
      across({{i_vars}}, ~length(unique(.)), .names = "nunique_{.col}"),
      across({{i_vars}}, mean, .names = "mean_{.col}"),
      across({{i_vars}}, ~ sd(.)/sqrt(length(.)), .names = "se_{.col}")
    )
}
