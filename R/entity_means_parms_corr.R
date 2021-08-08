entity_means_parms_corr <- function(tbl_chains, param_single, entity_param) {
  #' @name entity_means_parms_corr
  #' @title return by-entity means of a multivariate (posterior) distribution
  #' @description by-entity mean aggregation of multivariate (posterior) distribution
  #' @param tbl_chains tibble with chains of all model parameters
  #' @param param_single a string stating the name of the parameter to be extracted
  #' @param entity_param a string stating the name of the entity to be extracted
  #' (e.g., "subjects", "items")
  #' @return the by-entity mean aggregated (posterior) parameters
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom stringr str_detect str_extract str_c
  #' @importFrom dplyr group_by ungroup summarize mutate
  #' @importFrom tidyr pivot_wider
  #'
  #' @export

  tbl_chains %>%
    filter(str_detect(parameter, str_c("^", param_single))) %>%
    group_by(parameter) %>%
    summarize(
      value = mean(value)
    ) %>%
    ungroup() %>%
    mutate(
      entity = entity_param,
      entity_id = str_extract(parameter, "(?<=,)[0-9]+(?=\\])"),
      parameter_name = param_single,
      parameter_id = str_extract(parameter, str_c("(?<=", param_single, "\\[)", "[0-9]+"))
    ) %>%
    pivot_wider(
      id_cols = entity_id,
      names_from = parameter_id,
      names_prefix = param_single,
      values_from = value
    ) %>%
    mutate(
      entity = entity_param
    )

}
