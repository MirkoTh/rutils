pivot_chains <- function(m, exclude_warmup = TRUE){
  #' @name pivot_chains
  #' @title pivot stan chains
  #' @description add chain nrs, concatenate chains, and pivot them into long format
  #' @param m a fitted stan model
  #' @param exclude_warmup should warmup samples be excluded? Defaults to TRUE
  #' @return the processed chains as one tbl
  #'
  #' @importFrom magrittr `%>%`
  #' @importFrom tibble as_tibble
  #' @importFrom reshape melt
  #' @importFrom dplyr mutate
  #' @importFrom purrr pmap map_df set_names
  #' @importFrom tidyr pivot_longer
  #'
  #' @export

  add_chain_nr <- function(x, nr) {
    x %>% as_tibble() %>%
      mutate(chain_nr = nr)
  }

  idx_start <- ifelse(exclude_warmup, m@sim$warmup + 1, 1)
  out <- list(m@sim$samples, 1:length(m@sim$samples)) %>%
    pmap(add_chain_nr) %>%
    map(function(x) x[idx_start:m@sim$iter, ]) %>%
    map_df(as_tibble) %>%
    pivot_longer(cols = names(.)[names(.) != "chain_nr"]) %>%
    set_names(c("chain", "parameter", "value"))

  return(out)
}
