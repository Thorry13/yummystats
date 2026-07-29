#' Initialize
#'
#' @param data Data to digest.
#' @param vars Columns to analyse.
#' @param id A set of columns that uniquely identify each observation.
#' @param group A set of columns used to group observations
#' @param params A set of parameters for the analysis
#'
#' @import dplyr
#' @export
#'
ingest = function(data, vars, id, group, params=NULL){
  yum = list()
  class(yum) = 'yum'

  yum$data = data

  yum$vars = tibble(var=vars) %>%
    rowwise() %>%
    mutate(
      var_type = case_when(
        is.logical(data[[var]]) ~ 'logical',
        is.numeric(data[[var]]) ~ 'numerical',
        TRUE ~ 'categorical')
    ) %>%
    ungroup()

  yum$params$id = data %>% select(any_of({{id}})) %>% names()
  yum$params$group = data %>% select(any_of({{group}})) %>% names() # to rename ?
  # yum$params$strata_var = df %>% select({{strata_var}}) %>% names() # to rename ?
  yum$params$stats = params$stats
  yum$params$formats = params$formats
  yum$params$layout = params$layout

  return(yum)
}
