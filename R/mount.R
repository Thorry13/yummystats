#' Mount the gourmex machine.
#'
#' @param vars
#' @param id_cols
#' @param group_cols
#' @param params
#'
#' @returns
#' @export
#'
#' @examples
mount = function(vars, id_cols, group_cols=NULL, params=NULL){
  gourmex = list()
  class(gourmex) = 'gourmex'

  gourmex$storage = list()

  gourmex$params$vars = vars
  gourmex$params$id_cols = id_cols # data %>% select(any_of({{id_cols}})) %>% names()
  gourmex$params$group_cols = group_cols # data %>% select(any_of({{group_cols}})) %>% names() # to rename ?
  # gourmex$params$strata_var = df %>% select({{strata_var}}) %>% names() # to rename ?
  gourmex$params$stats = params$stats
  gourmex$params$formats = params$formats
  gourmex$params$layout = params$layout

  return(gourmex)
}


# reduce_params = function(params){
#
# }
