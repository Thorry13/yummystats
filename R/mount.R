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
mount = function(vars, id_cols, group_cols=NULL, strata_cols=NULL, params=NULL){
  gourmex = list()
  class(gourmex) = 'gourmex'

  gourmex$storage = list()

  gourmex$params$vars = vars
  gourmex$params$id_cols = id_cols
  gourmex$params$group_cols = group_cols
  gourmex$params$strata_cols = strata_cols

  gourmex$params$stats = params$stats
  gourmex$params$formats = params$formats
  gourmex$params$layout = params$layout

  return(gourmex)
}


# reduce_params = function(params){
#
# }
