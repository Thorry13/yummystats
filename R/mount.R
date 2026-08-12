#' Mount the Gourmex machine.
#'
#' @description
#' A Gourmex is a set of tools able to calculate and display stats
#' on a nice custom layout.
#'
#' When mounting a Gourmex, you will first need to provide some columns information:
#' - Which variables to analyse (`vars`) ?
#' - Which ones used to identify observation (`id_cols`) ?
#' - Which ones used to group stats and compare (`group_cols`) ?
#' - Which ones used to separate stats (`strata_cols`) ?
#'
#' Then Gourmex asks for a set of parameters (`params`) to know:
#' - which stats to compute (`params$stats`).
#' - how to format them for human reading (`params$shapes`).
#' - how to combine them in the returned layout (`params$layout`).
#'
#' @param vars The variables to compute stats on.
#' @param id_cols The columns used to identify observations.
#' @param group_cols The columns used to identify groups to be compared.
#' @param strata_cols The columns used to separate distinct analysis.
#' @param params The parameters from the analysis to the display.
#'
#' @returns A mounted Gourmex, ready for ingestion.
#' @export
#'
#' @seealso [ingest()], [digest()]
#'
#' @examples
#' gourmex = mount(c('Sepal.Length', 'Species'), id_cols='fid') # iris data will be ingested later
#'
mount = function(vars, id_cols, group_cols=NULL, strata_cols=NULL, params=NULL){
  gourmex = list()
  class(gourmex) = 'gourmex'

  gourmex$storage = list()

  gourmex$params$vars = vars
  gourmex$params$id_cols = id_cols
  gourmex$params$group_cols = group_cols
  gourmex$params$strata_cols = strata_cols

  gourmex$params$stats = params$stats
  gourmex$params$shapes = params$shapes
  gourmex$params$layout = params$layout

  return(gourmex)
}

