#' Dispatch
#'
#' @param gourmex Gourmex ready for dispatch (shaping done).
#'
#' @returns Gourmex ready to serve
#'
#' @importFrom purrr transpose
#' @importFrom rlang .data .env
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' data = tibble::rownames_to_column(iris, 'fid')
#' gourmex = mount(c('Sepal.Length', 'Species'), id_cols='fid', params=default_params())
#' gourmex = ingest(gourmex, data)
#' gourmex = digest(gourmex)
#' gourmex = shape(gourmex)
#' gourmex = dispatch(gourmex)
#'
dispatch = function(gourmex){
  # Extract parameters
  params_layout = gourmex$params$layout
  group_cols = gourmex$params$group_cols
  strata_cols = gourmex$params$strata_cols

  all_shapes = gourmex$storage$shaped

  if(is.null(strata_cols))
    all_shapes = list(all_shapes)
  else{
    all_shapes = lapply(all_shapes, \(X){
        G = X %>% group_by(across(all_of(strata_cols)))
        keys = group_keys(G) %>% unite('.strata', !!strata_cols) %>% pull(.strata) %>% as.character()
        return(G %>% group_split(.keep=FALSE) %>% setNames(keys))}
      ) %>%
      transpose()
  }

  gourmex$storage$layout = lapply(all_shapes, \(S) build_layout(S, params_layout, gourmex$types, group_cols))
  return(gourmex)
}


build_layout = function(S, params_layout, types, group_cols){
  # Init Layout
  df_layout = NULL
  layout_cols = setdiff(names(params_layout$template), 'var_type')

  # W4.3
  for(var in types$var){
    # Extract shaped stats
    df_shapes = S[[var]]

    # Load the right template
    var_type = types %>% filter(.data$var == .env$var) %>% pull(var_type)
    current_template = params_layout$template %>% filter(var_type == .env$var_type)

    # W4.3.1
    if(var_type != 'numerical'){
      df_shapes = df_shapes %>% mutate(level=.data[[var]]) # during ingestion instead ?
      df_shapes = df_shapes %>% filter(!is.na(.data[[var]])) # should be set up before ...
    }

    # W4.3.2
    for(i in 1:nrow(current_template)){
      current_template_i = current_template[i,] %>% select(-var_type)

      # Init layout row
      df_layout_row = df_shapes

      # W4.3.2.1
      # Prepare pivot
      if(!is.null(group_cols)){
        pivot_levels = lapply(group_cols, \(g) levels(df_layout_row[[g]])) %>%
          expand.grid() %>%
          setNames(group_cols) %>%
          arrange(across(all_of(group_cols))) %>%
          unite('pivot_levels', everything(), sep=params_layout$pivot_sep) %>% pull(pivot_levels)
        df_layout_row = df_layout_row %>%
          unite('.pivot', !!group_cols, sep=params_layout$pivot_sep) %>%
          mutate(.pivot = factor(.pivot, levels=pivot_levels)) %>%
          arrange(.pivot) %>%
          mutate(.pivot2 = str_glue(params_layout$pivot_glue))
        pivot_levels = format_categorical(pivot_levels, df_layout_row %>% distinct(.pivot, .pivot2), from='.pivot', to='.pivot2')
        df_layout_row = df_layout_row %>% mutate(.pivot = factor(.pivot2, levels=pivot_levels)) %>% select(-.pivot2)
      }

      # W4.3.2.2
      # Dispatch stats - body
      for(col in layout_cols)
        df_layout_row = df_layout_row %>% mutate(!!col := str_glue(current_template_i[[col]]))

      # W4.3.2.3
      # Dispatch stats - header
      new_layout_cols = layout_cols %>%
        sapply(\(x) unique(str_glue_data(df_layout_row, x)), USE.NAMES = F) %>% unlist()

      # Keep the desired columns only
      df_layout_row = df_layout_row %>%
        select(all_of(layout_cols), any_of(c('{.pivot}', '.pivot'))) %>%
        distinct()

      # W4.3.2.4
      # Pivot if necessary
      if(!is.null(group_cols)){
        # Save positions
        .pivots_location = which(names(current_template_i)=="{.pivot}")

        # Pivot
        df_layout_row = df_layout_row %>%
          pivot_wider(names_from=.pivot, values_from=all_of("{.pivot}"))

        # Relocate values according to saved positions
        df_layout_row = df_layout_row %>%
          relocate(all_of(pivot_levels), .before=all_of(.pivots_location))

        # Rename header names
        names(df_layout_row) = new_layout_cols
      }

      # Add row
      df_layout = bind_rows(df_layout, df_layout_row)
    }
  }
  return(df_layout)
}
