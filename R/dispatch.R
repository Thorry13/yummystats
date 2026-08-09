#' Dispatch
#'
#' @param gourmex Gourmex ready for dispatch (shaping done).
#'
#' @returns Gourmex ready to watch
#'
#' @importFrom rlang .data .env
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

  pivot_col = params_layout$pivot_col
  # if(!is.null(strata_cols))
  #   pivot_col = paste0(c(sprintf('{%s}', strata_var), pivot_col),collapse = '|')

  # Init Layout
  df_layout = NULL
  layout_cols = setdiff(names(params_layout$template), 'var_type')

  # W4.3
  # var = 'gender'
  # var = 'symptoms'
  # var = 'age'
  # report = lapply(gourmex$vars$var, function(var){
  for(var in gourmex$types$var){
    # Extract shaped stats
    df_shapes = gourmex$storage$shaped[[var]]

    # df_var = gourmex$df_vars %>% filter(var == .env$var)
    # Load the right template
    var_type = gourmex$types %>% filter(.data$var == .env$var) %>% pull(var_type)
    current_template = params_layout$template %>% filter(var_type == .env$var_type)

    # W4.3.1
    # Display TRUE level only if var_type is logical - should be set up before...
    # if(var_type == 'logical')
    #   df_shapes = df_shapes %>% filter(as.logical(.data[[var]]))
    # Remove NA levels ?
    # if(var %in% names(df_shapes))
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
      if(!is.null(pivot_col)){
        df_layout_row = df_layout_row %>% mutate(pivot_col = str_glue(pivot_col))
        pivot_names = df_layout_row %>% arrange(across(all_of(group_cols))) %>% pull(pivot_col) %>% unique() # replace gp_vars by pivot_col, but factorize and set levels correctly first
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
        select(all_of(layout_cols), any_of(c('{pivot_col}', 'pivot_col'))) %>%
        distinct()

      # W4.3.2.4
      # Pivot if necessary
      if(!is.null(pivot_col)){
        # Save positions
        pivot_cols_location = which(names(current_template_i)=="{pivot_col}")

        # Pivot
        df_layout_row = df_layout_row %>%
          pivot_wider(names_from=pivot_col, values_from=all_of("{pivot_col}"))

        # Relocate values according to saved positions
        df_layout_row = df_layout_row %>%
          relocate(all_of(pivot_names), .before=all_of(pivot_cols_location))

        # Rename header names
        names(df_layout_row) = new_layout_cols

        # Add row
        df_layout = bind_rows(df_layout, df_layout_row)
      }
    }
  }

  gourmex$storage$layout = df_layout
  return(gourmex)
}

