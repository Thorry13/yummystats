#### Definition of a standard dataframe for test ####
set.seed(1)
N = 10000

# numerical values
ages = runif(N, 0, 100)

# # insert missing values
ages[1:(0.03*N)] = NA
ages = sample(ages, N)

# factor
genders = factor(c(rep('Female', 0.45*N), rep('Male', 0.35*N), rep(NA, 0.2*N))) %>% sample(N)

# categorical values
some_occupations = c('Doctor', 'Teacher', 'Manager', 'Firefighter', 'Police officer', NA)
occupations = sample(some_occupations, N, replace=TRUE) %>% sample(N)

# for stratas
some_locations = c('Europe', 'US/Canada', 'South America', 'Africa', 'Asia', 'Oceania', NA)
locations = sample(some_locations, N, replace=TRUE) %>% sample(N)

# boolean values
happy = c(rep(T, 0.78*N), rep(F, 0.18*N), rep(NA, 0.4*N)) %>% sample(N)

# lists
symptoms_list = c('s1', 's2', 's3')
n_s = floor(runif(N, 0, 4))
symptoms = sapply(n_s, \(n) sample(symptoms_list, n)) %>% sample(N)

groups = c(rep('A', 0.5*N), rep('B', 0.3*N), rep('AB+', 0.2*N)) %>% sample(N)

# Build data frame
df_test = tibble(
    group=groups,
    age=ages,
    gender=genders,
    happy=happy,
    occupation=occupations,
    symptoms=symptoms,
    location=locations) %>%
  mutate(id = row_number())

# ---
# STATS

stat_funcs_cat = list(
  N = expr(\(...) stat_total(..., by=gourmex$params$strata_cols)),
  n_avail = expr(\(...) stat_n_avail(..., by=gourmex$params$strata_cols)),
  n_level = expr(\(...) stat_total(..., by= c(var, gourmex$params$strata_cols))),
  n_missing = expr(\(...) stat_n_miss(..., by=gourmex$params$strata_cols)),
  p_level = expr(\(...) stat_perc(..., numerator_col = 'n_level', denominator_col = 'n_avail', by=gourmex$params$strata_cols)))

# for comparative analysis
stat_funcs_cat_gp = list(
  N_group = expr(\(...) stat_total(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
  n_avail_group = expr(\(...) stat_n_avail(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
  n_level_group = expr(\(...) stat_total(..., by=c(var, gourmex$params$group_cols, gourmex$params$strata_cols))),
  p_level_group = expr(\(...) stat_perc(..., numerator_col = 'n_level_group', denominator_col = 'n_avail_group', by=gourmex$params$strata_cols)),
  p_value = expr(\(...) stat_pvalue(..., by=gourmex$params$group_cols, strata=gourmex$params$strata_cols))
  )

stat_funcs_logic = stat_funcs_cat
stat_funcs_logic_gp = stat_funcs_cat_gp

stat_funcs_num = list(
  num_stats = expr(\(...) stat_numeric(..., by=gourmex$params$strata_cols)),
  N = expr(\(...) stat_total(..., by=gourmex$params$strata_cols)),
  n_avail = expr(\(...) stat_n_avail(..., by=gourmex$params$strata_cols)),
  n_missing = expr(\(...) stat_n_miss(..., by=gourmex$params$strata_cols)))
stat_funcs_num_gp = list(
  num_stats_group = expr(\(...) stat_numeric(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols), suffix = '_group')),
  N_group = expr(\(...) stat_total(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
  n_avail_group = expr(\(...) stat_n_avail(..., by=c(gourmex$params$group_cols, gourmex$params$strata_cols))),
  p_value = expr(\(...) stat_pvalue(..., by=gourmex$params$group_cols, strata=gourmex$params$strata_cols))
)

A = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat), stat_func = unname(stat_funcs_cat))
B = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic), stat_func = unname(stat_funcs_logic))
C = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num), stat_func = unname(stat_funcs_num))
A_gp = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat_gp), stat_func = unname(stat_funcs_cat_gp))
B_gp = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic_gp), stat_func = unname(stat_funcs_logic_gp))
C_gp = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num_gp), stat_func = unname(stat_funcs_num_gp))

stat_funcs = bind_rows(A,B,C)
stat_funcs_gp = bind_rows(A_gp,B_gp,C_gp)
myparams0 = list('stats'=stat_funcs, 'shapes'=NULL, 'layout'=NULL)
myparams = list('stats'=bind_rows(stat_funcs, stat_funcs_gp), 'shapes'=NULL, 'layout'=NULL)


#---
# SHAPES

shapes_cat = list(
  N = format_numbers,
  n_avail = format_numbers,
  n_level = \(x) round_numbers(x, precision=1) %>% format_numbers(),
  n_missing = format_numbers,
  p_level = format_perc
)
shapes_logic = shapes_cat
shapes_num = list(
  N = format_numbers,
  n_avail = format_numbers,
  mean = \(x) style_number(x, 1),
  CI95 = \(x) style_number(x, 1),
  median = \(x) style_number(x, 1),
  min = \(x) style_number(x, 1),
  max = \(x) style_number(x, 1),
  q1 = \(x) style_number(x, 1),
  q3 = \(x) style_number(x, 1)
)

shapes_cat_gp = list(
  N_group = format_numbers,
  n_avail_group = format_numbers,
  n_level_group = \(x) round_numbers(x, precision=1) %>% format_numbers(),
  p_level_group = format_numbers,
  p_value = style_pvalue
)
shapes_logic_gp = shapes_cat_gp
shapes_num_gp = list(
  N_group = format_numbers,
  n_avail_group = format_numbers,
  mean_group = \(x) style_number(x, 1),
  CI95_group = \(x) style_number(x, 1),
  median_group = \(x) style_number(x, 1),
  min_group = \(x) style_number(x, 1),
  max_group = \(x) style_number(x, 1),
  q1_group = \(x) style_number(x, 1),
  q3_group = \(x) style_number(x, 1)
)

A = tibble(var_type = 'categorical', stat_name = names(shapes_cat), shape_func = unname(shapes_cat))
B = tibble(var_type = 'logical', stat_name = names(shapes_logic), shape_func = unname(shapes_logic))
C = tibble(var_type = 'numerical', stat_name = names(shapes_num), shape_func = unname(shapes_num))
A_gp = tibble(var_type = 'categorical', stat_name = names(shapes_cat_gp), shape_func = unname(shapes_cat_gp))
B_gp = tibble(var_type = 'logical', stat_name = names(shapes_logic_gp), shape_func = unname(shapes_logic_gp))
C_gp = tibble(var_type = 'numerical', stat_name = names(shapes_num_gp), shape_func = unname(shapes_num_gp))

shapes = bind_rows(A,B,C)
shapes_gp = bind_rows(A_gp,B_gp,C_gp)
myparams0$shapes = shapes
myparams$shapes = bind_rows(shapes, shapes_gp)
