#### Definition of a standard dataframe for test ####
N = 1000

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
# TO REMOVE
group_expr = expr(gourmex$params$group_cols)
strata_expr = expr(gourmex$params$strata_cols)

stat_funcs_cat = list(
  N = expr(\(...) stat_total(..., by=eval(strata_expr))),
  n_avail = expr(\(...) stat_n_avail(..., by=eval(strata_expr))),
  n_level = expr(\(...) stat_total(..., by= c(var, eval(strata_expr)))),
  n_missing = expr(\(...) stat_n_miss(..., by=eval(strata_expr))),
  p_level = expr(\(...) stat_perc(..., numerator_col = 'n_level', denominator_col = 'n_avail', by=eval(strata_expr))))

# for comparative analysis
stat_funcs_cat_gp = list(
  N_group = expr(\(...) stat_total(..., by=c(eval(group_expr), eval(strata_expr)))),
  n_avail_group = expr(\(...) stat_n_avail(..., by=c(eval(group_expr), eval(strata_expr)))),
  n_level_group = expr(\(...) stat_total(..., by=c(var, eval(group_expr), eval(strata_expr)))),
  p_level_group = expr(\(...) stat_perc(..., numerator_col = 'n_level_group', denominator_col = 'n_avail_group', by=eval(strata_expr))),
  p_value = expr(\(...) stat_pvalue(..., by=eval(group_expr), strata=eval(strata_expr)))
  )

stat_funcs_logic = stat_funcs_cat
stat_funcs_logic_gp = stat_funcs_cat_gp

stat_funcs_num = list(
  num_stats = expr(\(...) stat_numeric(..., by=eval(strata_expr))),
  N = expr(\(...) stat_total(..., by=eval(strata_expr))),
  n_avail = expr(\(...) stat_n_avail(..., by=eval(strata_expr))),
  n_missing = expr(\(...) stat_n_miss(..., by=eval(strata_expr))))
stat_funcs_num_gp = list(
  num_stats_group = expr(\(...) stat_numeric(..., by=c(eval(group_expr), eval(strata_expr)), suffix = '_group')),
  N_group = expr(\(...) stat_total(..., by=c(eval(group_expr), eval(strata_expr)))),
  n_avail_group = expr(\(...) stat_n_avail(..., by=c(eval(group_expr), eval(strata_expr)))),
  p_value = expr(\(...) stat_pvalue(..., by=eval(group_expr), strata=eval(strata_expr)))
)

A = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat), stat_func = unname(stat_funcs_cat))
B = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic), stat_func = unname(stat_funcs_logic))
C = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num), stat_func = unname(stat_funcs_num))
A_gp = tibble(var_type = 'categorical', stat_name = names(stat_funcs_cat_gp), stat_func = unname(stat_funcs_cat_gp))
B_gp = tibble(var_type = 'logical', stat_name = names(stat_funcs_logic_gp), stat_func = unname(stat_funcs_logic_gp))
C_gp = tibble(var_type = 'numerical', stat_name = names(stat_funcs_num_gp), stat_func = unname(stat_funcs_num_gp))

stat_funcs = bind_rows(A,B,C)
stat_funcs_gp = bind_rows(A_gp,B_gp,C_gp)
myparams0 = list('stats'=stat_funcs, 'formats'=NULL, 'layout'=NULL)
myparams = list('stats'=bind_rows(stat_funcs, stat_funcs_gp), 'formats'=NULL, 'layout'=NULL)



