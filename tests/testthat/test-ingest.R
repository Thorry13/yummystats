check_base = function(X){
  expect_equal(class(X), 'yum')
  expect_equal(names(X), c('data', 'vars', 'params'))
  expect_identical(X$data, df_test)
  expect_equal(X$params$id, 'id')
}

test_that("simple stats", {
  R_cat = ingest(df_test, "cat", 'id')
  R_cont = ingest(df_test, 'val', 'id')
  R_logic = ingest(df_test, 'sick', 'id')
  R_signs = ingest(df_test, 'signs', 'id', 'group')

  check_base(R_cat)
  check_base(R_cont)
  check_base(R_logic)
  check_base(R_signs)
})

test_that("grouped stats", {
  gp_cols = 'group'
  R_cat = ingest(df_test, "cat", 'id', gp_cols)
  check_base(R_cat)
  expect_equal(R_cat$params$group, gp_cols)
  expect_identical(R_cat$vars, tibble(var='cat', var_type='categorical'))
  # expect_false(is.null(R_cat$params$stats))
  # expect_false(is.null(R_cat$params$formats))
  # expect_false(is.null(R_cat$params$layout))

  R_cont = ingest(df_test, 'val', 'id', gp_cols)
  check_base(R_cont)
  expect_equal(R_cont$params$group, gp_cols)
  expect_identical(R_cont$vars, tibble(var='val', var_type='numerical'))

  R_logic = ingest(df_test, 'sick', 'id', gp_cols)
  check_base(R_logic)
  expect_equal(R_logic$params$group, gp_cols)
  expect_identical(R_logic$vars, tibble(var='sick', var_type='logical'))

  # R_logic_as_cat = report_df(df_test, 'sick', 'id', 'group', stat_type='categorical')

  R_signs = ingest(df_test, 'signs', 'id', 'group')
  check_base(R_signs)
  expect_equal(R_signs$params$group, gp_cols)
  expect_identical(R_signs$vars, tibble(var='signs', var_type='categorical'))

})
