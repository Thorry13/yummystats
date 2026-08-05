test_that("rounding numbers works", {
  V = c(3.3,33.3,13.4)

  expect_equal(round_numbers(V), c(3,33,13))
  expect_equal(round_numbers(V, precision=10), c(0,30,10))
  expect_equal(round_numbers(V, total_consistency=T), c(3,33,14))
  expect_equal(round_numbers(V, precision=5, keep_low_numbers=T), c(3.3,35,15))
  expect_equal(round_numbers(V, precision=5, keep_low_numbers=T, low_limit=3), c(5,35,15))
})


test_that("formatting numbers works",{
  V = c(7,2600,71.98,9.71,98)

  expect_equal(format_numbers(V), c('7', '2600', '72', '10', '98'))
  expect_equal(format_numbers(V, big.mark=',', digits=1), c('7.0', '2,600.0', '72.0', '9.7', '98.0'))
  expect_equal(format_numbers(V, lower_bound=10), c('<10', '2600', '72', '<10', '98'))
})


test_that("formatting percentages works",{
  V = c(0.054,0.334,0.1138,0.175,0.0792,0.09,0.154)

  expect_true(sum(V)==1)
  expect_false(sum(round_numbers(V, 0.01))==1)
  expect_equal(format_perc(V), c('5', '33', '11', '18', '8', '9', '15'))
  expect_equal(format_perc(V, precision=0.1, digits=2, add_sign=TRUE), c('5.40 %', '33.40 %', '11.40 %', '17.50 %', '7.90 %', '9.00 %', '15.40 %'))
  expect_equal(sum(as.numeric(format_perc(V, sumTo100 = TRUE))), 100)
})


test_that("formatting categorical values works",{
  dict_path = system.file("extdata", "country-codes.csv", package = "yummystats")
  df_dict = read.csv2(dict_path)
  countries = c('France', 'Great Britain', 'Morocco', 'United States', NA, 'United States of America (the)', 'Germany',
                'Viet Nam', 'Australia', 'Canada', 'Lao', 'New Zealand', 'Tunisia')

  expect_equal(format_categorical(countries, df_dict, from="Country", to="Alpha.2.code"),
               c('FR', 'Great Britain', 'MA', 'United States', NA, 'US', 'DE', 'VN', 'AU', 'CA', 'Lao', 'NZ', 'TN'))
  expect_equal(format_categorical(countries, df_dict, from="Country", to="Alpha.3.code", unmatched='???', default=''),
               c('FRA', '???', 'MAR', '???', '', 'USA', 'DEU', 'VNM', 'AUS', 'CAN', '???', 'NZL', 'TUN'))
  expect_equal(format_categorical(countries, df_dict,  from="Country", to="Alpha.2.code", .fn = \(x) paste0('c:',x), unmatched='??'),
               c('c:FR', 'c:??', 'c:MA', 'c:??', NA, 'c:US', 'c:DE', 'c:VN', 'c:AU', 'c:CA', 'c:??', 'c:NZ', 'c:TN'))
  expect_equal(format_categorical(countries, df_dict, from="Country", to="Alpha.3.code", default='UNK'),
               c('FRA', 'Great Britain', 'MAR', 'United States', 'UNK', 'USA', 'DEU', 'VNM', 'AUS', 'CAN', 'Lao', 'NZL', 'TUN'))


  country_levels = df_dict$Country
  code_levels = df_dict$Alpha.3.code
  countries_f = factor(countries, country_levels)
  codes_f = format_categorical(countries_f, df_dict, from="Country", to="Alpha.3.code", .fn=\(x) paste0('c:',x))
  expect_equal(as.character(codes_f),
               c('c:FRA', NA, 'c:MAR', NA, NA, 'c:USA', 'c:DEU', 'c:VNM', 'c:AUS', 'c:CAN', NA, 'c:NZL', 'c:TUN'))
  expect_equal(levels(codes_f), paste0('c:',code_levels))
})


test_that("formatting lists works", {
  dict_path = system.file("extdata", "country-codes.csv", package = "yummystats")
  df_dict = read.csv2(dict_path)
  countries = list(
    c('France', 'Great Britain'),
    c('Morocco', 'United States'),
    NULL,
    c('United States of America (the)', 'Germany', 'Viet Nam'),
    c('Australia', 'Canada'),
    'Lao',
    c('New Zealand', 'Tunisia'))

  f = \(x) format_categorical(x, df_dict, from="Country", to="Alpha.2.code")
  expect_equal(as.list(format_list(countries, f)),
               list(
                 c('FR', 'Great Britain'),
                 c('MA', 'United States'),
                 NULL,
                 c('US', 'DE', 'VN'),
                 c('AU', 'CA'),
                 'Lao',
                 c('NZ', 'TN')))
  expect_equal(as.list(format_list(countries, f, .fn=length)),
               list(2, 2, 0, 3, 2, 1, 2))
})
