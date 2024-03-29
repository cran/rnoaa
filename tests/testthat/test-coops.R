context("coops")

test_that("coops works", {
  vcr::use_cassette("coops_search", {
    aa <- coops_search(station_name = 8723970, begin_date = 20140927, 
      end_date = 20140928, product = "water_temperature")
  })

  # class
  expect_is(aa$data, "data.frame")
  expect_is(aa$metadata$name, "character")

  # dimensions
  expect_equal(NCOL(aa$data), 3)

  skip_on_cran()
  # works with product="predictions" that used to fail
  z <- coops_search(station_name = 8551762, 
    begin_date = 20200501, end_date = 20200531, 
    datum = "mtl", product = "predictions", time_zone = "lst_ldt")
  expect_is(z, "list")
  expect_named(z, "predictions")
  expect_is(z$predictions, "data.frame")
  expect_named(z$predictions, c('t', 'v'))
})

test_that("coops fails well", {
  skip_on_cran()
  skip_on_ci()

  expect_error(coops_search(), "argument \"product\" is missing, with no default")

  vcr::use_cassette("coops_search_fail_no_data", {  
    expect_error(
      coops_search(station_name = 8775244, begin_date = 20140927, 
        end_date = 20140928, product = "air_temperature"), 
      "No data was found"
    )
  })

  expect_error(
    coops_search(station_name = 8775244, begin_date = 20140927, 
      end_date = 20140928, product = "monthly_mean"), 
    "Must specify a datum for water level products"
  )

  expect_error(
    coops_search(station_name = 8724580, begin_date = 20040927,
                 end_date = 20140928, product = "high_low", datum = "stnd"),
    "The maximum duration the NOAA API"
  )

  expect_error(
    coops_search(station_name = "ps0401", begin_date = 20151121,
                 end_date = 20151231, product = "currents"),
    "The maximum duration the NOAA API"
  )
})
