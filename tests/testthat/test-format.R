
tmp <- tempfile()

setup(dir.create(tmp))
teardown(unlink(tmp, recursive = TRUE))

prt_cars <- create_prt(demo_data_frame("mtcars"), dir = tmp, n_chunks = 2L)
prt_iris <- create_prt(demo_data_frame("iris"), dir = tmp, n_chunks = 2L)

prt_all <- create_prt(df_all, dir = tmp)

test_that("print() returns output invisibly", {
  expect_output(ret <- withVisible(print(prt_cars)))
  expect_false(ret$visible)
  expect_identical(ret$value, prt_cars)
})

test_that("format_dt output matches known output", {

  skip_on_non_utf8_locale()

  local_edition(3)

  expect_snapshot({
    print(prt_cars, n = 8L, width = 30L)
    print(prt_iris, n = 5L, width = 30L)
    print(prt_iris, n = -1L, width = 30L)
    print(prt_iris, n = Inf, width = 30L)
    print(prt_iris, n = 3L, width = 5L)
    print(prt_iris, n = NULL, width = 70L)
    print(prt_all, n = NULL, width = 30L)
    print(prt_all, n = NULL, width = 300L)
    print(create_prt(tibble::tibble(a = seq.int(10000)), dir = tmp), n = 5L,
          width = 30L)
    print(format_dt(prt_all, n = 1L, max_extra_cols = 2L, width = 30L))
    print(format_dt(prt_all, n = 1L, max_extra_cols = 0L, width = 30L))
    print(
      format_dt(
        create_prt(tibble::tibble("mean(x)" = 5, "var(x)" = 3), dir = tmp),
        width = 28
      )
    )
  })
})

test_that("trunc_mat for POSIXct columns", {

  skip_on_os("windows")
  skip_on_non_utf8_locale()

  df <- tibble::tibble(x = as.POSIXct("2016-01-01 12:34:56 GMT") + 1:12)

  expect_output_file_opts(
    print(create_prt(df, dir = tmp), n = 8L, width = 60L),
    "format/POSIXct-8-60.txt"
  )
})

test_that("trunc_mat for wide-character columns", {

  skip_on_os("windows")
  skip_on_non_utf8_locale()

  x <- c("\u6210\u4ea4\u65e5\u671f", "\u5408\u540c\u5f55\u5165\u65e5\u671f")
  df <- setNames(tibble::tibble(1:3, 4:6), x)

  expect_output_file_opts(
    print(create_prt(df, dir = tmp), n = 8L, width = 60L),
    "format/wide-8-60.txt"
  )
})
