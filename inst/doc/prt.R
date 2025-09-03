## ----include = FALSE----------------------------------------------------------

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(prt)

## ----setup--------------------------------------------------------------------
tmp <- tempfile()
dir.create((tmp))

dat <- data.table::setDT(nycflights13::flights)
print(dat)

## ----naive-split--------------------------------------------------------------
flights <- as_prt(dat, n_chunks = 2L, dir = tempfile(tmpdir = tmp))
print(flights)

## ----user-split---------------------------------------------------------------
dat <- data.table::setorderv(dat, "carrier")
grp <- cumsum(table(dat$carrier)) / nrow(dat) < 0.5
dat <- split(dat, grp[dat$carrier])

by_carrier <- as_prt(dat, dir = tempfile(tmpdir = tmp))
by_carrier

## ----subset-col---------------------------------------------------------------
str(flights[[1L]])
identical(flights[["year"]], flights$year)
identical(flights[["year"]], flights[, "year", drop = TRUE])
str(flights$yea)

## ----subset-row---------------------------------------------------------------
datasets::mtcars[, "mpg"]
flights[, "dep_time"]

jan_dt <- flights[flights$month == 1L, ]

jan_dt[1L]
flights[1L]

## ----subset-nse---------------------------------------------------------------
identical(jan_dt, subset(flights, month == 1L))

## ----row-order----------------------------------------------------------------
bench::mark(
  subset(flights, carrier == "AA"),
  subset(by_carrier, carrier == "AA")
)

## ----nse-issue0---------------------------------------------------------------
month <- 1L
subset(flights, month == month, 1L:7L)

identical(jan_dt, subset(flights, month == !!month))
identical(jan_dt, subset(flights, .env$month == .data$month))

## ----nse-issue1---------------------------------------------------------------
subset(flights, select = year:day)

## ----nse-issue2---------------------------------------------------------------
sched_dep_time <- "dep_time"
colnames(subset(flights, select = sched_dep_time))

actual_dep_time <- "dep_time"
colnames(subset(flights, select = actual_dep_time))

colnames(subset(flights, select = .env$sched_dep_time))
colnames(subset(flights, select = .env$actual_dep_time))

## ----nse-issue3, error = TRUE, eval = getRversion() > "3.5.0"-----------------
try({
colnames(subset(flights, select = .data$sched_dep_time))
colnames(subset(flights, select = .data$actual_dep_time))
})

## ----part_safe----------------------------------------------------------------
is_true <- function(x) !is.na(x) & x
expr <- quote(is_true(arr_delay > mean(arr_delay, na.rm = TRUE)))
nrow(subset_quo(flights, expr, part_safe = FALSE))
nrow(subset_quo(flights, expr, part_safe = TRUE))

## ----forward------------------------------------------------------------------
col_safe_subset <- function(x, expr, cols) {
  stopifnot(is_prt(x), is.character(cols))
  subset(x, {{ expr }}, .env$cols)
}

air_time <- c("dep_time", "arr_time")
col_safe_subset(flights, month == 1L, air_time)

## ----tbl----------------------------------------------------------------------
new_tbl <- function(...) structure(list(...), class = "my_tbl")

dim.my_tbl <- function(x) {
  rows <- unique(lengths(x))
  stopifnot(length(rows) == 1L)
  c(rows, length(x))
}

head.my_tbl <- function(x, n = 6L, ...) {
  as.data.frame(lapply(x, `[`, seq_len(n)))
}

tail.my_tbl <- function(x, n = 6L, ...) {
  as.data.frame(lapply(x, `[`, seq(nrow(x) - n + 1L, nrow(x))))
}

print.my_tbl <- function(x, ..., n = NULL, width = NULL,
                         max_extra_cols = NULL) {

  out <- format_dt(x, n = n, width = width, max_extra_cols = max_extra_cols)
  out <- paste0(out, "\n")

  cat(out, sep = "")

  invisible(x)
}

## ----register-s3, include = FALSE---------------------------------------------
if (base::getRversion() < "4.0.0") {

  .S3method <- function(generic, class, method) {

    if(missing(method)) {
      method <- paste(generic, class, sep = ".")
    }

    method <- match.fun(method)

    registerS3method(generic, class, method, envir = parent.frame())

    invisible(NULL)
  }
}

Map(.S3method, c("dim", "head", "tail", "print"), "my_tbl",
    list(dim.my_tbl, head.my_tbl, tail.my_tbl, print.my_tbl))

## ----print--------------------------------------------------------------------
new_tbl(a = letters, b = 1:26)

## ----teardown-----------------------------------------------------------------
unlink(tmp, recursive = TRUE)

