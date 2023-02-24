box::use(
  magrittr[`%>%`],
  checkmate[assert_count, assert_number, assert_numeric, assert_choice, assert_integerish],
  gsignal[fir1],
  rwavelet[MakeONFilter]
)

#' @export
create_FIR_filter <- function(n, w, f_s, type) {
  assert_count(n, positive = TRUE)
  assert_number(f_s, lower = 1)
  assert_numeric(w, upper = f_s, sorted = TRUE, unique = TRUE, min.len = 1, max.len = 2)
  assert_choice(type, c("low", "high", "stop", "pass"))

  if (length(w) == 1) {
    assert_choice(type, c("low", "high"))
  } else if (length(w) == 2) {
    assert_choice(type, c("stop", "pass"))
  }

  fir_filter <- fir1(
    n = n,
    w = w / (f_s / 2),
    type = type
  )

  return(fir_filter)
}

#' @export
create_WT_filter <- function(type, type_ver) {
  assert_choice(type, c("Haar", "Beylkin", "Coiflet", "Daubechies", "Symmlet", "Vaidyanathan", "Battle"))
  if (type == "Coiflet") {
    assert_choice(type_ver, c(1:5))
  } else if (type == "Daubechies") {
    assert_choice(type_ver, c(seq(from = 4, to = 20, by = 2)))
  } else if (type == "Symmlet") {
    assert_choice(type_ver, c(4:10))
  } else if (type == "Battle") {
    assert_choice(type_ver, c(1, 3, 5))
  } else {
    assert_integerish(type_ver, len = 1, lower = 1)
  }

  wt_filter <- MakeONFilter(Type = type, Par = type_ver)

  return(wt_filter)
}

#' @export
min_max_norm <- function(x, ...) {
  return((x - min(x, ...)) / (max(x, ...) - min(x, ...)))
}

#' @export
z_score_norm <- function(x, ...) {
  return((x - mean(x, ...)) / sd(x, ...))
}

#' @export
reactive_storage <- function(max_slots) {
  index <- 1
  function(storage, data) {
    storage[[paste0("slot_", index)]] <- data
    index <<- index %% min(c(max_slots, 5)) + 1
  }
}

#' @export
rm_reactive_value_index <- function(rv, ind) {
  .subset2(rv, "impl")$.values$remove(ind)
}

#' @export
clear_reactive_value <- function(rv) {
  .subset2(rv, "impl")$.values$clear()
}

#' @export
reactive_vals_as_list <- function(rv) {
  .subset2(rv, "impl")$.values$values(TRUE)
}
