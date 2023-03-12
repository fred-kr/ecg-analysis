box::use(
  magrittr[`%>%`],
  checkmate[assert_count, assert_number, assert_numeric, assert_choice, assert_integerish],
  gsignal[fir1],
  rwavelet[MakeONFilter],
  shiny[is.reactivevalues, icon, tags],
)

# TODO: put functions into relevant files

# Creating smoothing filters

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

# Editing reactive values

#' @export
rv_remove_key <- function(rv, key) {
  if (is.reactivevalues(rv) & is.character(key)) {
    .subset2(rv, "impl")$.values$remove(key)
  } else {
    stop("Error: param 'rv' must be of type 'reactiveValues' and param 'key' must be of type 'character'")
  }
}

#' @export
rv_remove_all_keys <- function(rv) {
  if (is.reactivevalues(rv)) {
    .subset2(rv, "impl")$.values$clear()
  } else {
    stop("Error: param 'rv' must be of type 'reactiveValues'")
  }
}

#' @export
rv_as_list <- function(rv) {
  if (is.reactivevalues(rv)) {
    .subset2(rv, "impl")$.values$values(TRUE)
  } else {
    stop("Error: param 'rv' must be of type 'reactiveValues'")
  }
}

#' @export
rv_get_n_keys <- function(rv) {
  if (is.reactivevalues(rv)) {
    .subset2(rv, "impl")$.values$size()
  } else {
    stop("Error: param 'rv' must be of type 'reactiveValues'")
  }
}

# Function to create a sort of save slot system in form of a `reactiveValues`
# object
#' @export
reactive_storage <- function(max_slots = 3) {
  index <- 1
  function(storage, data) {
    storage[[paste0("slot_", index)]] <- data
    index <<- index %% min(c(max_slots, 5)) + 1
  }
}

# Use material design icon
#' @export
md_icon <- function(icon_name, ...) {
  icon(
    name = NULL,
    class = NULL,
    lib = NULL,
    tags$span(class = c("mdi", paste0("mdi-", icon_name))),
    ...
    )
}
