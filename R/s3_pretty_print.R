# 2025 Nov 13 -
#> instead of new classes, think about a 'formatter'
#> - a data class holding a static collection of data (dict where you know all the keys, and they're stable)
#> - as long as you're careful, you have a consistent set of keys
#> - a (nested?) object with attributes that I was passing individually
#> - have a validator for the dict
#>
#> How do you indicate what formatter user needs to use?
#> - Facade pattern
#> - Internal function that takes a formatter
#> - for each formatter, you have a lightweight one-line wrapper
#>
#> Could also have one function per journal
#> - within the function, grab the journal-specific formatter
#> - in python this would be a gloabal dict-of-dicts
#>
#> probably don't need OOP here

format_lancet <- function(df, metric){
  format_journal(style = "lancet")
}

format_journal <- function(style){
  format <- .format_list[[style]]
  #> how to leave breadcrumbs to .format_list?
  #> - have a global value in the script
  #> - have a function that returns this
  #>   - the list/env lives in the function
  #>   - this function would need to be able to add new styles
  #>
  format <- format_list(sytle)
}

format_list_add <- function(my_style){
  #> this can have a validator
  #> this may still need to update some global object
  #> probably want to hide this implementation details
  #> - this name `format_list_add` may not age well
  #> - instead 'add_style' is a more generic name
}

# constructor
new_j_data <- function(dtype = character(), mean = numeric(), lower = NULL, upper = NULL){
  # validate we at least have a mean numeric type
}

#> 4 classes
#> group 1
#> 1 count
#> 2 rates - more similar to counts, needs a denom
#> group 2
#> 3 percent
#> 4 PP

# validator
assert_new_j_data <- function(j_data){
  # assert data_type is a valid class/dtype
  # validate we have at least 1 vector of values
  # if we only have 1 it has to be mean, and we'll copy mean to both upper and lower gracefully
  # or we can have all three
}

# should we have a parent/generic super-class?
# - "just some estimate"


# some way to set options once in an R process
# - package would provide some in-built sets of opts like Lancet and nature

# constructor
new_formatting_needs <- function(
     digits_round_prop         = 1L
    , digits_sigfig_count      = 3L
    , nsmall                   = 1L
    , decimal.mark             = "."
    , negative_sign            = "-"
    , big.mark_count           = ","
    , mean_neg_text            = "a decrease of "
    , UI_only                  = FALSE
    , UI_text                  = ""
    , assert_clu_relationships = TRUE
    , is_lancet                = FALSE
){

}

# and a validator

set_format_opts <- function(formatting_needs){
  # lapply(needs, options)
}


# 2025 Nov 12 - pair programming with Kyle
# mvec <- c(2,2.5,2.1, 4) # by setting parent data.frame class, this validation is handled by default
mvec <- c(2,2.5,2.1)
lvec <- c(1,1.5, 1.1)
uvec <- c(3,3.5,3.1)

new_j_data <- function(dtype = character(), mean = numeric(), lower = NULL, upper = NULL){
  # validate we at least have a mean numeric type
  stopifnot(is.character(dtype))
  stopifnot(is.numeric(mean), is.numeric(lower), is.numeric(upper))
  structure(.Data = data.frame(
    mean = mean
    , lower = lower
    , upper = upper
  )
  , class = c('data.frame','j_data',  dtype)
  )
}

dat <- new_j_data('count', mvec, lvec, uvec)

val_j_data <- function(j_data){
  # stopifnot(inherits(j_data, 'j_data'))
  # stopifnot(any(c("count", "prop", "pp", "rate") %in% class(j_data)))
  UseMethod("val_j_data")
}


val_j_data.countish <- function(j_data){
  val_j_data(j_data)
  stopifnot(any(c("count", "rate") %in% class(j_data)))
  stopifnot(all(j_data$mean >=0))
  stopifnot(all(j_data$lower >=0))
  stopifnot(all(j_data$upper >=0))
}

val_j_data.propish <- function(j_data){
  val_j_data(j_data)
  stopifnot(any(c("count", "rate") %in% class(j_data)))
  stopifnot(all(j_data$mean >=0 & j_data$mean <=1))
  stopifnot(all(j_data$lower >=0 & j_data$lower <=1))
  stopifnot(all(j_data$upper >=0 & j_data$upper <=1))
}

# user-facing constructors
j_count <- function(){

}


# Claude example
# ============================================================================
# S3 Class Constructor and Validator
# ============================================================================

# Constructor function
new_j_data <- function(data, dtype) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(dtype))

  structure(
    data,
    class = c(dtype, "j_data", "data.frame")
  )
}

# Generic validator (base class validation)
val_j_data <- function(j_data) {
  UseMethod("val_j_data")
}

# Default method (base j_data validation)
val_j_data.j_data <- function(j_data) {
  # Check class inheritance
  if (!inherits(j_data, "j_data")) {
    stop("Object must inherit from 'j_data' class", call. = FALSE)
  }

  # Check required columns exist
  required_cols <- c("mean", "lower", "upper")
  missing_cols <- setdiff(required_cols, names(j_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  # Check for NA values
  if (any(is.na(j_data[required_cols]))) {
    stop("NA values found in mean, lower, or upper columns", call. = FALSE)
  }

  # Check that lower <= mean <= upper
  if (!all(j_data$lower <= j_data$mean)) {
    stop("lower must be <= mean", call. = FALSE)
  }
  if (!all(j_data$mean <= j_data$upper)) {
    stop("mean must be <= upper", call. = FALSE)
  }

  invisible(j_data)
}

# Specific validators for subtypes
val_j_data.count <- function(j_data) {
  # First run base class validation
  NextMethod("val_j_data")

  # Count-specific validation
  if (!all(j_data$mean >= 0)) {
    stop("count data: mean must be >= 0", call. = FALSE)
  }
  if (!all(j_data$lower >= 0)) {
    stop("count data: lower must be >= 0", call. = FALSE)
  }
  if (!all(j_data$upper >= 0)) {
    stop("count data: upper must be >= 0", call. = FALSE)
  }

  invisible(j_data)
}

val_j_data.rate <- function(j_data) {
  # First run base class validation
  NextMethod("val_j_data")

  # Rate-specific validation (same as count for non-negative values)
  if (!all(j_data$mean >= 0)) {
    stop("rate data: mean must be >= 0", call. = FALSE)
  }
  if (!all(j_data$lower >= 0)) {
    stop("rate data: lower must be >= 0", call. = FALSE)
  }
  if (!all(j_data$upper >= 0)) {
    stop("rate data: upper must be >= 0", call. = FALSE)
  }

  invisible(j_data)
}

val_j_data.prop <- function(j_data) {
  # First run base class validation
  NextMethod("val_j_data")

  # Proportion-specific validation
  if (!all(j_data$mean >= 0 & j_data$mean <= 1)) {
    stop("prop data: mean must be between 0 and 1", call. = FALSE)
  }
  if (!all(j_data$lower >= 0 & j_data$lower <= 1)) {
    stop("prop data: lower must be between 0 and 1", call. = FALSE)
  }
  if (!all(j_data$upper >= 0 & j_data$upper <= 1)) {
    stop("prop data: upper must be between 0 and 1", call. = FALSE)
  }

  invisible(j_data)
}

val_j_data.pp <- function(j_data) {
  # First run base class validation
  NextMethod("val_j_data")

  # Percentage point-specific validation (same as prop)
  if (!all(j_data$mean >= 0 & j_data$mean <= 1)) {
    stop("pp data: mean must be between 0 and 1", call. = FALSE)
  }
  if (!all(j_data$lower >= 0 & j_data$lower <= 1)) {
    stop("pp data: lower must be between 0 and 1", call. = FALSE)
  }
  if (!all(j_data$upper >= 0 & j_data$upper <= 1)) {
    stop("pp data: upper must be between 0 and 1", call. = FALSE)
  }

  invisible(j_data)
}

# ============================================================================
# Helper function to create validated objects
# ============================================================================

j_data <- function(data, type = c("count", "rate", "prop", "pp")) {
  type <- match.arg(type)
  obj <- new_j_data(data, type)
  val_j_data(obj)
  obj
}

# ============================================================================
# Example Usage
# ============================================================================

# Create sample data
count_data <- data.frame(
  mean = c(10, 20, 30),
  lower = c(8, 18, 28),
  upper = c(12, 22, 32)
)

prop_data <- data.frame(
  mean = c(0.1, 0.5, 0.9),
  lower = c(0.05, 0.45, 0.85),
  upper = c(0.15, 0.55, 0.95)
)

# Create validated objects
my_count <- j_data(count_data, "count")
my_prop <- j_data(prop_data, "prop")

# Validate existing objects
val_j_data(my_count)  # Passes
val_j_data(my_prop)   # Passes
