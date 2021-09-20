# validate specified factors
is_valid_categorical <- function( x, fact ) {
  
  is_category <- (is.character(x) | is.factor(x) | is.integer(x))
  msg <- NULL
  if( !is_category ) {
    msg <- paste0( msg, "Factor ", fact,
                   " is not a valid factor (it does not have type character, factor or integer).\n"
                   )
  }
  length_ge_one <- len_unique(x) > 1
  if( !length_ge_one ) {
    msg <- paste0( msg, "Factor ", fact,
                   " only has one unique value - therefore it is invalid for encoding." )
  }
  if( !is_category | !length_ge_one ) {
    stop(msg)
  }
}

all_na <- function( x ) {
  all( is.na(x) )
}

is_valid_noncategorical <- function( x, nonfact ) {
  
  is_number <- is.double(x) | is.integer( x )
  msg <- NULL
  if( !is_number ) {
    msg <- paste0( msg, "Variable ", nonfact,
                   " is not a valid numeric variable (not of type integer or double) - did you forget to include this as a factor?"
    )
    stop(msg)
  }
  all_na <- all_na(x)
  if( all_na ) {
    msg <- paste0( msg, "Variable ", nonfact,
                   " only contains missing values - please drop it manually." )
  }
  length_ge_one <- len_unique(x) > 1
  if( !length_ge_one & !all_na) {
    msg <- paste0( msg, "Variable ", nonfact,
                   " only has one unique value - therefore it is invalid for encoding." )
  }
  if( !is_number | all_na | !length_ge_one ) {
    stop(msg)
  }
}

validate_factors <- function( X, fact) {
  sapply( fact, function(i) is_valid_categorical( X[[i]], i ) )
}

validate_nonfactors <- function( X, nonfact ) {
  sapply( nonfact, function(i) is_valid_noncategorical( X[[i]], i ) )
}

fact_in_table <- function( X, fact ) {
  # if all factor variables are in the table, the set of all names contains all 
  # of the given factor names, with none of the names excluded
  superset_cond <- !(fact %in% colnames(X))
  if(any(superset_cond)) {
    missing_fact <- fact[ superset_cond ]
    stop( "Factor ", missing_fact ," not found in table." )
  }
}

validate_data <- function( X, fact ) {
  fact_in_table(X, fact)
  nonfact <- setdiff( colnames(X), fact ) 
  validate_factors( X, fact )
  validate_nonfactors( X, nonfact )
}

