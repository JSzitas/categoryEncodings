len_unique <- function( x ) {
  length(unique(x))
}

is_likely_factor <- function( x, cutoff = 0.25 ) {

  # convert to character, and find unique values
  total_levels <- len_unique(as.character(x))
  # compare to total length
  total_length <- length(unlist(x))
  ratio_of_unique <- (total_length - total_levels) / (total_length)
  return( ratio_of_unique > cutoff )
}

is_likely_factor_df <- function(data_frame) {
  sapply( data_frame, is_likely_factor )
}
handle_y <- function( X, Y ) {
  if (!is.null(Y)) {
    if (is.numeric(Y)) {
      Y <- colnames(X)[Y]
    }
    Y_ <- X[, .SD, .SDcols = Y]
    X <- X[, .SD, .SDcols = !Y]
  }
  return(list( X = X, Y = Y_))
}

handle_factors <- function( X, factors ) {
  if(is.null(factors)) {
    factors <- names(which(is_likely_factor_df(X)))
    if (length(factors) == ncol(X)) {
      stop(
        "\n Factor inference impossible: inferred total number of factors: ",
        length(factors),
        ", equal to total number of columns (adjusted for Y) : ",
        ncol(X)
      )
    }
    warning("\n Inferring factors: \n ", paste(factors, "\n"))
  }
  if (is.numeric(factors)) {
    factors <- colnames(X)[factors]
  }
  return(factors)
}

check_colnames <- function( old_colnames, new_colnames ) {
  colnames_match <- old_colnames %in% new_colnames
  
  if( !all( colnames_match) ) {
    missing_columns <- old_colnames[ which( !colnames_match ) ]
    stop( paste0( "New data is missing columns: \n", missing_columns ) )
  }
}

as_dt_tibble <- function( X ) {
  data.table::as.data.table(as.data.frame(X))
}

to_data_table <- function( X ) { 
  class_x <- class(X)[1]
  coercion <- list( "data.table" = identity, 
                    "data.frame" = data.table::as.data.table, 
                    "tbl_df" = as_dt_tibble )[[class_x]]
  do.call( coercion, list(X) )
}
