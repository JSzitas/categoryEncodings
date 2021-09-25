fit_encoder <- function( X, encodings ) {
  original_colnames_X <- colnames(X) 
  
  encoding_names <- names(encodings)
  # get the numeric columns not used for encoding
  non_encoding_columns <- setdiff( colnames(X), encoding_names)
  
  recorded_encodings <- lapply( encoding_names,
                    FUN = function(factor) {
                      do.call(
                        what = encodings[[factor]],
                        args = list(
                          X[,.SD, .SDcols = c(non_encoding_columns, factor) ],
                          fact = factor,
                          keep_factor = TRUE,
                          encoding_only = TRUE
                        )
                      )
                    })
  
  encoder_fit <- function( X, keep = FALSE ) {
    X <- to_data_table(X)
    res <- X[recorded_encodings[[1]], on = encoding_names[1]]
    if (length(encoding_names) > 1) {
      for (i in 2:length(encoding_names)) {
        res <- res[recorded_encodings[[i]], on = encoding_names[i]]
      }
    }
    if (keep == FALSE) {
      res[, (encoding_names) := NULL]
    }
    return( res )
  }
  encoder_reverse <- function( X ) {
    X <- to_data_table(X)
    
    all_encoded_names <- NULL
    encoded_columns <- colnames( recorded_encodings[[1]] )
    # the factor name always comes first, so the join is always on names other 
    # than the first name
    encoded_columns <- encoded_columns[ 2:length(encoded_columns) ]
    res <- recorded_encodings[[1]][X, on = encoded_columns ]
    all_encoded_names <- c( all_encoded_names, encoded_columns)
    
    if (length(encoding_names) > 1) {
      for (i in 2:length(encoding_names)) {
        encoded_columns <- colnames( recorded_encodings[[i]] )
        # the factor name always comes first, so the join is always on names other 
        # than the first name
        encoded_columns <- encoded_columns[ 2:length(encoded_columns) ]
        res <- recorded_encodings[[i]][res, on = encoded_columns ]
        all_encoded_names <- c( all_encoded_names, encoded_columns)
      }
    }
    res[, (all_encoded_names) := NULL]
    
    return( res[, .SD , .SDcols = original_colnames_X] )
  }
  
  
  return( list( encoded = encoder_fit(X),
                fitted_encoder = encoder_fit,
                fitted_deencoder = encoder_reverse))
}
