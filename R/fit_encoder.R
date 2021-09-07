fit_encoder <- function( X, encodings ) {
  original_colnames_X <- colnames(X) 
  
  encoding_names <- names(encodings)
  # get the numeric columns not used for encoding
  non_encoding_columns <- setdiff( colnames(X), encoding_names)
  
  result <- lapply( encoding_names,
                    FUN = function(factor) {
                      selection <- c(non_encoding_columns, factor)
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
    res <- X[result[[1]], on = encoding_names[1]]
    if (length(encoding_names) > 1) {
      for (i in 2:length(encoding_names)) {
        res <- res[result[[i]], on = encoding_names[i]]
      }
    }
    if (keep == FALSE) {
      res[, (encoding_names) := NULL]
    }
    return( res )
  }
  return( list( encoded = encoder_fit(X),
                fitted_encoder = encoder_fit))
}
