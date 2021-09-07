
default_encoding_style <- function( x, ncol_x, total_factors ) {
  total_levels <- len_unique( unlist(x))
  
  if( total_levels > (ncol_x - total_factors) ) {
    return("mean")
  }
  return("spca")
}

default_encoding_style_df <- function( X, fact ) {
  sapply( X[,.SD , .SDcols =fact], function(i) default_encoding_style( i, ncol(X), length(fact)))
}
determine_encoding_style <- function( X, fact, custom = NULL ) {
  # custom allows you to use your own function to determine the 
  # appropriate method, i.e. it is a mapping of the form
  # X[,column] -> encoding method for column X
  if( !is.null(custom) ) {
    encoding_methods <- custom(X, fact)
  }
  else{
    encoding_methods <- default_encoding_style_df(X, fact)
  }  
  validate_encoding_methods(encoding_methods)
  # finally retrieve appropriate functions
  encoding_methods <- lapply( encoding_methods, get_encoding_method )
  
  return( encoding_methods )
} 

assign_encodings <- function( X, fact, methods = NULL, custom_assignment_method = NULL ) {
  if( !is.null(methods) ) {
    # if only 1 method was suppled, assume it works for all factors, and replicate it
    if( length(methods) == 1 ) {
      methods <- rep(methods, length(fact))
    }
    encoding_methods <- lapply( methods, get_encoding_method )
  }
  # otherwise determine encodings automatically (optionally with a user supplied function)
  else{
    encoding_methods <- determine_encoding_style( X, fact, custom_assignment_method )
  }
  names(encoding_methods) <- fact
  return(encoding_methods)
}
