#' Encode a given factor variable automatically
#'
#' @description Transforms the original design matrix automatically, using the appropriate encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact Optional: The factor variable(s) to encode by -
#'             either positive integer(s) specifying the 
#'             column number, or the name(s) of the column.
#'             If left empty a heuristic is used to determine the factor 
#'             variable(s), and a warning is written with the names of 
#'             the variables converted.
#' @param method Optional: A character string indicating which encoding method to use,
#'               either of the following: 
#'               * "mean"
#'               * "median"
#'               * "deviation"
#'               * "lowrank"
#'               * "SPCA"
#'               * "mnl"
#'               * "dummy"
#' If only a single method is specified, it is taken to encode either all of the variables 
#' supplied through *fact*, or variables which have been flagged as factors automatically. 
#' If multiple methods are specified, the number of methods must match the number of 
#' factor variables in *fact* - and these are applied to correspond in the order in 
#' which they were supplied. In case a missmatch occurs, an error is raised.                  
#' If left empty, the appriopriate method is selected on a case by case basis 
#' (and the selected methods are written out to console).                            
#' @param keep Whether to keep the original factor column(s), defaults to **FALSE**.
#' @return A new data.table X which contains the new columns and optionally the old factor(s).
#' @details Automatically selects the appropriate method given the number of anticipated
#'          newly created variables, based on the results in Johannemann et al.(2019) 
#'          'Sufficient Representations for Categorical Variables', and 
#'          a simple heuristic - where 
#'          
#' @import data.table
#' @importFrom data.table .SD
#' @export
#' 
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#'  encode_categories( design_mat, method = "mean" )
#' 


encode_categories <- function( X,
                         fact = NULL,
                         method = NULL,
                         keep = FALSE )
{
  X <- data.table::data.table(X)
  
  is_likely_factor <- function(datafm)
    {
    res <- lapply(datafm, FUN = function(i){
      total_levels <- levels(as.factor(as.character(i)))
      total_length <- length(unlist(i))
      ratio_of_unique_values <- (total_length - length(total_levels))/(total_length) 
        
      if(ratio_of_unique_values > 0.2){
          return(TRUE)
        }
        else{
          return(FALSE)
        }
      })
      return(unlist(res))
    }
  if(is.null(fact)){
    all_factor <- names(which(is_likely_factor(X) == TRUE))
  }
  else{
    if(is.numeric(fact)){
      fact <- colnames(X)[fact]
    }
    all_factor <- fact
  }

  method_table <- list( encode_mean, encode_median, encode_deviation,
                        encode_lowrank, encode_SPCA, encode_mnl,
                        encode_dummy )

  method_names <- c( "mean", "median", "deviation",
                     "lowrank", "SPCA", "mnl",
                     "dummy")
  if(!is.null(method))
  {
    methods_used <- lapply( method, FUN = match, table = method_names)
          if(sum(is.na(unlist(methods_used))) == TRUE ){
            stop("Failed to match the supplied method(s).")
          }
  }
  else
  {
   methods_used <- lapply(1:length(all_factor), FUN = function(i){
     total_levels <- length(levels(unlist(X[,(all_factor[i])])))
     # more subgroups than there are other columns
      # if( total_levels > ncol(X[,!..all_factor]) ){
         if( total_levels > (ncol(X) - ncol(X[, .SD, .SDcols = all_factor])) ){
        # default to mean encoding
         return(1)
       }
     else{
       return(5)  }})
  }


if(length(methods_used) < length(all_factor)){
  stop(paste("The number of supplied methods(", length(methods_used),
  ") is not equal to the number of factors(", length(all_factor),").", 
        "Please specify the correct number of methods, and/or factor variables.",
             sep = ""))
}  
if(length(methods_used) > length(all_factor)){
  warning(paste("More methods(", length(methods_used),") than factors
  (", length(all_factor),") detected, using only the first few, 
           until there is enough methods for the number of factor variables, 
           and dropping the rest.", sep = ""))
  methods_used <- methods_used[1:length(all_factor)]
}  
  
  final <- lapply(1:length(all_factor), FUN = function(i){
    current_factor <- all_factor[i]

    X_curr <- cbind(X[, .SD, .SDcols = !all_factor], X[,.SD, .SDcols = current_factor])
    
    res <- do.call( what = method_table[[methods_used[[i]]]],
                    args = list( X_curr,
                                 fact = current_factor,
                                 keep_factor = TRUE,
                                 encoding_only = TRUE))
    })
 

res <- X[final[[1]], on = all_factor[1]]
if(length(all_factor) > 1){
  for(i in 2:length(all_factor)){
    res <- res[final[[i]], on = all_factor[i]]
  }  
}
if(keep == FALSE){
  res[,(all_factor) := NULL]
}


return(res)
}




