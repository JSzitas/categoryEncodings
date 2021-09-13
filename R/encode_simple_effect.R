#' Encode a given factor variable using a simple effect encoding
#'
#' @description Transforms the original design matrix using a simple effect encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' 
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @export
#'
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_simple_effect(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_simple_effect <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  
  X <- data.table::data.table(X)
  
  length_eff <- length(levels(as.factor(unlist(X[, .SD, .SDcols = fact]))))
  
  eff <- matrix(-1 / length_eff, nrow = length_eff, ncol = length_eff )
  eff <- eff + diag(length_eff)
  eff <- data.table::data.table( eff[, 1:(length_eff - 1)] )
  
  colnames(eff) <-  paste( fact,"_",
                           c((1:(ncol(eff)))),
                           "_eff", sep = "")
  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  eff <- cbind( factor_var, eff )
  colnames(eff)[1] <- fact
  eff <- data.table::data.table(eff)
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(eff[,-1])
    }
    else{
      return(eff)  
    }
  }
  X <- X[eff, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
