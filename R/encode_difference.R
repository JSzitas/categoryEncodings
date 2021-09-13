#' Encode a given factor variable using difference encoding
#'
#' @description Transforms the original design matrix using a difference encoding.
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
#' encode_difference(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_difference <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  
  X <- data.table::data.table(X)
  
  length_diff <- length(levels(as.factor(unlist(X[, .SD, .SDcols = fact]))))
  
  diff <- matrix(-1 / (col(matrix(0,length_diff, length_diff)) + 1),
                 length_diff, length_diff)
  
  diff[lower.tri(diff)] <- 0
  diff <- diff[, 1:(length_diff - 1)]
  diff[row(diff) == (col(diff) + 1)] <- - apply(diff, 2, sum)
  diff <- data.table::data.table(diff)
  
  colnames(diff) <-  paste( fact,"_",
                            c((1:(ncol(diff)))),
                            "_diff", sep = "")
  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  diff <- cbind( factor_var, diff )
  colnames(diff)[1] <- fact
  diff <- data.table::data.table(diff)
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(diff[,-1])
    }
    else{
      return(diff)  
    }
  }
  X <- X[diff, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
