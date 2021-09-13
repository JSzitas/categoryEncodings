#' Encode a given factor variable using deviation encoding
#'
#' @description Transforms the original design matrix using a deviation dummy encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The deviation dummy variable encoding, with reference class level set to -1. 
#' The reference class is always the last class observed. 
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
#' #encode_deviation(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_deviation <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  X <- data.table::data.table(X)
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  
  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  reference <- rep(-1, (length(factor_var)-1))
  
  dummies <- data.frame(rbind(reference, diag(length(factor_var)-1)))
  colnames(dummies) <- paste( fact,"_",
                              factor_var[1:length(factor_var)-1],
                              "_deviate" , sep = "")
  
  rownames(dummies) <- NULL
  
  
  dummy_mat <- cbind(factor_var, dummies )
  colnames(dummy_mat)[1] <- fact
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(dummy_mat[,-1])
    }
    else{
      return(dummy_mat)  
    }
  }
  
  X <- X[dummy_mat, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
