#' Encode a given factor variable using helmert encoding
#'
#' @description Transforms the original design matrix using a helmert 
#' (reverse difference) encoding.
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
#' encode_helmert(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_helmert <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  
  X <- data.table::data.table(X)
  
  length_helm <- length(levels(as.factor(unlist(X[, .SD, .SDcols = fact]))))
  helm <- diag(1:length_helm ) - 1
  helm[lower.tri(helm)] <- 0
  helm <- helm/rep( 1:length_helm,
                    each = length_helm )
  helm <- data.table::data.table(helm[,-1])
  
  
  colnames(helm) <-  paste( fact,"_",
                            c((1:(ncol(helm)))),
                            "_helm", sep = "")
  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  helm <- cbind( factor_var, helm )
  colnames(helm)[1] <- fact
  helm <- data.table::data.table(helm)
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(helm[,-1])
    }
    else{
      return(helm)  
    }
  }
  X <- X[helm, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
