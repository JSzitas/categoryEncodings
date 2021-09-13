#' Encode a given factor variable using median encoding
#'
#' @description Transforms the original design matrix using a median encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details This might be somewhat lacking in theory (to the author's best knowledge), but 
#' feel free to try it and publish the results if they turn out interesting on some 
#' particular problem. 
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @importFrom stats median 
#' @export
#'
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_median(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_median <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  medians <- X[, lapply(.SD, median, na.rm = TRUE), by = fact ]
  
  colnames(medians) <- c( fact,
                          paste( fact,"_",
                                 colnames(X)[which(colnames(X) != fact)],
                                 "_median", sep = ""))
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(medians[,-1])
    }
    else{
      return(medians)  
    }
  }
  X <- X[medians, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
