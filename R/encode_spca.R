#' Encode a given factor variable using a sparse PCA representation
#'
#' @description Transforms the original design matrix using a sPCA encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.  
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - sPCA.
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @importFrom sparsepca spca
#' @export
#' 
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' #encode_SPCA(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_spca <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact ]
  
  SPCA <- sparsepca::spca(means[,2:ncol(means)], verbose = FALSE)
  
  PCAs <- cbind(means[,1], SPCA[["scores"]])
  colnames(PCAs) <- c( fact ,
                       paste( fact, "_",
                              (1:(ncol(PCAs)-1)),
                              "_SPCA", sep = "")
  )
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(PCAs[,-1])
    }
    else{
      return(PCAs)  
    }
  }
  
  
  X <- X[PCAs, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}
