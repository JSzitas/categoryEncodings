#' Encode a given factor variable using a multinomial logit representation
#'
#' @description Transforms the original design matrix using a mnl encoding.
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
#' 'Sufficient Representations for Categorical Variables' - mnl.
#' @importFrom data.table data.table
#' @importFrom data.table setkeyv
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @importFrom glmnet glmnet
#' @importFrom stats formula 
#' @importFrom stats coef
#' @export
#' 
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_mnl(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
encode_mnl <-
  function(X,
           fact,
           keep_factor = FALSE,
           encoding_only = FALSE) {
    if (is.numeric(fact)) {
      fact <- colnames(X)[fact]
    }
    X <- data.table::data.table(X)
    
    mnl <- glmnet::glmnet(x = as.matrix(X[, .SD, .SDcols = -fact]),
                          y = unlist(X[, .SD, .SDcols = fact]),
                          family = "multinomial")
    mnl <- coef(mnl, s = min(mnl$lambda), na.rm = TRUE)
    mnl <- t(as.matrix(as.data.frame(lapply(mnl, as.matrix))))
    mnl <- apply(mnl, MARGIN = 2, FUN = as.numeric)
    mnl <- data.table::data.table(mnl)
    
    colnames(mnl) <-  paste(fact, "_",
                            c("intercept",
                              (1:(ncol(
                                mnl
                              ) - 1))),
                            "_mnl", sep = "")
    
    factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
    
    mnl <- cbind(factor_var, mnl)
    colnames(mnl)[1] <- fact
    mnl <- data.table::data.table(mnl)
    
    if (encoding_only == TRUE) {
      if (keep_factor == FALSE) {
        return(mnl[, -1])
      }
      else{
        return(mnl)
      }
    }
    X <- X[mnl, on = fact]
    
    if (keep_factor == FALSE) {
      X[, (fact) := NULL]
    }
    return(X)
}
