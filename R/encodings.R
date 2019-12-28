#' Encode a given factor variable using means encoding 
#'
#' @description Transforms the original design matrix using a means encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - Means Encoding.
#' @import data.table
#' @export
#' 
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_mean(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 



encode_mean <- function(X, fact, keep_factor = FALSE){

  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact ]

  colnames(means) <- c( fact,
                        paste( fact,"_",
                               colnames(X)[which(colnames(X) != fact)],
                               "_mean", sep = ""))
  X <- X[means, on = fact]

  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}


#' Encode a given factor variable using low rank encoding
#'
#' @description Transforms the original design matrix using a low rank encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - Low rank.
#' @import data.table
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
#' encode_lowrank(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_lowrank <- function(X, fact, keep_factor = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  .SD <- NULL
  means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact ]

  
  low_rank <- cbind(data.table::data.table(svd(means[,2:ncol(means)])$u),means[,1])
  colnames(low_rank) <- c( paste( fact,"_",
                                  colnames(X)[which(colnames(X) != fact)],
                                  "_lowrank", sep = ""),
                           fact)

  X <- X[low_rank, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}


#' Encode a given factor variable using a sparse PCA representation
#'
#' @description Transforms the original design matrix using a sPCA encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - sPCA.
#' @import data.table
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
#' 

encode_SPCA <- function(X, fact, keep_factor = FALSE){

  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact ]
  
  SPCA <- sparsepca::spca(means[,2:ncol(means)], verbose = FALSE)
  
  PCAs <- cbind(SPCA[["scores"]], means[,1])
  colnames(PCAs) <- c( paste( fact, "_",
                              colnames(X)[which(colnames(X) != fact)],
                              "_SPCA", sep = ""),
                       fact )
  
  X <- X[PCAs, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}

#' Encode a given factor variable using a multinomial logit representation
#'
#' @description Transforms the original design matrix using a mnl encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - mnl.
#' @import data.table
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @importFrom nnet multinom
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
#' 

encode_mnl <- function(X, fact, keep_factor = FALSE){

  factor_var <- levels(unlist(X[,fact]))
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  reference <- matrix(rep(0,ncol(X)), nrow = 1)
  
  X <- data.table::data.table(X)
  random_file <- file()
  sink(file = random_file,type = "output")
  mnl <- data.frame( rbind( reference,
                            coef( nnet::multinom( formula = formula(
                              paste(fact,"~.", sep = "")),
                              data = as.data.frame(X)) ) ))
  sink()
  close(random_file)
  colnames(mnl) <- paste( fact,"_",
                          c("intercept",
                            colnames(X)[which(colnames(X) != fact)]),
                          "_mnl", sep = "")
  mnl <- cbind(factor_var,mnl)
  rownames(mnl) <- NULL

  X <- X[mnl, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}

#' Encode a given factor variable using dummy variables
#'
#' @description Transforms the original design matrix using a dummy variable encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The basic dummy variable encoding, with reference class level set to 0. 
#' The reference class is always the first class observed. 
#' @import data.table
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
#' encode_dummy(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_dummy <- function(X, fact, keep_factor = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }

  fact_levs <- levels(X[,which(colnames(X) == fact )])
  
  reference <- rep(0, length(fact_levs)-1)
  
  X <- data.table::data.table(X)
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste( fact,"_",
                              fact_levs[2:length(fact_levs)],
                              "_dummy" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  X <- X[dummy_mat, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}

#' Encode a given factor variable using deviation encoding
#'
#' @description Transforms the original design matrix using a deviation dummy encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The deviation dummy variable encoding, with reference class level set to -1. 
#' The reference class is always the last class observed. 
#' @import data.table
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
#' 

encode_deviation <- function(X, fact, keep_factor = FALSE){
  
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }

  fact_levs <- levels(X[,which(colnames(X) == fact )])
  reference <- rep(-1, length(fact_levs)-1)
  
  X <- data.table::data.table(X)
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste( fact,"_",
                              fact_levs[1:length(fact_levs)-1],
                              "_deviate" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  X <- X[dummy_mat, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}


#' Encode a given factor variable using median encoding
#'
#' @description Transforms the original design matrix using a median encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by - either a positive integer specifying the 
#'             column number, or the name of the column.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details This might be somewhat lacking in theory (to the author's best knowledge), but 
#' feel free to try it and publish the results if they turn out interesting on some 
#' particular problem. 
#' @import data.table
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
#' 

encode_median <- function(X, fact, keep_factor = FALSE){
  
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
  X <- X[medians, on = fact]
  
  if(keep_factor == FALSE){
    X[,(fact) := NULL]
  }
  return(X)
}



