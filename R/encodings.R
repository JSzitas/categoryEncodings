#' Encode a given factor variable using means encoding 
#'
#' @description Transforms the original design matrix using a means encoding.
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
#' 'Sufficient Representations for Categorical Variables' - Means Encoding.
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
#' encode_mean(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 



encode_mean <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE ){

  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)
  

    means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact] 

  sel_vec <- which(colnames(X) == fact)

  colnames(means) <- c( fact,
                        paste( fact,"_",
                               colnames(X)[-sel_vec],
                               "_mean", sep = ""))
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(means[,-1])
    }
    else{
      return(means)  
    }
  }
  
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
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - Low rank.
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
#' encode_lowrank(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_lowrank <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE)
{
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  data.table::setkeyv(X, fact)

    means <- X[, lapply(.SD, mean, na.rm = TRUE), by = fact] 
  
  low_rank <- cbind( means[,1], data.table::data.table(svd(means[,2:ncol(means)])$u))
  colnames(low_rank) <- c( fact,
                           paste( fact,"_",
                                  (1:(ncol(low_rank)-1)),
                                  "_lowrank", sep = ""))
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(low_rank[,-1])
    }
    else{
      return(low_rank)  
    }
  }
  

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
#' 

encode_SPCA <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){

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
#' 

encode_mnl <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  X <- data.table::data.table(X)
  
  mnl <- glmnet::glmnet( x = as.matrix(X[,.SD, .SDcols = -fact]),
                         y = unlist(X[,.SD, .SDcols = fact ]),
                         family = "multinomial")
  mnl <- coef(mnl, s = min(mnl$lambda), na.rm = TRUE)
  mnl <- t(as.matrix(as.data.frame(lapply(mnl, as.matrix))))
  mnl <- apply(mnl, MARGIN = 2, FUN = as.numeric)
  mnl <- data.table::data.table(mnl)

  colnames(mnl) <-  paste( fact,"_",
                           c("intercept",
                             (1:(ncol(mnl)-1))),
                           "_mnl", sep = "")
  
  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  mnl <- cbind(factor_var,mnl)
  colnames(mnl)[1] <- fact
  mnl <- data.table::data.table(mnl)
  
  if(encoding_only == TRUE){
    if(keep_factor == FALSE){
      return(mnl[,-1])
    }
    else{
      return(mnl)  
    }
  }
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
#' @param encoding_only Whether to return the full transformed dataset or only the new 
#'                      columns. Defaults to FALSE and returns the full dataset.
#'                      
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The basic dummy variable encoding, with reference class level set to 0. 
#' The reference class is always the first class observed. 
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
#' encode_dummy(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_dummy <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){

  X <- data.table::data.table(X)
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }

  factor_var <- levels(as.factor(unlist(X[, .SD, .SDcols = fact])))
  
  reference <- rep(0, length(factor_var)-1)
  
  dummies <- data.frame(rbind(reference, diag(length(factor_var)-1)))
  colnames(dummies) <- paste( fact,"_",
                              factor_var[2:length(factor_var)],
                              "_dummy" , sep = "")
  
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

#' Encode a given factor variable using a repeated effect encoding
#'
#' @description Transforms the original design matrix using a repeated effect encoding.
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
#' encode_repeated_effect(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 

encode_repeated_effect <- function(X, fact, keep_factor = FALSE, encoding_only = FALSE){
  
  if(is.numeric(fact)){
    fact <- colnames(X)[fact]
  }
  
  X <- data.table::data.table(X)
  
  length_eff <- length(levels(as.factor(unlist(X[, .SD, .SDcols = fact]))))
  
  eff <- matrix(-1 / length_eff, nrow = length_eff, ncol = length_eff )
  eff <- eff + diag(length_eff)
  eff <- eff[, 1:(length_eff - 1)]
  
  
  eff <- matrix(0, nrow = length_eff, ncol = length_eff )
  eff <- matrix((length_eff - col(eff)) / length_eff, nrow = length_eff, ncol = length_eff )
  eff[lower.tri(eff)] <- 0
  eff_2 <- matrix(-col(eff) / length_eff, nrow = length_eff, ncol = length_eff )
  eff_2[upper.tri(eff_2)] <- 0
  diag(eff_2) <- 0
  eff <- eff + eff_2
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
