#' Encode a given factor variable using means encoding 
#'
#' @description Transforms the original design matrix using a means encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - Means Encoding.
#' @import data.table
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
  
  means <- dplyr::summarise_all(dplyr::group_by(X,!!dplyr::sym(fact)),mean)
  
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(means) <- c( name_fact_var,
                        paste( colnames_other_X,
                          "_mean", sep = ""))
  
  res <- dplyr::left_join(X, means, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}

#' Encode a given factor variable using low rank encoding
#'
#' @description Transforms the original design matrix using a low rank encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - Low rank.
#' @import data.table
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
  
  means <- dplyr::summarise_all(dplyr::group_by(X,!!dplyr::sym(fact)),mean)
  
  
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(means) <- c( name_fact_var,
                        paste(
                          colnames_other_X,
                          "_mean", sep = ""))
  
  
  
  low_rank <- cbind(data.frame(svd(means[,2:ncol(means)])$u),means[,1])
  colnames(low_rank) <- c( paste( colnames_other_X,"_lowrank", sep = ""),
                           name_fact_var )
  
  
  res <- dplyr::left_join(X, low_rank, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}

#' Encode a given factor variable using a sparse PCA representation
#'
#' @description Transforms the original design matrix using a sPCA encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - sPCA.
#' @import data.table
#'
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_SPCA(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_SPCA <- function(X, fact, keep_factor = FALSE){
  
  means <- dplyr::summarise_all(dplyr::group_by(X,!!dplyr::sym(fact)),mean)
  
  
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(means) <- c( name_fact_var,
                        paste(
                          colnames_other_X,
                          "_mean", sep = ""))
  
  
  SPCA <- sparsepca::spca(means[,2:ncol(means)])
  
  PCAs <- cbind(SPCA[["scores"]], means[,1])
  colnames(PCAs) <- c( paste( colnames_other_X,"_SPCA", sep = ""),
                           name_fact_var )
  
  res <- dplyr::left_join(X, PCAs, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}

#' Encode a given factor variable using a multinomial logit representation
#'
#' @description Transforms the original design matrix using a mnl encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details Uses the method from Johannemann et al.(2019) 
#' 'Sufficient Representations for Categorical Variables' - mnl.
#' @import data.table
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

  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  reference <- matrix(rep(0,ncol(X)), nrow = 1)
  rownames(reference) <- levels(X[,fact])[1]
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )

  mnl <- data.frame( rbind( reference,
                            coef( nnet::multinom( formula = formula(
                              paste(name_fact_var,"~.", sep = "")),
                                                  data = X) ) ))
  
  colnames(mnl) <- paste( c("intercept",colnames_other_X), "_mnl", sep = "")
  factor_var <- rownames(mnl)
  mnl <- cbind(factor_var,mnl)
  rownames(mnl) <- NULL

  res <- dplyr::left_join(X, mnl, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }

  return(res)
}

#' Encode a given factor variable using dummy variables
#'
#' @description Transforms the original design matrix using a dummy variable encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The basic dummy variable encoding, with reference class level set to 0. 
#' The reference class is always the first class observed. 
#' @import data.table
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
  
  
  fact_levs <- levels(X[,which(colnames(X) == fact )])

  reference <- rep(0, length(fact_levs)-1)
  
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste(fact_levs[2:length(fact_levs)],
                             "_dummy" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  res <- dplyr::left_join(X, dummy_mat, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}

#' Encode a given factor variable using deviation encoding
#'
#' @description Transforms the original design matrix using a deviation dummy encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details The deviation dummy variable encoding, with reference class level set to -1. 
#' The reference class is always the last class observed. 
#' @import data.table
#'
#' @examples
#' 
#' design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
#'                      sample( sample(letters, 10), 100, replace = TRUE)
#'                      )
#' colnames(design_mat)[6] <- "factor_var"
#' 
#' encode_deviation(X = design_mat, fact = "factor_var", keep_factor = FALSE)
#' 
#' 

encode_deviation <- function(X, fact, keep_factor = FALSE){
  
  fact_levs <- levels(X[,which(colnames(X) == fact )])
  
  reference <- rep(-1, length(fact_levs)-1)
  
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste(fact_levs[1:length(fact_levs)-1],
                             "_deviate" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  res <- dplyr::left_join(X, dummy_mat, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}


#' Encode a given factor variable using median encoding
#'
#' @description Transforms the original design matrix using a median encoding.
#'
#' @param X The data.frame/data.table to transform. 
#' @param fact The factor variable to encode by.
#' @param keep_factor Whether to keep the original factor column(defaults to **FALSE**).
#' @return A new data.table X which contains the new columns and optionally the old factor.
#' @details This might be somewhat lacking in theory (to the author's best knowledge), but 
#' feel free to try it and publish the results if they turn out interesting on some 
#' particular problem. 
#' @import data.table
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
  
  medians <- dplyr::summarise_all(dplyr::group_by(X,!!dplyr::sym(fact)),median)
  
  
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(medians) <- c( name_fact_var,
                        paste( colnames_other_X,
                               "_mean", sep = ""))
  
  res <- dplyr::left_join(X, medians, by = fact)
  if(keep_factor == FALSE){
    res <- dplyr::select( res, -!!dplyr::sym(fact))
  }
  return(res)
}



