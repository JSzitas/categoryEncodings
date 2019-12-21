#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'



encode_mean <- function(X, fact){
  
  means <- X %>% 
    dplyr::group_by(!!dplyr::sym(fact)) %>%
    dplyr::summarise_all(mean)
  
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(means) <- c( name_fact_var,
                        paste( colnames_other_X,
                          "_mean", sep = ""))
  
  res <- dplyr::left_join(X, means, by = fact) %>% 
    dplyr::select(-!!dplyr::sym(fact))
  return(res)
  
}

encode_lowrank <- function(X, fact){
  
  means <- X %>% 
    dplyr::group_by(!!dplyr::sym(fact)) %>%
    dplyr::summarise_all(mean)
  
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
  
  
  res <- dplyr::left_join(X, low_rank, by = fact) %>% 
    dplyr::select(-!!dplyr::sym(fact))
  return(res)
}

encode_SPCA <- function(X, fact){
  
  means <- X %>% 
    dplyr::group_by(!!dplyr::sym(fact)) %>%
    dplyr::summarise_all(mean)
  
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
  
  
  res <- dplyr::left_join(X, PCAs, by = fact) %>% 
    dplyr::select(-!!dplyr::sym(fact))
  return(res)
}

encode_mnl <- function(X, fact){

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

  res <- dplyr::left_join(X, mnl, by = fact) %>% 
      dplyr::select(-!!dplyr::sym(fact))

  return(res)
}



encode_dummy <- function(X, fact){
  
  
  fact_levs <- levels(X[,which(colnames(X) == fact )])

  reference <- rep(0, length(fact_levs)-1)
  
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste(fact_levs[2:length(fact_levs)],
                             "_dummy" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  res <- dplyr::left_join(X, dummy_mat, by = fact) %>% 
    dplyr::select(-!!dplyr::sym(fact))
  
  return(res)
}


encode_deviation <- function(X, fact){
  
  fact_levs <- levels(X[,which(colnames(X) == fact )])
  
  reference <- rep(-1, length(fact_levs)-1)
  
  dummies <- data.frame(rbind(reference, diag(length(fact_levs)-1)))
  colnames(dummies) <- paste(fact_levs[1:length(fact_levs)-1],
                             "_deviate" , sep = "")
  
  rownames(dummies) <- NULL
  factor_var <- fact_levs
  
  dummy_mat <- cbind(factor_var, dummies )
  
  res <- dplyr::left_join(X, dummy_mat, by = fact) %>% 
    dplyr::select(-!!dplyr::sym(fact))
  
  return(res)
}


