# data.table encodings 



encode_mean <- function(X, fact, keep_factor = FALSE){

  means <- dplyr::summarise_all(dplyr::group_by(X,!!dplyr::sym(fact)),mean)
  #means <- data.table::as.data.table(X)
  #data.table::setkeyv(means,fact)
  #means <- means[, means]
  # perhaps this doesnt require the joining at the end at all... 
  return(means)
  name_fact_var <- colnames(X)[which(colnames(X) == fact )]
  
  colnames_other_X <- 
    colnames( X[,which(colnames(X) != fact )] )
  
  colnames(means) <- c( name_fact_var,
                        paste( colnames_other_X,
                               "_mean", sep = ""))
  
  X <- data.table::data.table(X)
  X <- X[data.table::data.table(means), on = name_fact_var]
  
  if(keep_factor == FALSE){
    X <- X[,(name_fact_var) := NULL]
  }
  return(X)
}