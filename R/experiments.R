# write encodings for categoric variables 


new_fac <- sample(letters, 10)

design_mat <- data.frame(matrix(rnorm(5*100),ncol = 5))

design_mat <- cbind(design_mat, sample(new_fac, 100, replace = TRUE))
colnames(design_mat)[6] <- "factor_var"

# Johannemann, Hadad, Athey, Wager means encoding

     means <- design_mat %>% 
       dplyr::group_by(factor_var) %>%
       dplyr::summarise_all(mean) 
     colnames(means) <- c("factor_var",
                           paste(colnames(design_mat[,1:5]),"_mean", sep = ""))
  
res <- dplyr::left_join(design_mat, means, by = "factor_var") %>% 
  dplyr::select(-factor_var)

# Johannemann, Hadad, Athey, Wager low rank

means <- design_mat %>% 
  dplyr::group_by(factor_var) %>%
  dplyr::summarise_all(mean) 
colnames(means) <- c("factor_var",
                     paste(colnames(design_mat[,1:5]),"_mean", sep = ""))

low_rank <- cbind(data.frame(svd(means[,2:6])$u),means[,1])
colnames(low_rank) <- c( paste(colnames(design_mat[,1:5]),"_lowrank", sep = ""),
                         "factor_var")

res <- dplyr::left_join(design_mat, low_rank, by = "factor_var") %>% 
  dplyr::select(-factor_var)


# Johannemann, Hadad, Athey, Wager sparse PCA

means <- design_mat %>% 
  dplyr::group_by(factor_var) %>%
  dplyr::summarise_all(mean) 
colnames(means) <- c("factor_var",
                     paste(colnames(design_mat[,1:5]),"_mean", sep = ""))

  # it seems the scores argument is correct here
PCAs <- sparsepca::spca(means[,2:6]) %>% .[["scores"]] %>% cbind(means[,1])
colnames(PCAs) <- c( paste(colnames(design_mat[,1:5]),"_SPCA", sep = ""),
                         "factor_var")

res <- dplyr::left_join(design_mat, PCAs, by = "factor_var") %>% 
  dplyr::select(-factor_var)

# Johannemann, Hadad, Athey, Wager multinomial logit

reference <- matrix(rep(0,6), nrow = 1)
rownames(reference) <- levels(design_mat[,6])[1]

mnl <- data.frame( rbind( reference,
                          coef( nnet::multinom( formula = factor_var~.,
                                                data = design_mat) ) ))

colnames(mnl) <- paste(c("intercept",colnames(design_mat[,1:5]) ),"_mnl")
factor_var <- rownames(mnl)
mnl <- cbind(factor_var,mnl)
rownames(mnl) <- NULL

res <- dplyr::left_join(design_mat, mnl, by = "factor_var") %>% 
  dplyr::select(-factor_var)


# dummy

levels(design_mat[,6])[1]
reference <- rep(0, length(levels(design_mat[,6]))-1)

dummies <- data.frame(rbind(reference, diag(length(levels(design_mat[,6]))-1)))
colnames(dummies) <- paste(levels(design_mat[,6])[2:length(levels(design_mat[,6]))],
                           "_dummy" , sep = "")
rownames(dummies) <- NULL
factor_var <- levels(design_mat[,6])

dummy_mat <- cbind(factor_var, dummies )

res <- dplyr::left_join(design_mat, dummy_mat, by = "factor_var") %>% 
  dplyr::select(-factor_var)

# deviation 

reference <- rep(-1, length(levels(design_mat[,6]))-1)

dummies <- data.frame(rbind( diag(length(levels(design_mat[,6]))-1), reference))
colnames(dummies) <- paste(levels(design_mat[,6])[1:length(levels(design_mat[,6]))-1],
                           "_dummy" , sep = "")
rownames(dummies) <- NULL
factor_var <- levels(design_mat[,6])

dummy_mat <- cbind(factor_var, dummies )

res <- dplyr::left_join(design_mat, dummy_mat, by = "factor_var") %>% 
  dplyr::select(-factor_var)

#




