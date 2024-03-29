% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_median.R
\name{encode_median}
\alias{encode_median}
\title{Encode a given factor variable using median encoding}
\usage{
encode_median(X, fact, keep_factor = FALSE, encoding_only = FALSE)
}
\arguments{
\item{X}{The data.frame/data.table to transform.}

\item{fact}{The factor variable to encode by - either a positive integer specifying the 
column number, or the name of the column.}

\item{keep_factor}{Whether to keep the original factor column(defaults to **FALSE**).}

\item{encoding_only}{Whether to return the full transformed dataset or only the new 
columns. Defaults to FALSE and returns the full dataset.}
}
\value{
A new data.table X which contains the new columns and optionally the old factor.
}
\description{
Transforms the original design matrix using a median encoding.
}
\details{
This might be somewhat lacking in theory (to the author's best knowledge), but 
feel free to try it and publish the results if they turn out interesting on some 
particular problem.
}
\examples{

design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
                     sample( sample(letters, 10), 100, replace = TRUE)
                     )
colnames(design_mat)[6] <- "factor_var"

encode_median(X = design_mat, fact = "factor_var", keep_factor = FALSE)

}
