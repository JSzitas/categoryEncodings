% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_categories.R
\name{encode_categories}
\alias{encode_categories}
\title{Encode a given factor variable automatically}
\usage{
encode_categories(X, Y = NULL, fact = NULL, method = NULL, keep = FALSE)
}
\arguments{
\item{X}{The data.frame/data.table to transform.}

\item{Y}{Optional: The dependent variable to ignore in the transformation.}

\item{fact}{Optional: The factor variable(s) to encode by -
either positive integer(s) specifying the
column number, or the name(s) of the column.
If left empty a heuristic is used to determine the factor
variable(s), and a warning is written with the names of
the variables converted.}

\item{method}{Optional: A character string indicating which encoding method to use,
              either of the following:
              * "mean"
              * "median"
              * "deviation"
              * "lowrank"
              * "spca"
              * "mnl"
              * "dummy"
              * "difference"
              * "helmert"
              * "simple_effect"
              * "repeated_effect"
If only a single method is specified, it is taken to encode either all of the variables
supplied through *fact*, or variables which have been flagged as factors automatically.
If multiple methods are specified, the number of methods must match the number of
factor variables in *fact* - and these are applied to correspond in the order in
which they were supplied. In case a missmatch occurs, an error is raised.
If left empty, the appriopriate method is selected on a case by case basis
(and the selected methods are written out to console).}

\item{keep}{Whether to keep the original factor column(s), defaults to **FALSE**.}
}
\value{
A new data.table X which contains the new columns and optionally the old factor(s).
}
\description{
**[deprecated: use encoder()]** Transforms the original design matrix automatically, using the appropriate encoding.
}
\details{
Automatically selects the appropriate method given the number of anticipated
         newly created variables, based on the results in Johannemann et al.(2019)
         'Sufficient Representations for Categorical Variables', and
         a simple heuristic - where
}
\examples{

design_mat <- cbind( data.frame( matrix(rnorm(5*100),ncol = 5) ),
                     sample( sample(letters, 10), 100, replace = TRUE)
                     )
colnames(design_mat)[6] <- "factor_var"

 encode_categories( design_mat, method = "mean" )

}
