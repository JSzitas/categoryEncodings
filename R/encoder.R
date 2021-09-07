#' Train an encoder
#' 
#' @description Make your own encoder to be used in a pipeline
#' 
#' @param X The data.frame/data.table to transform.
#' @param Y Optional: The dependent variable to ignore in the transformation.
#' @param fact Optional: The factor variable(s) to encode by -
#'             either positive integer(s) specifying the
#'             column number, or the name(s) of the column.
#'             If left empty a heuristic is used to determine the factor
#'             variable(s), and a warning is written with the names of
#'             the variables converted.
#' @param method Optional: A character string indicating which encoding method to use,
#'               either of the following:
#'               * "mean"
#'               * "median"
#'               * "deviation"
#'               * "lowrank"
#'               * "spca"
#'               * "mnl"
#'               * "dummy"
#'               * "difference"
#'               * "helmert"
#'               * "simple_effect"
#'               * "repeated_effect"
#' If only a single method is specified, it is taken to encode either all of the variables
#' supplied through *fact*, or variables which have been flagged as factors automatically.
#' If multiple methods are specified, the number of methods must match the number of
#' factor variables in *fact* - and these are applied to correspond in the order in
#' which they were supplied. In case a missmatch occurs, an error is raised.
#' If left empty, the appriopriate method is selected on a case by case basis
#' (and the selected methods are written out to console).
#' @param custom_encoding_assignment **experimental** A function which takes two arguments (**X** and **fact**) denoting 
#' the data and the factors, respectivelly, and assigns a valid encoding **method** to each factor in **fact**. 
#' @return A new data.table X which contains the new columns and optionally the old factor(s).
#' @details Automatically selects the appropriate method given the number of anticipated
#'          newly created variables, based on the results in Johannemann et al.(2019)
#'          'Sufficient Representations for Categorical Variables', and
#'          a simple heuristic - where
#'
#' @importFrom data.table data.table
#' @importFrom data.table .SD
#' @importFrom data.table ':='
#' @export
#'
encoder <- function( X, Y = NULL, fact = NULL, method = NULL, custom_encoding_assignment = NULL, ... )
{
  X <- to_data_table(X)
  # get rid of Y (if applicable)
  x_y_list <- handle_y(X,Y)
  X <- x_y_list[["X"]]
  Y <- x_y_list[["Y"]]
  # find factors if necessary
  factors <- handle_factors( X, fact )
  # get relevant encodings
  assigned_encodings <- assign_encodings( X, factors, method, custom_encoding_assignment )
  result <- fit_encoder( X, assigned_encodings )
  result[["encoded"]] <- cbind(result[["encoded"]], Y)
  return(result)
}
