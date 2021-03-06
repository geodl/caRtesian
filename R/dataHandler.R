#' transformDataset
#'
#' Transforms the given data frame so that the columns specified by outputColumns
#' are moved to the end of the data frame.
#'
#' @param dataset the data frame to transform
#' @param outputColumns a vector containing integers to specify the columns
#'
#' @return the data frame after transformation
#'
transformDataset <- function(dataset, outputColumns) {

  if (!checkOutputBounds(ncol(dataset), outputColumns)) {
    stop("The values specified in outputColumns are not in the correct range")
  }

  columnRange <- 1:ncol(dataset)

  #Find the indexes of the columns that are outputs
  columnsToRemove <- match(outputColumns, columnRange)

  #Remove the columns from columnRange that are in columnsToRemove
  columnRange <- columnRange[! columnRange %in% columnsToRemove]

  #Return the dataset with the outputColumns moved to the end
  return(dataset[, c(columnRange, outputColumns)])
}

#' checkOutputBounds
#'
#' Checks that the columns chosen as outputs are valid choices. They are valid
#' if each column specified is within the range of 1 and numCol, and that at
#' least 1 column will remain to be used as input if using all output column
#' choices.
#'
#' @param numCol the number of columns in the dataset
#' @param outputColumns a vector containing integers to specify the columns
#'
#' @return a boolean of if the choices were valid or not
#'
checkOutputBounds <- function(numCol, outputColumns) {

  #Check there will still be an input column if using these outputColumns
  if (numCol - length(outputColumns) < 1) {
    stop("Using the specified values for outputColumns will result in no
         input columns.")
  }

  #Check each integer correctly specifies a column
  return(all(outputColumns <= numCol & outputColumns >= 1))
}

#' extractRequiredFields
#'
#' Extract the fields from dataset that are contained in model. It is a wrapper
#' for model.frame which provides a more informative handling of errors by
#' stating which variables were missing if any.
#'
#' @param dataset the dataset to extract from
#' @param model a formula containing the variables to extract
#'
#' @return a dataset containing only the extracted fields
#' @importFrom stats model.frame
#'
extractRequiredFields <- function(dataset, model) {

  out <- tryCatch({
    dataset <- model.frame(model, dataset)

  }, error = function(cond) {

    datasetColumns <- colnames(dataset)
    modelVariables <- all.vars(model)

    missingVariables <- c()

    #Find the variables missing from datasetColumns
    for (i in modelVariables) {
      if (!is.element(i, datasetColumns)) {
        missingVariables <- c(missingVariables, i)
      }
    }
    stop("Error: model specified variables which could not be found in the ",
         "dataset. The missing variables were: ", missingVariables)

  })

  return(out)
}

#' calculateOutputSize
#'
#' Calculates the output size from a given model. The output size is the
#' number of terms on the left hand side of the model.
#'
#' @param model the formula to calculate the output size of
#'
#' @return the number of terms on the left hand side of the model
#'
calculateOutputSize <- function(model) {
  return(length(all.vars(model[[2]])))
}

#' calculateInputSize
#'
#' Calculates the input size from a given model. The input size is the
#' number of terms on the right hand side of the model.
#'
#' @param model the formula to calculate the input size of
#'
#' @return the number of terms on the right hand side of the model
#'
calculateInputSize <- function(model) {
  return(length(all.vars(model[[3]])))
}

#' validSelectionInput
#'
#' Checks that the number of arguments provided matches the what the provided
#' function expects.
#'
#' @param arguments A list containing the function and arguments
#'
#' @return a boolean stating if the arguments are valid
#' @export
#' @examples
#'
#' foo <- function() {}
#'
#' #Extra parameters to be added in the args member to match the parameters
#' #foo expects
#' validSelectionInput(list(func = foo, args = c(population = NA)))
validSelectionInput <- function(arguments) {

  expectedArgsLength <- length(formals(arguments$func))
  providedArgsLength <- length(arguments$args)

  if (expectedArgsLength != providedArgsLength) {
    message("Error: The function provided expects ", expectedArgsLength,
        " arguments but ", providedArgsLength, " were provided.\n")
    return(FALSE)
  }

  if (!("population" %in% names(arguments$args))) {
    message("Error: 'population' was not found in arguments$args. The ",
        "structure should follow list (func = foo, c('population' = NA, ...))",
        "\n")
    return(FALSE)
  }

  if (!is.na(arguments$args["population"])) {
    message("Error: 'population' parameter is not NA.\n")
    return(FALSE)
  }

  message("Success: The provided arguments match the number of arguments the",
      "provided function expects.\n")
  return(TRUE)
}
