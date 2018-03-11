#' transformDataset
#'
#' Transforms the given data frame so that the columns specified by outputColumns
#' are moved to the end of the data frame.
#'
#' @param dataset the data frame to transform
#' @param outputColumns a vector containing integers to specify the columns
#'
#' @return the data frame after transformation
#' @examples
#' transformDataset(dataset, c(3, 4))
#' transformDataset(dataset, 4)
transformDataset <- function(dataset, outputColumns) {

  if(!checkOutputBounds(ncol(dataset), outputColumns)) {
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
#' @examples
#' checkOutputBounds(ncol(dataset), c(4, 5))
#' checkOutputBounds(5, list(3, 5))
#' checkOutputBounds(7, 3)
checkOutputBounds <- function(numCol, outputColumns) {

  #Check there will still be an input column if using these outputColumns
  if(ncol(dataset) - length(outputColumns) < 1) {
    stop("Using the specified values for outputColumns will result in no
         input columns.")
  }

  #Check each integer correctly specifies a column
  return(all(outputColumns <= ncol(dataset) & outputColumns >= 1))
}
