#' sampleWithoutBiasOrNA
#'
#' Perform the sample function after removing any duplicate or NA values from
#' the vector
#'
#' @param x the vector to choose from
#' @param size the number of items to choose
#' @param replace should sampling be with replacement?
#'
#' @return the result of the sample function on x
#'
sampleWithoutBiasOrNA <- function(x, size, replace = FALSE) {

  #Convert matrix to vector
  x <- c(x)

  #Remove any NA values
  x <- x[!is.na(x)]

  #Remove duplicates to avoid bias
  x <- unique(x)

  return(sample(x, size = size, replace = replace))
}

#' getValidInputs
#'
#' Determines the valid range of input chromoIDs for a given chromoID
#'
#' @param chromoID the chromoID to calculate the range of
#' @param functionNodeRange all the chromoIDs contained in functionNodes
#' @param functionNodeStructure the parameters used to create functionNodes
#'
#' @return the valid chromoIDs
#'
getValidInputs <- function(chromoID, functionNodeRange, functionNodeStructure) {

  #Put the functionNode chromoIDs into a matrix
  functionNodeMatrix <- matrix(functionNodeRange,
                               nrow = functionNodeStructure$rows,
                               ncol = functionNodeStructure$cols)

  #Find the column index containing chromoID
  column <- which(functionNodeMatrix == chromoID, arr.ind = TRUE)[[1, 2]]


  #Get the columns that are in the levelsBack range
  validColumns <- (column - functionNodeStructure$levelsBack):(column - 1)
  validColumns <- validColumns[validColumns >= 1]

  #Extract the chromoIDs from the validColumns
  validChromoIDs <- functionNodeMatrix[, validColumns]

  return(as.vector(validChromoIDs))
}

#' is.functionNode
#'
#' Checks if the node specified by chromoID is a functionNode
#'
#' @param solution the solution containing the nodes
#' @param chromoID the chromoID of the node to check
#'
#' @return a logical value stating if the node is an functionNode or not
#'
is.functionNode <- function(solution, chromoID) {

  return(is.element(chromoID, solution$functionNodes$chromoID))
}

#' is.outputNode
#'
#' Checks if the node specified by chromoID is an outputNode
#'
#' @param solution the solution containing the nodes
#' @param chromoID the chromoID of the node to check
#'
#' @return a logical value stating if the node is an outputNode or not
#'
is.outputNode <- function(solution, chromoID) {

  return(is.element(chromoID, solution$outputNodes$chromoID))
}

#' findRow
#'
#' Finds the row in the solution that contains chromoID
#'
#' @param solution the solution containing a row with chromoID
#' @param chromoID the row to find
#'
#' @return the row found
#'
findRow <- function(solution, chromoID) {

  #Get the chromoIDs of the functionNodes
  chromoIDs <- solution$functionNodes$chromoID

  #If the chromoID is the ID of an inputNode
  if (chromoID < min(chromoIDs)) {

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$inputNodes$chromoID == chromoID)

    return(solution$inputNodes[rowIndex, ])

    #If the chromoID is the ID of an outputNode
  } else if (chromoID > max(chromoIDs)) {

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$outputNodes$chromoID == chromoID)

    return(solution$outputNodes[rowIndex, ])

  } else {
    #The chromoID is the ID of a functionNode

    #Find the rowIndex of the node with this chromoID
    rowIndex <- which(solution$functionNodes$chromoID == chromoID)

    return(solution$functionNodes[rowIndex, ])
  }
}

#' sortPopulation
#'
#' Sorts the population from lowest fitness value to highest
#'
#' @param population the population to be sorted
#'
#' @return the population after sorting
#'
sortPopulation <- function(population) {

  #Extract the fitness values from the population
  fitnessValues <- sapply(population, "[[", "fitness")

  #Get the index ordering that will put the values
  #into ascending order and reorder the population
  return(population[order(fitnessValues, decreasing = FALSE, na.last = TRUE)])
}

#' printEvolutionDetails
#'
#' Print information about the progress of evolution
#'
#' @param currGeneration the current generation of evolution
#' @param maxGeneration the maximum generations for evolution
#' @param bestSolution the best solution found so far
#' @param population the population of all solutions
#' @param updateFreq how many generations pass between progress updates
#' @param updateCount how many generations have passed since last update
#'
#' @return updateCount + 1
#'
printEvolutionDetails <- function(currGeneration, maxGeneration,
                                  bestSolution, population,
                                  updateFreq, updateCount) {

  #Catch the case where the user does not want progress updates
  if(updateFreq == 0) {
    return(updateFreq)

    #If this is a generation to print information of
  } else if(updateCount == updateFreq) {
    avgFitness <- mean(sapply(population, "[[", "fitness"))
    cat("\nGeneration:", currGeneration, "/", maxGeneration)
    cat("\nFitness of best solution so far:", bestSolution$fitness)
    cat("\nAverage fitness of population:", avgFitness, "\n")

    updateCount <- 1
  }

  return(updateCount + 1)
}

#' printFinalDetails
#'
#' Print information on the final result of evolution
#'
#' @param finalGeneration the final generation of evolution
#' @param maxGeneration the maximum generations for evolution
#' @param bestSolution the best solution found
#' @param population the population of all solutions
#'
#' @return NULL
printFinalDetails <- function(finalGeneration, maxGeneration,
                              bestSolution, population) {

  avgFitness <- mean(sapply(population, "[[", "fitness"))
  cat("\nGeneration:", finalGeneration, "/", maxGeneration)
  cat("\nFitness of best solution so far:", bestSolution$fitness)
  cat("\nAverage fitness of population:", avgFitness, "\n")

  return(NULL)
}

#' extractRequiredNodes
#'
#' This function is to be used at the end of evolution so that only the required
#' nodes are passed out of the program.
#'
#' @param solution the solution containing the nodes to be extracted
#'
#' @return the required nodes
#'
extractRequiredNodes <- function(solution) {

  inputNodes <- solution$inputNodes

  outputNode <- solution$outputNodes[1, ]

  functionNodes <- nodesToProcess(solution)

  requiredNodes <- list(inputNodes = inputNodes,
                        functionNodes = functionNodes,
                        outputNodes = outputNode)

  return(requiredNodes)
}

#' printSolution
#'
#' Prints the solution in a textual format
#'
#' @param solution the solution to be printed as text
#' @param functionSet the functionSet used when creating the solution
#'
#' @return the textual format
#'
printSolution <- function(solution, functionSet) {

  functionNodes <- solution$functionNodes

  startChromoID <- tail(functionNodes, 1)$chromoID

  text <- buildSolutionText(functionNodes, functionSet, startChromoID)

  cat("\n", "Best solution found as text:\n", text, "\n", sep = "")

  return(text)
}

#' buildSolutionText
#'
#' Used to recursively build up a character vector which shows the function
#' applied at the given node and each of its inputs.
#'
#' @param functionNodes the structure of function nodes
#' @param functionSet the functionSet used when creating the functionNodes
#' @param chromoID the chromoID of the node to start at
#'
#' @return the character vector built
#'
buildSolutionText <- function(functionNodes, functionSet, chromoID) {

  #If the chromoID is the ID of an inputNode
  if (chromoID < min(functionNodes$chromoID)) {
    #Write a variable name into functionText
    functionText <- letters[chromoID]
    #If the function has two arguments
  } else {

    #Get the current node
    currentNode <- functionNodes[functionNodes$chromoID == chromoID, ]

    #Get the function from the functionSet
    func <- functionSet[currentNode$funcID, ]

    #Get the inputs to the function
    inputs <- currentNode$inputs[[1]]

    #Create the structure for a one parameter function
    functionText <- paste(func$funcName, "(arg1)", sep = "")

    if (func$arity == 2) {
      #Create the structure for a two parameter function
      functionText <- paste("(arg1 ", func$funcName, " arg2)", sep = "")

      rightArgument <- buildSolutionText(functionNodes, functionSet, inputs[2])

      #Replace the x and y character by the rightArgument
      functionText <- gsub("arg2", rightArgument, functionText)
    }

    leftArgument <- buildSolutionText(functionNodes, functionSet, inputs[1])

    #Replace the y character by the argument
    functionText <- gsub("arg1", leftArgument, functionText)
  }

  return(functionText)
}

#' plotGraph
#'
#' Plots the graph using ggplot2 and plotly and displays it in an interactive
#' shiny window through the browser.
#'
#' @param plotData the data to plot
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous
#' @importFrom ggplot2 xlab ylab ggtitle
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom shiny fluidPage shinyApp runApp
#'
plotGraph <- function(plotData) {

  #This function was adapted from Michael Majka's answer on Stack Overflow
  #The thread can be found here:
  #https://stackoverflow.com/a/38919892/4474422

  rows <- nrow(plotData)
  breaks <- round(c(1, rows * 0.125, rows * 0.25, rows * 0.375, rows * 0.5,
                    rows * 0.625, rows * 0.75, rows * 0.875, rows))

  ui <- fluidPage(
    plotlyOutput("fitnessPlot")
  )

  server <- function(input, output) {
    output$fitnessPlot <- renderPlotly({
      ggplot(plotData, aes(x = plotData$generation, y = plotData$bestFitness)) +
        geom_line(colour = "firebrick") +
        geom_point(colour = "firebrick") +
        scale_x_continuous(breaks = breaks) +
        xlab("Generation") + ylab("Fitness Value") +
        ggtitle("Fitness Values from each Generation")
    })
  }

  app = shinyApp(ui = ui, server = server)

  runApp(app)
}
