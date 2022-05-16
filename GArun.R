library(GA)#This function is used by the GA to compute or report the statistics of your interest after every generation.
#This function overrides the default functionality provided by gaMonitor().
monitor <- function(obj){
  # gaMonitor(obj) #call the default gaMonitor to print the usual messages during evolution
  iter <- obj@iter #get the current iternation/generation number
  if (iter <= maxGenerations){ #some error checking
    fitness <- obj@fitness #get the array of all the fitness values in the present population
    #<<- assigns a value to the global variable declared outside the scope of this function.
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }
  else{ #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}
runGA <- function(noRuns = 50, problem = "sharpe"){
  #Specify GA parameter values; using the default values below.
  if (problem == "feature"){
    maxGenerations <<- 200 #<<- makes it a global variable. So it will be visible to other functions e.g. monitor()
    popSize = 200
    pcrossover = 0.8
    pmutation = 0.1
    type = "binary"
    data <- asset_returns
    xx <- data[,-ncol(data)]
    yy <- data[,ncol(data)]
    fitness = featureFitness #fitness function defined in feature-selection.R
  }
  else if (problem == "sharpe"){
    maxGenerations <<- 200
    crossover= gareal_laCrossover
    popSize = 200
    pcrossover = 0.6
    pmutation = 0.2
    run = 30
    type = "real-valued"
    data= asset_returns_test
    min = rep(0,ncol(asset_returns_test)) #minimum is city indexed 1
    max = rep(1,ncol(asset_returns_test))
    #maximum is the number of cities in the data set
    fitness =function(x){-obj(x)}
    #fitness function defined in TSP.R
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  #Set up what stats you wish to note.
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1) #stats of all the runs
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx),
               names = colnames(xx), seed=i, popSize = popSize,
               pcrossover = pcrossover, pmutation = pmutation,
               maxiter = maxGenerations, monitor= monitor)
    else if (problem == "tsp")
      GA <- ga(type = type, fitness = fitness, distMatrix = data,
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               run = run, pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    else if (problem == "sharpe")
      GA <- ga(type = type, fitness = fitness, crossover=crossover,
               lower = min, upper = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}
getBestFitness<-function(){
  return(bestFitness)
}
getBestSolution<-function(){
  return(bestSolution)
}

std = function(x){
  port.returns = portfolio_returns(x)
  
  return (var(port.returns))
}
Mean = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns))
}
mean(arithmetic)

arithmetic=runGA(noRuns = 30, problem = "sharpe")


bestFitness
bestSolution














