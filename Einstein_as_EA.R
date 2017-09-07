#!/usr/bin/env Rscript
############################################################
# Title: Solving the Einstein Riddle
# Author: Steph de Silva
# Email: steph@rex-analytics.com
# Date last modified: 21/08/17
#############################################################
# Purpose: the program solves Einstein's riddle using a number
# of different algorithms. User to choose the algorithm.
#############################################################
#############################################################
# Section 1: Parameter allocation, problem set up
# Section 2: Opening messages, discussion
# Section 3: Define functions for algorithm
# Section 4: The algorithm
# Section 5: Results of the algorithm
# Section 6: Close.
#################################################################
# Problem Structure
# 5x5 matrix
# col 1 nationalities
# col 2 colour
# col 3 pet
# col 4 drink
# col 5 cigarette
# row number is house number
#############################################################
# Section 1: Parameter allocation, problem set up
#############################################################
rm(list=ls(all=TRUE))
set.seed(5678)
# setwd("~/Documents/UNE/COSC550/Programming Assignment")

library(abind)
library(ggplot2)
nationalities <- c("Brit", "Norwegian", "Dane", "German", "Swede")
colours <- c("blue", "green", "yellow", "white", "red")
pets <- c("horse", "cat", "dog", "fish", "bird")
drinks <- c("water", "beer", "coffee", "tea", "milk")
cigarettes <- c("pall mall", "dunhill", "blends", "bluemasters", "prince")
x<- cbind(nationalities, colours, pets, drinks, cigarettes)
popsize <- 200 # population size
numgen <- 50000 # number of generations to iterate
CR <- 0.5 # baseline probability of crossover
rowCross <- 2 # number of rows to cross over
mutability <- 0.9 # baseline mutation rate
minMutate <- 10  # baseline minimum mutation
maxMutate <- 20 # baseline maximum mutation
fitSel <- 1 # which fitness criteria to use if 3, return vector
initialisation <- 1 # 0 random, 1 from a reduced matrix with ARC3 variety
boundary1 <- 250 # when to switch to something to get out of the local minima
temperature <-10 # parameters on simulated annealing
cool <- 0.99

# Solution
test_set <- {}
test_set <- cbind(test_set, c("Norwegian","Dane", "Brit", "German", "Swede"))
test_set <- cbind(test_set, c("yellow", "blue", "red", "green", "white"))
test_set <- cbind(test_set, c("cat", "horse", "bird", "fish", "dog"))
test_set <- cbind(test_set, c("water", "tea", "milk", "coffee", "beer"))
test_set <- cbind(test_set, c("dunhill", "blends", "pall mall", "prince", "bluemasters"))
#############################################################
# Section2. Initial Messages and discussion
#############################################################
cat("\n=============================================================================\n")
cat ("Welcome to Steph's attempt to solve the problem with an evolutionary algorithm (EA).\n")
cat ("Author: Steph de Silva")
cat ("Contact: steph@rex-analytics.com")
cat("\n=============================================================================\n")
cat ("\n")
response <- 99999
while (response != 4){
  cat("\n=============================================================================\n")
  cat ("Here is the menu of choices for you: \n")
  cat ("1. Run program, collect results \n")
  cat ("2. View basic information on algorithm \n")
  cat ("3. View package dependency and version information for R \n")
  cat ("4. Exit. \n")
  cat ("Please make a selection: 1-4 \n")
  cat("\n=============================================================================\n")
  response <-readLines(file("stdin"),1)
  on.exit(close(response))
  # if (response != 6 | response != 5 | response != 4 | response != 3 | response !=2 | response != 1){
  #   cat ("Please make a selection:")
  #   cat ("Here is the menu of choices for you: \n")
  #   cat ("1. Run program, collect results \n")
  #   cat ("2. View information on algorithm \n")
  #   cat ("3. View package dependency and version information for R \n")
  # }
  
  if (response == 2){
    cat("=============================================================================\n")
    cat ("The algorithm used here is a hybrid of the simulated annealing, evolutionary algorithms\n 
and the ARC3 algorithm. The basic form of the algorithm is as follows: \n
1.	Initialise population using random draws from a state-space that is reduced from all possibilities by partial application of the ARC3 algorithm. \n
2.	Evolve the population according to the fitness criteria by randomly mutating cells and/or transposing entire rows from other organisms.\n
3.	After a number of generations, this results in a convergence on a local minima and an invariant population. Once convergence has stalled, randomly choose between reinitialising the bottom 95% of the population (measured by fitness criterion) or simulated annealing.\n
\nA chart showing convergence is generated in deSilva.pdf in this directory with results. \n
=============================================================================\n
")
    cat ("Would you like further information about the algorithm? Y/N \n")
    response2 <-readLines(file("stdin"),1)
    on.exit(close(response2))
    cat("\n=============================================================================\n")
    if (response2 == "Y" | response2 == "y"){
      cat ("The algorithm attempts to evolve a solution to the Einstein problem. The organisms are a 5x5 matrix with a column for nationalities, house colours, pets, drinks and cigarettes. The row position of an item indicates the house it belongs to.\n
         The challenge I set for myself here was not to allow the algorithm to “view” the answer, not even by proxy through the fitness criterion. The fitness criterion is measured only by the clues given. Further information about the fitness criterion is given below.\n
         The simplex algorithm is the obvious choice for this problem, as an econometrician I’m already quite familiar with it. Cedric challenged me to give something different like an EA a go. That seemed like a lot more fun than reliving my undergraduate assignments.\n\n
         So here we are.\n
         ")
    }
    cat("\n=============================================================================\n")
    cat("Would you like further information about the ARC3 stage? Y/N \n")
    response3 <-readLines(file("stdin"),1)
    on.exit(close(response3))
    cat("\n=============================================================================\n")
    if (response3 == "Y" | response3 == "y"){
      cat("This stage uses a simplified ARC3 algorithm, which takes all possibilities in the problem and reduces the state spaces by application of some of the constraints. This is used to initialise the population with a set of organisms that is closer to the desired solution than random chance alone would generate.
      This was useful for two reasons: it made convergence of the algorithm faster and improved the result.\n
        ")
    }
    cat("\n=============================================================================\n")
    cat("Would you like further information about the Evolutionary Algorithm stage? Y/N")
    response4 <-readLines(file("stdin"),1)
    on.exit(close(response4))
    cat("\n=============================================================================\n")
    if (response4 == "Y" | response4 == "y"){
      cat("After the population is intitialised, it’s tested for fitness and a challenger organism is created from the “winning” organism. This is a basic tourney-type algorithm.\n
        The challenger organisms are evolved in two ways: \n
        ")
      cat("1.	Transposition. This is set to occur at the outset of the algorithm with a probability of 0.5. The maximum number of rows that can be swapped is two. The probability of transposition is a monotonically decreasing function of the number of generations passed. The number of rows to be transposed is random, but has an upper maximum that is also a monotonically decreasing function of the number of generations passed, capped at a minimum of 1.
\n")
      cat("2. Mutation. The probability of mutation is set at the outset of the algorithm at 0.9, but is also a monotonically decreasing function. The number of cells to mutate is randomised, set at the beginning of the algorithm between 10-20 cells, but the number of cells is also a monotonically decreasing function of the number of generations.\n")
      cat("\n")
      cat("The mutated/transposed challenger organism is tested for fitness. It’s added to the population and the least fit organism removed before moving onto the next generation.\n
        Why allow these parameters to change? \nAt the beginning of the algorithm, large mutations/transpositions regularly get the solution closer to the desired state, faster. However, as the generations increase, large mutations do not improve fitness as the organism is already close to the global minima. Therefore by tapering this type of evolution, the algorithm can efficiently approach the minima.\n
        ")
    }
    cat("\n=============================================================================\n")
    cat("Would you like further information about the simulated annealing/reinitialisation stage? Y/N \n")
    response5 <-readLines(file("stdin"),1)
    on.exit(close(response5))
    cat("\n=============================================================================\n")
    if (response5 == "Y" | response5 == "y"){
      cat("After a number of generations of ordinary EA, the population becomes invariant as less-fit organisms are culled in each generation. Unfortunately, the population converges on a local minima. Once the population has stalled in improvement for a designated number of generations (500 in this case) I dealt with this in two ways. These were chosen with a probability of 0.5 each:
\n")
      cat("1.	Reinitialise the bottom 95% of the population by fitness. I culled the population up to the 95th percentile in fitness and replaced with a random reinitialisation from the ARC3 set.
\n")
      cat("2. Switch to simulated annealing. Rather than attempt to mutate the challenger organisms using other elements of an invariant population, in this stage, we used the fitness function to find the column of the organism most “unfit” and randomly swap cells within that column. This avoids attempting to mutate columns with good fitness or introducing doubled-up elements e.g. two cells with the same nationality. This results in a more efficient algorithm at this stage.")
      cat("\n")
    }
    cat("\n=============================================================================\n")
    cat("Would you like further information about the fitness criterion chosen? Y/N \n")
    response6 <-readLines(file("stdin"),1)
    on.exit(close(response6))
    cat("\n=============================================================================\n")
    if (response6 == "Y" | response6 == "y"){
      cat("This was an awkward problem for designing a fitness criterion, so I tried a bunch of things to see what worked. Some things I tried include:\n
\n")
      cat("1. A simple “add up all the constraints that are violated” function. This worked OK, but prioritised double-ups of cells, e.g. multiple Brits or cats.")
      cat("2. I added a simple addendum to (1) which added 5 – unique(cells) in each column. That is, for a column that was entirely unique elements would add 0. A column which had five “Brits” would add 4. A combination of (1) and (2) formed the basis for most of the testing that followed.")
      cat("3. I also tested quadratic terms (in the vein of the sum of squares type fitness functions), however while these provided good coverage away from the solution, as it approached the global minima, coverage wasn’t any better than the linear version.")
      cat("4. I expanded the fitness function based on (1) and (2) by summing the errors in constraints across columns. This allowed for a targeted approach in the simulated annealing stage.")
      cat("5. I tried the logistic regression trick, e.g. taking the fitness function as exp(1 + fitness), however like the quadratic version, it gave great coverage away from the solution where I didn’t need it and made it difficult to distinguish closer to the global minima.")
      cat("\n\n Ultimately a simple combination of (1) and (2) with use of (4) at later stages provided the best outcomes on testing.
\n")
      cat("A fitness function with lots of weird corners and undifferntiable bits was a big challenge.")
    }
    cat("\n=============================================================================\n")
    cat("Would you like further information about how I decided the values for the parameters used? Y/N
\n")
    response7 <-readLines(file("stdin"),1)
    on.exit(close(response7))
    cat("\n=============================================================================\n")
    if (response7 == "Y" | response7 == "y"){
      cat("Put simply, I’m an econometrician: I collect data and I test it. So I ran the variations of the algorithm a whole bunch of times and I worked out where there was signal and where there was noise. (I tested with a few different seeds to see which parts were random noise!)
\n")
    }
  } else if (response == 3) {
    cat ("\n-----------------------------------------------------------------\n")
    cat ("Libraries required: \n")
    cat ("abind\n")
    cat ("ggplot2\n")
    cat ("R version: 3.1\n")
    cat ("This program constructed on: x86_64-apple-darwin13.4.0\n")
    cat ("\n-----------------------------------------------------------------\n")
  } else if (response == 1) {
    cat("\n=============================================================================\n")
    cat ("Please wait. Anticipated run time is around 2 minutes on my macbook.\n")
    cat ("5000 generations will be evolved, but the simulated annealing takes a bit of time.\n")
    cat ("This should take less than five minutes. Sorry.\n")
    cat ("Fun fact to keep you entertained: when I was a Ph.D. student in Econometrics you had 'good code' ")
    cat ("if it took days to run. We were good at economics, less so at code.\n")
    cat("\n=============================================================================\n")
    start_time <- Sys.time() # time the algorithm
    
    #############################################################
    # 3. Define Functions in the Algorithm
    #############################################################
    
    # CONSTRAINT FUNCTIONS  
    # Constraint1
    # The Brit lives in the red house.
    constraint1 <- function(x){
      flag <- TRUE
      # The Brit lives in the red house.
      index <- which(x[,1] == "Brit")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,2] != "red" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 2
    # The Swede keeps dogs as pets.
    constraint2 <- function(x){
      flag <- TRUE
      index <- which(x[,1] == "Swede")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,3] != "dog" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint3
    # The Dane drinks tea.
    constraint3 <- function(x){
      flag <- TRUE
      index <- which(x[,1] == "Dane")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,4] != "tea" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 4
    # The green house is on the immediate left of the white house.
    constraint4 <- function(x){
      flag <- TRUE
      index1 <- which(x[,2] == "green")
      index2 <- which(x[,2] == "white")
      if (length(index1) != 1 | length(index2) != 1){
        flag <- FALSE
      } else { 
        if (index2 - index1 != 1 | index1 == 0 |index2 == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 5
    # The green house's owner drinks coffee.
    constraint5 <- function(x){
      flag <- TRUE
      index <- which(x[,2] == "green")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,4] != "coffee" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # The owner who smokes Pall Mall rears birds.
    # constraint 6
    constraint6 <- function(x){
      flag <- TRUE
      index <- which(x[,5] == "pall mall")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,3] != "bird" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 7
    # The owner of the yellow house smokes Dunhill.
    constraint7 <- function(x){
      flag <- TRUE
      index <- which(x[,2] == "yellow")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,5] != "dunhill" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 8
    # The owner living in the centre house drinks milk.
    constraint8 <- function(x){
      flag <- TRUE
      if(x[3,4] != "milk"){
        flag <- FALSE
      }
      return(flag)
    }
    
    # constraint 9
    # The Norwegian lives in the first house.
    constraint9 <- function(x){
      flag <- TRUE
      index <- which(x[,1] == "Norwegian")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (index != 1 | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 10
    # The owner who smokes Blends lives next to the one who keeps cats.
    constraint10 <- function(x){
      flag <- TRUE
      index1 <- which(x[,5] == "blends")
      index2 <- which(x[,3] == "cat")
      if (length(index1) != 1 | length(index2) != 1){
        flag <- FALSE
      } else { 
        if (abs(index2 - index1) != 1 | index1 == 0 |index2 == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    #constraint 11
    # The owner who keeps the horse lives next to the one who smokes Dunhill.
    constraint11 <- function(x){
      flag <- TRUE
      index1 <- which(x[,3] == "horse")
      index2 <- which(x[,5] == "dunhill")
      if (length(index1) != 1 | length(index2) != 1){
        flag <- FALSE
      } else { 
        if (abs(index2 - index1) != 1 | index1 == 0 |index2 == 0){
          flag <- FALSE
        }
      }
      return(flag)
      
    }
    # constraint 12
    # The owner who smokes Bluemasters drinks beer.
    constraint12 <- function(x){
      flag <- TRUE
      index <- which(x[,5] == "bluemasters")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,4] != "beer" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    # constraint 13
    # The German smokes Prince.
    constraint13 <- function(x){
      flag <- TRUE
      index <- which(x[,1] == "German")
      if (length(index) != 1){
        flag <- FALSE
      } else { 
        if (x[index,5] != "prince" | index == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    # constraint 14
    # The Norwegian lives next to the blue house.
    constraint14 <- function(x){
      flag <- TRUE
      index1 <- which(x[,1] == "Norwegian")
      index2 <- which(x[,2] == "blue")
      if (length(index1) != 1 | length(index2) != 1){
        flag <- FALSE
      } else { 
        if (abs(index2 - index1) != 1 | index1 == 0 |index2 == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    # constraint 15
    # The owner who smokes Blends lives next to the one who drinks water
    constraint15 <- function(x){
      flag <- TRUE
      index1 <- which(x[,5] == "blends")
      index2 <- which(x[,4] == "water")
      if (length(index1) != 1 | length(index2) != 1){
        flag <- FALSE
      } else { 
        if (abs(index2 - index1) != 1 | index1 == 0 |index2 == 0){
          flag <- FALSE
        }
      }
      return(flag)
    }
    
    
    # fitness function
    fit1 <- function(x, fitSel){
      
      fitness <- numeric(length = ncol(x))
      # Does each column have a unique set?
      for (i in 1:ncol(x)){
        fitness[i] <- fitness[i] + (5-length(unique(x[,i])))^2
      }
      
      # The Brit lives in the red house.
      if (constraint1(x) == FALSE){
        fitness[1] = fitness[1] + 1
        fitness[2] = fitness[2] + 1
      }
      # The Swede keeps dogs
      if(constraint2(x) == FALSE){
        fitness[1] = fitness[1] + 1
        fitness[3] = fitness[3] + 1
      }
      # The Dane drinks tea.
      if(constraint3(x) == FALSE){
        fitness[1] = fitness[1] + 1
        fitness[4] = fitness[4] + 1
      }
      # The green house is on the immediate left of the white house.
      if(constraint4(x) == FALSE){
        fitness[2] = fitness[2] + 2
      }
      # The green house's owner drinks coffee.
      if(constraint5(x) == FALSE){
        fitness[2] = fitness[2] + 1
        fitness[4] = fitness[4] + 1
      }
      # The owner who smokes Pall Mall rears birds.
      if(constraint6(x) == FALSE){
        fitness[5] = fitness[5] + 1
        fitness[3] = fitness[3] + 1
      }
      # The owner of the yellow house smokes Dunhill.
      if(constraint7(x) == FALSE){
        fitness[2] = fitness[2] + 1
        fitness[5] = fitness[5] + 1
      }
      # The owner living in the centre house drinks milk.
      if(constraint8(x) == FALSE){
        fitness[4] = fitness[4] + 2
      }
      # The Norwegian lives in the first house.
      if(constraint9(x) == FALSE){
        fitness[1] = fitness[1] + 2
      }
      # The owner who smokes Blends lives next to the one who keeps cats.
      if(constraint10(x) == FALSE){
        fitness[5] = fitness[5] + 1
        fitness[3] = fitness[3] + 1
      }
      # The owner who keeps the horse lives next to the one who smokes Dunhill.
      if(constraint11(x) == FALSE){
        fitness[3] = fitness[3] + 1
        fitness[5] = fitness[5] + 1
      }
      # The owner who smokes Bluemasters drinks beer.
      if(constraint12(x) ==  FALSE){
        fitness[5] = fitness[5] + 1
        fitness[4] = fitness[4] + 1
      }
      # The German smokes Prince.
      if(constraint13(x) == FALSE){
        fitness[1] = fitness[1] + 1
        fitness[5] = fitness[5] + 1
        
      }
      
      # The Norwegian lives next to the blue house.
      if(constraint14(x) == FALSE){
        fitness[1] = fitness[1] + 1
        fitness[2] = fitness[2] + 1
      }
      
      # The owner who smokes Blends lives next to the one who drinks water
      if(constraint15(x) == FALSE){
        fitness[4] = fitness[4] + 1
        fitness[5] = fitness[5] + 1
      }
      ### We can describe this in a few ways
      if (fitSel < 3){
        fitness_tuple <- c(sum(fitness),  t(fitness)%*%fitness)
      } else {
        fitness_tuple <- fitness
      }
      return(fitness_tuple)
    }
    
    ## A quicker solution if we use a state space constrained by an ARC3
    ## algorithm to initialise problem.
    
    constraints <- function(){
      constraints <- {}
      constraints$C1 <- c("Brit", "red", NA, NA, NA)
      constraints$C2 <- c("Swede", NA, "dog", NA, NA)
      constraints$C3 <- c("Dane", NA, NA, "tea", NA)
      constraints$C4 <- c(NA, "green", NA, "coffee", NA)
      constraints$C5 <- c(NA, NA, "bird", NA, "pall mall")
      constraints$C6 <- c(NA, "yellow",  NA, NA, "dunhill")
      constraints$C10<- c(NA, NA, NA, "beer", "bluemasters")
      constraints$C11 <- c("German", NA, NA, NA, "prince")
      adjacency <- expand.grid(nationalities, colours, pets, drinks, cigarettes, stringsAsFactors = FALSE)
      colnames(adjacency) <- c("nationalities", "colours", "pets", "drinks", "cigarettes")
      
      for (c in constraints){
        index <- which(is.na(c) == FALSE)
        index1 <- index[1]
        index2 <- index[2]
        reduced_index <- which(!(adjacency[,index1] == c[index1] & adjacency[, index2] != c[index2]))
        adjacency <- adjacency[reduced_index,]
        reduced_index <- which(!(adjacency[,index1] != c[index1] & adjacency[, index2] == c[index2]))
        adjacency <- adjacency[reduced_index,]
      }
      
      ## negative unary constraints
      # The owner who smokes Blends lives next to the one who keeps cats.
      # therefore blends can't have cats
      
      negconstraints <- {}
      negconstraints$D1 <- c(NA, NA, "cat", NA, "blends")
      
      # The owner who keeps the horse lives next to the one who smokes Dunhill.
      negconstraints$D2 <- c(NA, NA, "horse", NA, "dunhill")
      # The Norwegian lives next to the blue house.
      negconstraints$D3 <- c("Norwegian", "blue", NA, NA, NA)
      # The owner who smokes Blends lives next to the one who drinks water.
      negconstraints$D4 <- c(NA, NA, NA, "water", "blends")
      
      for (c in negconstraints){
        index <- which(is.na(c) == FALSE)
        index1 <- index[1]
        index2 <- index[2]
        reduced_index <- which(!(adjacency[,index1] == c[index1] & adjacency[, index2] == c[index2]))
        adjacency <- adjacency[reduced_index,]
      }
      return(adjacency)
    }
    # generate the reduced adjacency matrix from ARC3 algorithm
    adjacency <- constraints()
    
    ## initialize population 
    initialisation_func <- function(initialisation, popsize, fitSel, x, adjacency){
      pop <- array(data = NA, dim = c(5, 5, popsize))
      for (i in 1: popsize){
        if (initialisation == 0){
          pop_Index <- floor(runif(5, min = 1, max = nrow(x) + 1))
          pop[1:5,,i] <- as.matrix(x[pop_Index,])
        } else if (initialisation == 1){
          temp <- nrow(adjacency)
          pop_Index <- sample(1:temp, 5)
          pop[1:5,,i] <- as.matrix(adjacency[pop_Index,])
        }
      }
      return(pop)
    }
    
    ## Find current winner function
    
    find_winner <- function(pop, fit){
      current_winner <- which(fit == min(fit))
      if (length(current_winner) > 0){
        current_winner <- current_winner[1]
      }
      challenger <- pop[,,current_winner]
      return(challenger)
    }
    
    ## fitness of challenger
    
    fit_challenger <- function(challenger, fitSel){
      fit_tuple <- fit1(challenger, fitSel)
      fitchal=fit_tuple[fitSel]
      return(fitchal)
    }
    
    ## add challenger to population, remove worst performer
    grow_population <- function(pop, challenger, fit, fitchal){
      pop <- abind(pop, challenger, along = 3)
      fit <- c(fit, fitchal)
      index <- order(fit)
      lengthIndex <- length(index)
      fit <- fit[-index[lengthIndex]]
      pop <- pop[,,-index[lengthIndex]]
      return(pop)
    }
    
    grow_fitness <- function(fit, fitchal){
      fit <- c(fit, fitchal)
      index <- order(fit)
      lengthIndex <- length(index)
      fit <- fit[-index[lengthIndex]]
      return(fit)
    }
    
    
    
    #############################################################
    # 4. Define and run the algorithm
    #############################################################
    
    
    countImprov <- 0
    # initialise population
    pop <- initialisation_func(initialisation, popsize, fitSel,x, adjacency)
    # calculate fitness of initial population
    fit=numeric(popsize)
    for (i in 1:popsize){
      fit_tuple <- fit1(pop[,,i], fitSel)
      fit[i] <- fit_tuple[fitSel]
    }
    
    index <- order(fit)
    
    
    # plot fitness evolution
    chart_storage <- as.data.frame(matrix(data = 0, nrow = (numgen + 1)*2, ncol = 3))
    colnames(chart_storage) <- c("index", "minimum_fit", "panel")
    chart_storage[1, 2] <- (min(fit))
    chart_storage[1,1] <- (0)
    chart_storage[1,3] <- "Population Min. Fitness"
    chart_storage[numgen + 2, 1] <- (0)
    chart_storage[numgen + 2, 2] <- (var(fit))
    chart_storage[numgen + 2, 3] <- "Population Fitness Var."
    converge=numeric(numgen) # hold population variance during run - for convergence diagnostics
    noImprov=numeric() # hold last time fitness improved
    
    for (i in 1:numgen)
    {
      if (min(fit) == 0){ # if solution has been reached, stop.
        break
      }
      
      if (i %% 1000 == 0){
        current_time <- Sys.time()
        cat("\n=============================================================================\n")
        cat ("Number of generations passed: ", i, "\n")
        cat ("Number of generations to go: ", numgen - i, "\n")
        cat ("Current Fitness Level: ", min(fit))
        cat("\n=============================================================================\n")
        
      }
      
      CR_i <- CR/sqrt(i/1000) # reduce cross overs + mutations as # of gens increase
      if (i > numgen/10){
        rowCross_i <- 1
      } else {
        rowCross_i <- rowCross
      }
      
      mutability_i <- mutability
      minMutate_i <- minMutate
      maxMutate_i <- floor(maxMutate/sqrt(i/1000))
      if (minMutate > maxMutate_i){
        minMutate_i <- 1
      }
      
      # can't mutate like we would in a continuous state space
      # need a little creativity here
      # Crossovers -> take up to #rowCross rows from successive organisms
      
      challenger <- find_winner(pop, fit)
      if (runif(1, min = 0, max = 1) < CR_i){
        perturb_row <- floor(runif(rowCross_i, min = 1, max = (nrow(challenger) + 1)))
        for (k in 1:rowCross_i){
          replace_organism <- which(fit == sort(fit)[k])
          if (length(replace_organism) > 1){
            replace_organism <- replace_organism[1]
          }
          replacement <- pop[,,replace_organism]
          challenger[perturb_row[k],] <- replacement[perturb_row[k],]
        }
      }
      # number of cells to mutate
      if (runif(1, min = 0, max = 1) < mutability_i){
        num_mutate <- floor(runif(1, min = minMutate_i, max = (maxMutate_i + 1)))
        for (j in 1:num_mutate){
          mutate_cell <- floor(runif(2, min =1 , max = (nrow(challenger) + 1)))
          temp <- challenger[mutate_cell[1], mutate_cell[2]]
          index3 <- 5 - mutate_cell[1]
          if (index3 != 0){
            challenger[mutate_cell[1], mutate_cell[2]] <- challenger[index3, mutate_cell[2]]
            challenger[index3, mutate_cell[2]] <- temp
          }
        }
      }
      
      
      # get fitness for challenger
      fitchal <- fit_challenger(challenger, fitSel)
      # add the challenger to the population, sort and bump off the last
      pop <- grow_population(pop, challenger, fit, fitchal)
      fit <- grow_fitness(fit, fitchal)
      chart_storage[i+1, 2] <- min(fit)
      chart_storage[i+1,1] <- i
      chart_storage[i+1,3] <- "Population Min. Fitness"
      chart_storage[numgen + i+2, 1] <- i
      chart_storage[numgen + i+2, 2] <- var(fit)
      chart_storage[numgen + i+2, 3] <- "Population Fitness Var."
      
      
      if (min(fit) == 0){ # if solution has been reached, stop
        break
      }
      
      if(fitchal<min(fit)){
        noImprov=c(noImprov,countImprov) # count number of generations since last improvement
        countImprov=1
      } else {
        countImprov=countImprov+1
      }
      if (countImprov >= boundary1){        #algorithm is stuck! Two strategies to bump out of local minima
        # strategy 1: reinitialise the bottom -performing 95% of population 
        # this allows for diversity to continue
        # strategy 2: simulated annealing
        if (runif(1,0,1) < 0.5){                                
          fifth <- floor(quantile(1:popsize, 0.05))
          redraw_pop <- initialisation_func(initialisation, length(fifth:popsize), fitSel,x, adjacency)
          index <- order(fit)
          length_index <- length(fit)
          pop[,,index[fifth: length_index]] <- redraw_pop
          fit <- numeric(popsize)
          for (j in 1:popsize){
            fit_tuple <-fit1(pop[,,j], fitSel)
            fit[j] <- fit_tuple[fitSel]
          }
        } else {
          while(temperature > 0.00000001)
          {
            for (k in 1:5)
            {
              current_winner <- which(fit == min(fit))
              if (length(current_winner) > 0){
                current_winner <- current_winner[1]
              }
              challenger <- pop[,,current_winner]
              # choose a cell to mutate -> swap within the challenger organism, not outside it
              fit_tuple <- fit1(challenger, 3)
              col_to_mutate <- which(fit_tuple == max(fit_tuple))
              if (length(col_to_mutate) > 0){
                choose <- sample(1:length(col_to_mutate),1)
                col_to_mutate <- col_to_mutate[choose]
              }
              index <- sample(1:5, 2, replace = FALSE)
              temp <- challenger[index[1], col_to_mutate]
              challenger[index[1], col_to_mutate] <- challenger[index[2], col_to_mutate]
              challenger[index[2], col_to_mutate] <- temp
              fitchal=0
              fit_tuple <- fit1(challenger, fitSel)
              fitchal=fit_tuple[fitSel]
              if (fitchal<min(fit)) {
                countImprov <- 1
                pop <- grow_population(pop, challenger, fit, fitchal)
                fit <- grow_fitness (fit, fitchal)
                chart_storage[i+1, 2] <- min(fit)
                chart_storage[i+1,1] <- i
                chart_storage[i+1,3] <- "Population Min. Fitness"
                chart_storage[numgen + i+2, 1] <- i
                chart_storage[numgen + i+2, 2] <- var(fit)
                chart_storage[numgen + i+2, 3] <- "Population Fitness Var."
               
                   
              } else {
                swap=exp((-min(fit)/fitchal)/temperature)
                if(is.nan(swap)) swap=0
                if(swap<runif(1)){
                  pop <- grow_population(pop, challenger, fit, fitchal)
                  fit <- grow_fitness (fit, fitchal)
                  chart_storage[i+1, 2] <- min(fit)
                  chart_storage[i+1,1] <- i
                  chart_storage[i+1,3] <- "Population Min. Fitness"
                  chart_storage[numgen + i+2, 1] <- i
                  chart_storage[numgen + i+2, 2] <- var(fit)
                  chart_storage[numgen + i+2, 3] <- "Population Fitness Var."
                  
                  
                }
              }
            }
            
            if (min(fit) == 0){
              break
            }
            temperature=temperature*cool
          }
        }
      }
      if (min(fit) == 0){ # if solution has been found, break
        break
      }
    }
    #############################################################
    # 5. Algorithm Outcomes
    #############################################################
    
    # get best solution
    index=which(fit==min(fit))[1]
    cat ("\n===================================================================\n")
    cat ("Algorithm complete!\n")
    cat ("\n===================================================================\n")
    cat ("Current solution: \n")
    print(pop[,,index])
    chart <- ggplot(chart_storage) +
      labs(x="Generation", y="Fitness Criterion") +
      facet_grid(panel~., scale="free")+
      geom_point(aes(index, minimum_fit, colour="Population"), alpha = 0.3) +
      geom_hline(yintercept= 0, colour="slategrey")+
      theme_light()+
      scale_colour_manual(name="Fitness Criterion\n at each generation",values=c("#0086BF" ,"#002B49", "#3F4444")) +
      theme(legend.position="bottom")+
      theme(plot.margin = unit(c(1,1,1,1), "lines"))+
      ggtitle("Algorithm Convergence")
    pdf(file="desilva.pdf") # code from https://unix.stackexchange.com/questions/190337/how-can-i-make-a-graphical-plot-of-a-sequence-of-numbers-from-the-standard-input
    print(chart)
    dev.off()
    cat ("\n===================================================================\n")
    cat ("Fitness level:", min(fit), "\n")
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    cat ("Time taken: ", time_taken, " \n")
    cat ("\n===================================================================\n")
    cat ("The solution evolved:\n")
    solution <- which(pop[,3,index] == "fish" )
    owner <- pop[solution,1,index]
    cat ("The ",  owner, " owns the fish.\n")
    cat ("I hope it's the German.\n")
    cat ("Note: in this directory is a file 'deSilva.pdf', this will give a chart of the convergence.")
    cat ("\n===================================================================\n")
    #############################################################
    # 6. Close.
    #############################################################
    
  } else if (response == 4){
    cat ("\n===================================================================\n")
    cat ("Thankyou, have a nice day!")
    cat ("\n===================================================================\n")
  }
} # close while loop