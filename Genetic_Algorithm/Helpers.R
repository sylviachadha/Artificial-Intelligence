RANDOM_SELECTION <- function(population)
{
  selected_parent <- FITNESS_FUNCTION(population)
  return(selected_parent)
}

FITNESS_FUNCTION <- function(population)
{
  selection <- population
  selection$fitness_ratio <- FITNESS_RATIO(selection$domain_vector)

  selection$roulette_values <- GET_ROULETTE_VALUES(selection$fitness_ratio)
  selection$crossover_probability <- runif(n = nrow(selection), min = 0, max = 1)

  mating_pool <- GET_MATING_POOL(selection)
  return_parent <- sample(mating_pool,2)

  return(return_parent)
}

CROSSOVER <- function(dataframe,parent1,parent2,crossover_point)
{
  parent1_filter <- dataframe$chromosome_label %in% parent1
  parent2_filter <- dataframe$chromosome_label %in% parent2
  parent1_chromosome <- unlist(dataframe[parent1_filter,3])
  
  parent2_chromosome <- unlist(dataframe[parent2_filter,3])
  
  crossover_probability <- runif(n =1, min = 0, max = 1)
  child <- c()

  if(crossover_probability >= crossover_point)
  {
    child <- parent1_chromosome
  }
  else
  {
    for(i in 1:length(parent1_chromosome))
    {
      if(i<=crossover_point)
      {
        child <- append(child,parent1_chromosome[i])
      }
      else
      {
        child <- append(child,parent2_chromosome[i])
      }
    }
  }
  
  return(child)
}

MUTATE <- function(child,mutation_bit,mutation_value)
{
  mutation_probability <- runif(n =1, min = 0, max = 0.01)
  
  if(mutation_value < mutation_probability)
  {
    ifelse((child[mutation_bit] == 0),
           child<- replace(child,mutation_bit,1),
           child<-replace(child,mutation_bit,0))
    return(child)
  }
  else
  {
    return(child)
  }
}

GET_ROULETTE_VALUES  <- function(fitness_ratio)
{
  temp <- 0
  result <- c()
  
  for (i in fitness_ratio)
  {
    next_value <- i + temp
    result <- append(result, next_value)
    temp <- next_value
  }
  return(result)

}

GET_MATING_POOL <- function(dataframe)
{
  temp <- 0
  mating_pool <- c()
  for(i in 1:nrow(dataframe))
  {
    #Rotating wheel n times (n=size of population)
    rotation_value <- sample(100,1)
    chromosome <- GET_CHROMOSOME_NAME(dataframe,rotation_value)
    mating_pool <- append(mating_pool,chromosome)
  }
  
  return(unique(mating_pool))
}

GET_CHROMOSOME_NAME <- function(dataframe,rotation_value)
{
  
  temp <- 0
  for(i in 1:nrow(dataframe))
  {
    if((rotation_value > temp) & (rotation_value < dataframe[i,5]))
    {
      return(as.character(dataframe[i,2]))
    }
    temp <- dataframe[i,5]
  }
  
}

FITNESS_RATIO <- function(x)
{
  fitness_value = (15*x - (x*x))
  fitness_sum = sum(fitness_value)
  fitness_ratio = ((fitness_value/fitness_sum)*100)
  return(fitness_ratio)
}

DEC_BIN_LIST <- function(start_range,end_range,bnary_value_length=4)
{
  Dec_Bin_list <- list()
  for (i in start_range:end_range)
  {
    Dec_Bin_list[[toString(i)]] <- decimal2binary(i,bnary_value_length)
  }
  return(Dec_Bin_list)
}

SAMPLE_FROM_DF = function(df,n){
  return(df[sample(nrow(df),n),])
}
