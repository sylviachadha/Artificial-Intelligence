source("Helpers.R",echo = T)

GENETIC_ALGORITHM <-
  function(input_population,input_max_fitness)
  {
    fitness_value <- 0 
    while(fitness_value < input_max_fitness)
    {
      child <- list()
      new_population <- c()
      chromosome_label <- c()
      domain_vector <- c()
      
      for(i in 1:nrow(input_population))
      {
        x <- RANDOM_SELECTION(input_population)[1]
        y <- RANDOM_SELECTION(input_population)[2]
        
        crossover_location <- sample(2:3,1)
        new_population <- CROSSOVER(input_population,x,y,crossover_location)

        crossover_location <- sample(2:3,1)
        mutation_bit <- sample(1:4,1)

        mutation_value <- 0.1
        new_population <- MUTATE(new_population,mutation_bit,mutation_value)
        child[[toString(i)]] <- new_population

        chromosome_label <- append(chromosome_label,paste0(x,y))
        domain_vector <- append(domain_vector,
                                         binary2decimal(new_population))
      }
      
      remove(input_population)
      input_population <- data.frame(domain_vector,
                               chromosome_label)
      input_population$chromosome_values <- child

      fitness_value <- fitness_value + 1;
      
      remove(child,new_population,chromosome_label,domain_vector)
      
    }
  }