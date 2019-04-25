GET_CHROMOSOME <- function(Interval)
{
  Chromosome <- c()
  for (i in Interval)
  {
    Valid_gene <- RECURSIVE(i)
    Chromosome <- append(Chromosome,Valid_gene) 
  }
  return(Chromosome)
}

RECURSIVE <- function(i){
  sample_value <- sample(1:15,1)
  sample_binary <- decimal2binary(sample_value,4)
  count1 <- length(which(sample_binary == 1))
  if (count1 == 1 & i == count1 ){
    return(sample_binary)
  }
  else if (i == count1 & (CHECK_INTERVAL(sample_binary))){
    Valid_interval <- sample_binary
    return(sample_binary)
  }
  else{
    RECURSIVE(i)
  }
}

CHECK_INTERVAL <- function(sample_binary)
{
  output_value <- FALSE
  for (i in 1:length(sample_binary)) {
    if((i+1 <= 4) & (sample_binary[i] == 1) & (sample_binary[i+1] == 1))
    {
      output_value <- TRUE
      return(output_value)
    }
  }
  return(output_value)
}

# Ref: https://github.com/cran/GA/blob/master/R/genope.R
#gabin_Population_R
Initial_Population <- function(object)
{
  Interval <- c(2,2,1,1,1,1,1)
  
  pop <- matrix(as.double(NA), 
                nrow = object@popSize, 
                ncol = object@nBits)
  
  for (i in 1:nrow(pop))
  {
    t <- GET_CHROMOSOME(Interval)
    pop[i,] <- t
  }
  
  return(pop)
}

# Ref: https://github.com/cran/GA/blob/master/R/genope.R
# ga_spCrossover_R 
crossover <- function(object, parents)
{
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  
  #GA Takes any random sample crossover point, which breaks the 
  #constraint hence need to change crossover point according to
  # gene size (new crossover points: 4,8,12,16,20,24,28)
  gene_bits <- 4
  crossOverPoint <- sample(seq(gene_bits,n,gene_bits), size = 1)
  
  if(crossOverPoint == n) 
  { children <- parents
  fitnessChildren <- fitness }
  else 
  { children[1,] <- c(parents[1,1:crossOverPoint],
                      parents[2,(crossOverPoint+1):n])
  children[2,] <- c(parents[2,1:crossOverPoint],
                    parents[1,(crossOverPoint+1):n])
  }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}

# Ref: https://github.com/cran/GA/blob/master/R/genope.R
# gabin_raMutation_R 
mutation <- function(object, parent)
{
  parent <- as.vector(object@population[parent,])
  n <- length(parent)
  gene_bits <- 4
  #matrix(v,nrow=7,ncol=4,byrow = T)
  parent_matrix <- matrix(parent,
                          nrow=n/gene_bits,
                          ncol=gene_bits,
                          byrow = T)
  j <- sample(1:nrow(parent_matrix), size = 1)
  
  mutation_parent_gene = parent_matrix[j,]
  count1 <- length(which(mutation_parent_gene == 1))
  mutation_child_gene <- RECURSIVE(count1)
  
  while(all(mutation_parent_gene==mutation_child_gene))
  {
    mutation_child_gene <- RECURSIVE(count1)
  }
    parent_matrix[j,] <- mutation_child_gene
    mutate <- as.vector(t(parent_matrix))
  
  return(mutate)
}
