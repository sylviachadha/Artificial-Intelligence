library(GA)
source("Functions_Schedule.R")

GENE_SIZE <- 4
POPULATION <- 100
GENERATIONS <- 50
CHROMOSOME_BINARY_BITS <- 28

Unit_Members <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7")
Capacity <- c(20, 15, 35, 40, 15, 15, 10)
Interval <- c(2, 2, 1, 1, 1, 1, 1)
Max_Capacity <- sum(Capacity)

Time_Interval <- c("1", "2", "3", "4")
Load_Per_Time_Interval <- c(80, 90, 65, 70)

Fitness_Function <- function(chromosome)
{
  a <- matrix(chromosome,nrow=7,ncol=4,byrow = T)
  Net_Reserve_Interval <- c()
  for (i in 1:ncol(a))
  {
    loss <- sum((a[,i] * Capacity) )# Loss per interval
    load <- Load_Per_Time_Interval[i]
    Net_Reserve_Interval <- append(Net_Reserve_Interval,(Max_Capacity - loss - load))
  }
  Net_Reserve_Interval[Net_Reserve_Interval < 0] <- 0
  return(sum(Net_Reserve_Interval))
}

GA <- ga(
  type = "binary",
  fitness = Fitness_Function,
  nBits = CHROMOSOME_BINARY_BITS,
  popSize = POPULATION,
  population = Initial_Population,
  crossover  = crossover,
  mutation = mutation,
  maxiter = GENERATIONS,
  pcrossover = 0.7,
  pmutation = 0.01,
  run = 20
)
plot(GA)
sol <- summary(GA)
sol
Result <- as.vector(sol$solution[1,])
cat("Schedule_Result :",Result,"\n")

