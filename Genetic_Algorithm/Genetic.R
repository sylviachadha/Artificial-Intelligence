source("Helpers.R",echo = T)
source("Algorithm.R", echo = T)

#Domain
start_range <- 1
end_range <- 15
binary_val_length <- 4
Size_of_population <- 5
Number_Of_Generations <- 3

crossover_probabilty <- 0.7

#Domain Count 
domain_vector <- c(start_range:end_range)
domain_vector

#Domain Chromosomes
chromosome_label <- paste0("X",seq(start_range:end_range))
chromosome_label

#Create domain dataframe
domain <- data.frame(domain_vector,
                                chromosome_label)
#Add Binary values to dataframe
decimal_binary_list <- DEC_BIN_LIST(start_range,
                                    end_range,
                                    binary_val_length)
domain$chromosome_values <- decimal_binary_list


#Get Sample from domain
population <- SAMPLE_FROM_DF(domain, Size_of_population)

## TODO Pass function as input


GENETIC_ALGORITHM(population,Number_Of_Generations)



