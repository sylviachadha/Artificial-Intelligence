library("frbs")
setwd("/Users/sylvia/Documents/Workspace/R/Fuzzy")
data <- read.csv(file="./data/visualizing_soil.csv", header=TRUE, sep=",")

## Shuffle the data
## then split the data to be training and testing datasets
soilShuffled <- data[sample(nrow(data)), ]
soilShuffled[, 5] <- unclass(soilShuffled[, 5])
tra.soil <- soilShuffled[1 : 8500, ]
tst.soil <- soilShuffled[8501 : nrow(soilShuffled), 1 : 4]
real.soil <- matrix(soilShuffled[8501 : nrow(soilShuffled), 5], ncol = 1)

head(tra.soil)

## Define range of input data. Note that it is only for the input variables.
range.data.input <- apply(data[, -ncol(data)], 2, range)

range.data.input

## Set the method and its parameters. In this case we use FRBCS.W algorithm
method.type <- "FRBCS.W"
control <- list(num.labels = 7, type.mf = "GAUSSIAN", type.tnorm = "MIN",
                type.snorm = "MAX", type.implication.func = "ZADEH") 


## Learning step: Generate fuzzy model
object.cls <- frbs.learn(tra.soil, range.data.input, method.type, control)

## Predicting step: Predict newdata
res.test <- predict(object.cls, tst.soil)

## Display the FRBS model
result <- summary(object.cls)

result$rule

## Plot the membership functions
plotMF(object.cls)

err = 100*sum(real.soil!=res.test)/nrow(real.soil)
acc = 100 - err

cat("Error: ",err,"\n") 
cat("Acc: ",acc,"\n")

