library(keras)
library(tensorflow)
library(dplyr)

setwd("/Users/sylvia/Desktop")
#mydata <- read.csv(file.choose())
mydata <- read.csv(file = "iOS Device Data.csv")

selectdata1 = subset(mydata, activity == 1)
data1 <- head(selectdata1, n = 3000)

selectdata0 = subset(mydata, activity == 0)
data2 <- head(selectdata0, n = 3000)

selectdf <- bind_rows(data1, data2)
head(selectdf, n = 10)

#Randomly shuffle the data
selectdf <- selectdf[sample(nrow(selectdf)), ]
#Create 10 equally size folds
folds <- cut(seq(1, nrow(selectdf)), breaks = 10, labels = FALSE)
#Perform 10 fold cross validation
l <- list();

for (i in 1:10) {
  #Segement your data by fold using the which() function
  #rm(testIndexes,testData,trainData,x_train,y_train,model,history,x_test,y_test)
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- selectdf[testIndexes,]
  trainData <- selectdf[-testIndexes,]
  
  
  x_train <- as.matrix(trainData[, 1:6])
  y_train <- trainData[, 7]
  
  model <- keras_model_sequential()
  
  model %>% layer_dense(units = 10,
                        activation = 'relu',
                        input_shape = c(6)) %>%
    layer_dense(units = 10,
                activation = 'relu',
                input_shape = c(10)) %>%
    layer_dense(units = 1,
                activation = 'sigmoid')
  
  #summary(model)
  
  model %>% compile(
    optimizer = optimizer_adam(lr = 0.01),
    loss = 'binary_crossentropy',
    metrics = c('accuracy')
  )
  
  history <-
    model %>% fit(x_train, y_train, epochs = 100, batch_size = 50)

  x_test <- as.matrix(testData[, 1:6])
  y_test <- testData[, 7]
  
  #evaluate model on test data
  li <- model %>% evaluate(x_test, y_test)
  
  l[[i]] <- li
  
}
for (j in 1:10)
{
  cat("Fold:", j , ", loss:",l[[j]]$loss,",acc:",l[[j]]$acc,"\n")
}

