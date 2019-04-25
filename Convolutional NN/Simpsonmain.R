library(keras)

# List of Simpson characters to model
# Simpson_list <- c("Homer_simpson","Ned_flanders","Sideshow_bob",
#  "Abraham_grampa_simpson","Bart_simpson","Charles_montgomery_burns",
#  "Chief_wiggum")

# Number of Output classes (i.e.Simpson Characters) 
output_n <- 10

# Image size scale down for traning 
img_width <- 64
img_height <- 64
target_size <- c(img_width, img_height)

# RGB = 3 Channels
channels <- 3

# Define batch size and number of epochs
batch_size <- 32
epochs <- 20


# Path to image folders
train_image_files_path  <-
  "D:\\Sylvia\\Workspace\\Simpson\\train"
valid_image_files_path  <-
  "D:\\Sylvia\\Workspace\\Simpson\\valid"
test_image_files_path  <-
  "D:\\Sylvia\\Workspace\\Simpson\\test"

# Normalization and Data Augmentation on Training Data
train_data_gen <- image_data_generator(
  rescale = 1 / 255,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)

# Normalization, scale 0 to 1
valid_data_gen <- image_data_generator(rescale = 1 / 255)
test_data_gen <- image_data_generator(rescale = 1 / 255)

# Load images into memory and resize them
train_image_array_gen <-
  flow_images_from_directory(
    train_image_files_path,
    train_data_gen,
    target_size = target_size,
    class_mode = "categorical"
  )

valid_image_array_gen <-
  flow_images_from_directory(
    valid_image_files_path,
    valid_data_gen,
    target_size = target_size,
    class_mode = "categorical"
  )

test_image_array_gen <-
  flow_images_from_directory(
    test_image_files_path,
    test_data_gen,
    target_size = target_size,
    class_mode = "categorical"
  )

cat("Number of images per class:")
table(factor(train_image_array_gen$classes))

cat("\nClass label vs index mapping:\n")
train_image_array_gen$class_indices

Simpson_classes_indices <- train_image_array_gen$class_indices
Simpson_classes_indices

save(Simpson_classes_indices, file = "simpson_classes_indices.RData")

# Define the Keras Model
# Display number of Train, Valid, Test samples
train_samples <- train_image_array_gen$n
cat(" Training samples:",train_samples)
valid_samples <- valid_image_array_gen$n
cat(" Validation samples:",valid_samples)
test_samples <- test_image_array_gen$n
cat(" Test samples:",test_samples)


# Initialize model
  model <- keras_model_sequential()

# add layers
model %>%
  layer_conv_2d(
    filter = 32,
    kernel_size = c(5, 5),
    input_shape = c(img_width, img_height, channels)
  ) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(
  filter = 64,
    kernel_size = c(3, 3),
    input_shape = c(img_width, img_height, channels)
  ) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(
    filter = 128,
   kernel_size = c(3, 3),
    input_shape = c(img_width, img_height, channels)
  ) %>%
  layer_activation("relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  # layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(128) %>%
  layer_activation("relu") %>%
  layer_dropout(0.25) %>%
  
  layer_dense(64) %>%
  layer_activation("relu") %>%
  
  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>%
  layer_activation("softmax")
  
  # compile
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = "accuracy" 
  )
  
# fit_generator() to run the training
  hist <- model %>% fit_generator(
    # training data
      train_image_array_gen,
    # epochs
      steps_per_epoch = as.integer(train_samples / batch_size),
      epochs = epochs,
    # validation data
      validation_data = valid_image_array_gen,
      validation_steps = as.integer(valid_samples / batch_size),
    # print progress
      verbose = 2,
      callbacks = list(
    # save best model after every epoch
      callback_model_checkpoint("simpson_checkpoints.h5", save_best_only = TRUE),
    # only needed for visualising with TensorBoard
      callback_tensorboard(log_dir = "logs")
    )
   )

#Output as Plot
plot(hist)

#Evaluate model on test data
eval <- evaluate_generator(model, test_image_array_gen, steps = 1)  

#Training Stats
cat(" Training Accuracy:",tail(hist$metrics$acc,n=1),"\n","Training Loss:",tail(hist$metrics$loss,n=1),"\n")

#Validation Stats
cat(" Validation Accuracy:",tail(hist$metrics$val_acc,n=1),"\n","Validation Loss:",tail(hist$metrics$val_loss,n=1),"\n")

#Testing Stats
cat(" Test Accuracy:",eval$acc,"\n","Test Loss:",eval$loss,"\n")


