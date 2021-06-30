# load libraries
library(keras)
library(tfdatasets)
library(tidyverse)

# load training/val/test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "120000")

# training set
training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, 5:10]

# testing set
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, 5:10]

# validation set
validation = data_sets[3] %>% read_csv() %>% as.matrix()
validation_label = validation[, 1:4]
validation_label_scaled = validation_label %>% scale()
validation = validation[, 5:10]

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", input_shape = 6,
                    kernel_initializer = initializer_he_normal(),
                    kernel_regularizer = regularizer_l2()) %>%
        layer_dense(units = 32, activation = "relu",
                    kernel_initializer = initializer_he_normal()) %>% 
        layer_dense(units = 4)
model %>% 
        compile(optimizer = optimizer_adam(lr = .001),
                loss = "mse",
                metrics = list("mean_absolute_error"))
model %>% 
        fit(x = training,
            y = training_label_scaled,
            validation_data = list(validation, validation_label_scaled),
            verbose = 2,
            epochs = 500,
            batch_size = 512)
model %>% evaluate(testing, testing_label_scaled)

# save this model
model %>% save_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut120000_test")
