# load libraries
library(keras)
library(tfdatasets)
library(tidyverse)

# load training and test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "pca")

training = data_sets[2] %>% read_csv() %>% as.matrix()
trainin_label = training[, 3:7] %>% scale()
training = training[, 1:2]

testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 3:7] %>% scale()
testing = testing[, 1:2]

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 1, activation = "relu", input_shape = 2) %>%
        layer_dropout(.1) %>% 
        layer_dense(units = 5)
model %>% 
        compile(optimizer = optimizer_adam(lr = .001),
                loss = "mse",
                metrics = list("mean_absolute_error"))
model %>% 
        fit(x = training,
            y = trainin_label,
            validation_split = .01,
            verbose = 2,
            epochs = 100,
            batch_size = 512)
model %>% evaluate(testing, testing_label)
model %>% predict(testing)
