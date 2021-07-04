# load libraries
library(keras)
# library(tfdatasets)
library(tidyverse)

# load training/val/test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\desis_training_database"
data_sets = list.files(path = database_path, full.names = T)

# load training set
training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, 5:10]

# load testing set
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, 5:10]

# load validation set
validation = data_sets[3] %>% read_csv() %>% as.matrix()
validation_label = validation[, 1:4]
validation_label_scaled = validation_label %>% scale()
validation = validation[, 5:10]

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", input_shape = ncol(training),
                    kernel_initializer = initializer_glorot_normal()) %>% 
        layer_dense(units = 100, activation = "relu",
                    kernel_initializer = initializer_glorot_normal()) %>% 
        layer_dense(units = ncol(training_label_scaled), activation = "linear")
model %>% compile(optimizer = optimizer_adam(),
                  loss = "mse",
                  metric = list("mean_absolute_error"))
history = model %>% fit(x = training,
              y = training_label_scaled,
              validation_data = list(validation, validation_label_scaled),
              verbose = 2, 
              epochs = 500,
              batch_size = 512)

history %>% 
        as_tibble() %>% 
        filter(metric == "loss") %>% 
        mutate(rmse = sqrt(value)) %>% 
        ggplot(aes(x = epoch, y = rmse, col = data)) +
        geom_line(alpha = .4) +
        geom_point(alpha = .4) +
        labs(x = "Iteration", y = "RMSE", col = NULL,
             title = "ANN with 6 PCs") +
        ylim(0, 1) +
        theme_bw() +
        theme(legend.position = c(.3, .8),
              legend.direction = "horizontal")

model %>% evaluate(testing, testing_label_scaled)

# save this model
model %>% save_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut160000_desis_test")








