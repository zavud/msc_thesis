# load libraries
library(keras)
library(tidyverse)

# load training/val/test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "132300")

# training set
training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:4)]

# validation set
validation = data_sets[3] %>% read_csv() %>% as.matrix()
validation_label = validation[, 1:4]
validation_label_scaled = validation_label %>% scale()
validation = validation[, -c(1:4)]

# testing set
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, -c(1:4)]

# check if the dimensions of the matrices are correct
dim(training)
dim(validation)
dim(testing)

dim(training_label)
dim(validation_label)
dim(testing_label)

dim(training_label_scaled)
dim(validation_label_scaled)
dim(testing_label_scaled)

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", input_shape = ncol(training),
                    kernel_initializer = initializer_he_normal()) %>%
        layer_dense(units = 100, activation = "relu", kernel_initializer = initializer_he_normal()) %>% 
        layer_dense(units = ncol(training_label))
model %>% 
        compile(optimizer = optimizer_adam(lr = .01),
                loss = "mse",
                metrics = list("mean_absolute_error"))
history = model %>% 
        fit(x = training,
            y = training_label_scaled,
            validation_data = list(validation, validation_label_scaled),
            verbose = 2,
            epochs = 500,
            batch_size = 512)

history %>% 
        as_tibble() %>% 
        filter(metric == "loss") %>% 
        mutate(rmse = sqrt(value)) %>% 
        ggplot(aes(x = epoch, y = rmse, col =  data)) +
        geom_line(size = 1) +
        labs(x = "Iteration", y = "RMSE", title = "PRISMA LUT - ANN with 4 PCs", col = NULL) +
        theme_bw() +
        theme(legend.position = c(.8, .8))

# save the model
model %>% save_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut_prosail132300_test")
