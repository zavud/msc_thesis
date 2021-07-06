# load libraries
library(keras)
#library(tfdatasets)
library(tidyverse)

# load training/val/test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "162000")

# training set
training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 1:6]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:6)]

# testing set
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:6]
testing_label_scaled = testing_label %>% scale()
testing = testing[, -c(1:6)]

# validation set
validation = data_sets[3] %>% read_csv() %>% as.matrix()
validation_label = validation[, 1:6]
validation_label_scaled = validation_label %>% scale()
validation = validation[, -c(1:6)]

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", input_shape = ncol(training)) %>%
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = ncol(training_label))
model %>% 
        compile(optimizer = optimizer_adam(lr = .001),
                loss = "mse",
                metrics = list("mean_absolute_error"))
history = model %>% 
        fit(x = training,
            y = training_label_scaled,
            validation_data = list(validation, validation_label_scaled),
            verbose = 2,
            epochs = 200,
            batch_size = 512)

history %>% 
        as_tibble() %>% 
        filter(metric == "loss") %>% 
        mutate(rmse = sqrt(value)) %>% 
        ggplot(aes(x = epoch, y = rmse, col =  data)) +
        geom_line(size = 1) +
        labs(x = "Iteration", y = "RMSE", title = "PRISMA LUT - ANN with 6 PCs", col = NULL) +
        theme_bw() +
        theme(legend.position = c(.8, .8))


model %>% evaluate(testing, testing_label_scaled)
model %>% predict(pca_prisma %>% as.matrix())

# save this model
model %>% save_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut120000_test")
