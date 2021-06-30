# load libraries
library(keras)
library(tfdatasets)
library(tidyverse)

# load training and test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "pca")

training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 3:7]
training_label_scaled = training_label %>% scale()
training = training[, 1:2]

testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 3:7]
testing_label_scaled = testing_label %>% scale()
testing = testing[, 1:2]

# build the model
model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", input_shape = 2,
                    kernel_initializer = initializer_he_normal(),
                    kernel_regularizer = regularizer_l2()) %>%
        layer_batch_normalization() %>% 
        layer_dropout(.1) %>% 
        layer_dense(units = 5)
model %>% 
        compile(optimizer = optimizer_adam(lr = .001),
                loss = "mse",
                metrics = list("mean_absolute_error"))
model %>% 
        fit(x = training,
            y = training_label_scaled,
            validation_split = .01,
            verbose = 2,
            epochs = 250,
            batch_size = 512)
model %>% evaluate(testing, testing_label_scaled)
preds = model %>% predict(testing)
preds_unscaled = 5.00572963 + (preds[, 5] * 2.229029267)
plot(x = testing_label[, 5], preds_unscaled)

