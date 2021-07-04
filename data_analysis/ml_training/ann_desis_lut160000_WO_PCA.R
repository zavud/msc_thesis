# load libraries
library(tidymodels)
library(tidyverse)
library(raster)
library(rgdal)
library(keras)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_desis.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cm", "LAI", "cd")

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases"
lut_files = list.files(path = lut_path, pattern = "160000", full.names = T)
lut = read_csv(lut_files) %>% setNames(nms)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# divide the data into train/val/test sets
data_split = initial_split(lut, prop = .75)
training_val = training(data_split)
training = training_val[1:100000,]
validation = training_val[-c(1:100000), ]
testing = testing(data_split)
rm(training_val)

# preprop
training_label = training[, 236:239] %>% as.matrix() %>% scale()
training = training[, 1:235] %>% as.matrix()
validation_label = validation[, 236:239] %>% as.matrix() %>% scale()
validation = validation[, 1:235] %>% as.matrix()
testing_label = testing[, 236:239] %>% as.matrix() %>% scale()
testing = testing[, 1:235] %>% as.matrix()


model = keras_model_sequential() %>% 
        layer_dense(units = 100, activation = "relu", kernel_initializer = initializer_he_normal(), input_shape = ncol(training)) %>% 
        layer_dense(units = 100, activation = "relu", kernel_initializer = initializer_he_normal()) %>% 
        layer_dense(units = ncol(training_label), activation = "linear")
model %>% compile(optimizer = optimizer_adam(),
                  loss = "mse",
                  metric = list("mean_absolute_error"))
history = model %>% fit(x = training,
              y = training_label,
              validation_data = list(validation, validation_label),
              epochs = 500,
              batch_size = 512)

history %>% 
        as_tibble() %>% 
        filter(metric == "loss") %>% 
        mutate(rmse = sqrt(value)) %>% 
        ggplot(aes(x = epoch, y = rmse, col = data)) +
        geom_point(alpha = .5) +
        geom_line(alpha = .5) +
        labs(x = "Iteration", y = "RMSE", col = NULL,
             title = "ANN wihtout PCA") +
        ylim(0, 1) +
        theme_bw() +
        theme(legend.position = c(.3, .9),
              legend.direction = "horizontal")

