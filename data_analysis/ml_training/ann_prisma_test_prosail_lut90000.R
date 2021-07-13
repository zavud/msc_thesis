# load libraries
library(keras)
library(tidyverse)

# load training/val/test sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "67500")

# training set
training = data_sets[2] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:4)] %>% scale()

# store the scaling factors of training labels
mns_training_label = attributes(training_label_scaled)[[3]]
stds_training_label = attributes(training_label_scaled)[[4]]

center_cab = mns_training_label[[1]]
std_cab = stds_training_label[[1]]

# scaling factors for cw
center_cw = mns_training_label[[2]]
std_cw = stds_training_label[[2]]

# scaling factors for cm
center_cm = mns_training_label[[3]]
std_cm = stds_training_label[[3]]

# scaling factors for lai
center_lai = mns_training_label[[4]]
std_lai = stds_training_label[[4]]


# store the scaling factors of training set
mns_training = attributes(training)[[3]]
stds_training = attributes(training)[[4]]

# validation set
validation = data_sets[3] %>% read_csv() %>% as.matrix()
validation_label = validation[, 1:4]
validation_label_scaled = validation_label %>% scale(center = mns_training_label, scale = stds_training_label)
validation = validation[, -c(1:4)] %>% scale(center = mns_training, scale = stds_training)

# testing set
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale(center = mns_training_label, scale = stds_training_label)
testing = testing[, -c(1:4)] %>% scale(center = mns_training, scale = stds_training)

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
                    kernel_initializer = initializer_lecun_normal()) %>%
        layer_dense(units = 100, activation = "relu",
                    kernel_initializer = initializer_lecun_normal()) %>% 
        layer_dense(units = ncol(training_label_scaled))
model %>% 
        compile(optimizer = optimizer_adam(lr = .001, decay = .001 / 500),
                loss = "mse",
                metrics = list("mean_absolute_error"))
history = model %>% 
        fit(x = training,
            y = training_label_scaled,
            validation_data = list(validation, validation_label_scaled),
            verbose = 2,
            epochs = 500,
            batch_size = 512,
            callbacks = callback_early_stopping(monitor = "val_loss", patience = 25))

history %>% 
        as_tibble() %>% 
        filter(metric == "loss") %>% 
        mutate(rmse = sqrt(value)) %>% 
        ggplot(aes(x = epoch, y = rmse, col =  data)) +
        geom_line(size = 1) +
        labs(x = "Iteration", y = "RMSE", title = "PRISMA LUT - ANN with 4 PCs", col = NULL) +
        theme_bw() +
        theme(legend.position = c(.8, .8))

model %>% evaluate(testing, testing_label_scaled)

# predicstions on test set
preds_test = model %>% predict(testing)

# unscale the predictions
preds_cab = (preds_test[, 1] * std_cab) + center_cab
preds_cw = (preds_test[, 2] * std_cw) + center_cw
preds_cm = (preds_test[, 3] * std_cm) + center_cm
preds_lai = (preds_test[, 4] * std_lai) + center_lai

# calculate rsquared values for each variables
source("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\my_functions\\rsq.R") # load the function rsq i created
rsq_cab = rsq(preds = preds_cab, actual = testing_label[, 1])
rsq_cw = rsq(preds = preds_cw, actual = testing_label[, 2])
rsq_cm = rsq(preds = preds_cm, actual = testing_label[, 3])
rsq_lai = rsq(preds = preds_lai, testing_label[, 4])

rsq_total = (rsq_cab + rsq_cw + rsq_cm + rsq_lai) / 4 # ???

# compare predictions and testing labels
g_cab = ggplot(data.frame(x = testing_label[, 1],
                          y = preds_cab),
               aes(x, y)) + geom_point(alpha = .2, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cab modelled (RTM)", y = "Cab predicted (ANN)", title = "Cab") +
        theme_bw()

g_cw = ggplot(data.frame(x = testing_label[, 2],
                         y = preds_cw),
              aes(x, y)) + 
        geom_point(alpha = .2, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cw modelled (RTM)", y = "Cw predicted (ANN)", title = "Cw") +
        theme_bw()

g_cm = ggplot(data.frame(x = testing_label[, 3],
                         y = preds_cm),
              aes(x, y)) +
        geom_point(alpha = .2, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cm modelled (RTM)", y = "Cm predicted (ANN)", title = "Cm") +
        theme_bw()

g_lai = ggplot(data.frame(x = testing_label[, 4],
                          y = preds_lai),
               aes(x, y)) + 
        geom_point(alpha = .2, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "LAI modelled (RTM)", y = "LAI predicted (ANN)", title = "LAI") +
        theme_bw()

# make predictions on the prisma data
preds_prisma = model %>% predict(pca_prisma %>% as.matrix() %>% scale(center = mns_training, scale = stds_training))
preds_cab_prisma = (preds_prisma[, 1] * std_cab) + center_cab
preds_cw_prisma = (preds_prisma[, 2] * std_cw) + center_cw
preds_cm_prisma = (preds_prisma[, 3] * std_cm) + center_cm
preds_lai_prisma = (preds_prisma[, 4] * std_lai) + center_lai

# make biophysical maps for the NPHH
prisma_template = raster::as.data.frame(prisma[[55]], xy = T, na.rm = T)
prisma_biomap_df = prisma_template %>% dplyr::select(1:2) %>% mutate(cab = preds_cab_prisma,
                                                                     cw = preds_cw_prisma,
                                                                     cm = preds_cm_prisma,
                                                                     lai = preds_lai_prisma)
biomap_img = rasterFromXYZ(prisma_biomap_df, res = res(prisma[[55]]), crs = crs(prisma[[55]]))
plot(biomap_img)

# plot prisma Cab image
g_cab_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cab) %>% 
        filter(between(cab, 0, 100)) %>% 
        ggplot(aes(x, y, fill = cab)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cab map", fill = "Cab") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cab_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cab) %>% 
        ggplot(aes(x = cab)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cab", y = "Frequency", title = "Distribution of retrieved Cab values",
             subtitle = "Range in the simulation : 30 - 90") +
        theme_bw()
g_cab_map + g_cab_hist        

prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cab) %>% 
        filter(cab > 30, cab < 90) %>%
        count()

# cw
g_cw_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cw) %>% 
        ggplot(aes(x, y, fill = cw)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cw map", fill = "Cw") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cw_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cw) %>% 
        ggplot(aes(x = cw)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cw", y = "Frequency", title = "Distribution of retrieved Cw values",
             subtitle = "Range in the simulation : 0.0035 - 0.035") +
        theme_bw()
g_cw_map + g_cw_hist

# cm
g_cm_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cm) %>% 
        ggplot(aes(x, y, fill = cm)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cm map", fill = "Cm") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cm_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cm) %>% 
        ggplot(aes(x = cm)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cm", y = "Frequency", title = "Distribution of retrieved Cm values",
             subtitle = "Range in the simulation : 0.008 - 0.03") +
        theme_bw()

g_cm_map + g_cm_hist

# lai
g_lai_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, lai) %>% 
        filter(lai < 20) %>% 
        ggplot(aes(x, y, fill = lai)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved LAI map", fill = "LAI") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_lai_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(lai) %>% 
        ggplot(aes(x = lai)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "LAI", y = "Frequency", title = "Distribution of retrieved LAI values",
             subtitle = "Range in the simulation : 3 - 9") +
        theme_bw()
g_lai_map + g_lai_hist

