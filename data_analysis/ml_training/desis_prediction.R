# load libraries
library(keras)
library(raster)
library(rgdal)
library(patchwork)

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

# extract scaling information from the training data
centers = attributes(training_label_scaled)[[3]]
stds = attributes(training_label_scaled)[[4]]

# centers
center_cab = centers[[1]]
center_cm = centers[[2]]
center_lai = centers[[3]]
center_cd = centers[[4]]

# stds
std_cab = stds[[1]]
std_cm = stds[[2]]
std_lai = stds[[3]]
std_cd = stds[[4]]

# load the model
model = load_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut160000_desis_test")

# make predictions on the testing lut
preds_test = model %>% predict(testing)

# unscale the preds_test
pred_cab = (preds_test[, 1] * std_cab) + center_cab
pred_cm = (preds_test[, 2] * std_cm) + center_cm
pred_lai = (preds_test[, 3] * std_lai) + center_lai
pred_cd = (preds_test[, 4] * std_cd) + center_cd

# visualize the results
g_cab = tibble(x = testing_label[, 1], y = pred_cab) %>% 
        ggplot(aes(x, y)) +
        geom_point(alpha = .3, col = "brown") +
        labs(title = "a) Cab", x = "Cab modelled (RTM)", y = "Cab predicted (ANN)") +
        geom_abline(slope = 1, intercept = 0, col = "blue", size = 1.2) +
        theme_bw()
g_cm = tibble(x = testing_label[, 2], y = pred_cm) %>% 
        ggplot(aes(x, y)) +
        geom_point(alpha = .3, col = "brown") +
        labs(title = "b) Cm", x = "Cm modelled (RTM)", y = "Cm predicted (ANN)") +
        geom_abline(slope = 1, intercept = 0, col = "blue", size = 1.2) +
        theme_bw()
g_lai = tibble(x = testing_label[, 3], y = pred_lai) %>% 
        ggplot(aes(x, y)) +
        geom_point(alpha = .3, col = "brown") +
        labs(title = "c) LAI", x = "LAI modelled (RTM)", y = "LAI predicted (ANN)") +
        geom_abline(slope = 1, intercept = 0, col = "blue", size = 1.2) +
        theme_bw()
g_cd = tibble(x = testing_label[, 4], y = pred_cd) %>% 
        ggplot(aes(x, y)) +
        geom_point(alpha = .3, col = "brown") +
        labs(title = "d) CD", x = "CD modelled (RTM)", y = "CD predicted (ANN)") +
        geom_abline(slope = 1, intercept = 0, col = "blue", size = 1.2) +
        theme_bw()
g_cab + g_cm + g_lai + g_cd

# predict on the desis image (april)
preds_desis = model %>% predict(pca_desis %>% as.matrix())

# unscale the result
pred_cab_desis = (preds_desis[, 1] * std_cab) + center_cab
pred_cm_desis = (preds_desis[, 2] * std_cm) + center_cm
pred_lai_desis = (preds_desis[, 3] * std_lai) + center_lai
pred_cd_desis = (preds_desis[, 4] * std_cd) + center_cd

# desis biomaps
desis_template = desis_4[[1]] %>% raster::as.data.frame(xy = T, na.rm = T) %>% as_tibble()
desis_biomap_df = desis_template %>% dplyr::select(1:2) %>% mutate(cab = pred_cab_desis,
                                                            cm = pred_cm_desis,
                                                            lai = pred_lai_desis,
                                                            cd = pred_cd_desis)
desis_biomap_img = rasterFromXYZ(desis_biomap_df, res = res(desis_4), crs = crs(desis_4))
plot(desis_biomap_img)
