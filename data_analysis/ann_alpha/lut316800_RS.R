# load libraries
library(keras)
library(tidyverse)
library(raster)
library(tidymodels)

# load prisma wl bands
wl_path = ".\\data_analysis\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "cd", "d")

# load prisma data
prisma_path = ".\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::brick(prisma_path)
prisma_df = raster::as.data.frame(prisma, na.rm = T) %>% as_tibble() %>% setNames(wl)

# load the lut data
lut_path = ".\\data_analysis\\inform_prisma\\lut_database"
lut_file = list.files(path = lut_path, pattern = "316800", full.names = T)
lut_1 = read_csv(lut_file[1]) %>% setNames(nms)
lut_2 = read_csv(lut_file[2]) %>% setNames(nms)

# combine the 2 datasets
lut = bind_rows(lut_1, lut_2)

# remove the lut_1 and lut_2 from the environment
rm(lut_1, lut_2)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# add 3% noise to the simulated spectra
sds = apply(lut[, 1:231], 1, sd) # sd of each spectra
lut[, 1:231] = lut[, 1:231] + rnorm(n = ncol(lut[, 1:231]) * nrow(lut[, 1:231]), mean = 0, sd = sds * .03)

# divide the data into train/val/test sets
data_split = initial_split(lut, prop = .90)
training_val = training(data_split)
training = training_val[1:250120,]
validation = training_val[-c(1:250120), ]
testing = testing(data_split)
rm(training_val)

# relocation
training = training %>% 
        relocate(Cab, Cw, Cm, LAI, cd, d) %>% 
        as.matrix()
validation = validation %>% 
        relocate(Cab, Cw, Cm, LAI, cd, d) %>% 
        as.matrix()
testing = testing %>% 
        relocate(Cab, Cw, Cm, LAI, cd, d) %>% 
        as.matrix()

# training set
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:6)]

# mean and sd
mn = apply(training, 2, mean)
std = apply(training, 2, sd)
training = training %>% 
        scale(center = mn,
              scale = std)

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

# scaling factors for cd
#center_cd = mns_training_label[[5]]
#std_cd = stds_training_label[[5]]

# scaling factors for d
#center_d = mns_training_label[[6]]
#std_d = stds_training_label[[6]]

# store the scaling factors of training set
#mns_training = attributes(training)[[3]]
#stds_training = attributes(training)[[4]]

# validation set
validation_label = validation[, 1:4]
validation_label_scaled = validation_label %>% scale()
validation = validation[, -c(1:6)] %>% 
        scale(center = mn,
              scale = std)

# testing set
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, -c(1:6)]%>% 
        scale(center = mn,
              scale = std)

# prisma set
prisma_df = prisma_df %>% 
        as.matrix() %>% 
        scale(center = mn,
              scale = std)

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
        layer_dense(units = 512, activation = "relu", input_shape = ncol(training),
                    kernel_initializer = initializer_he_normal(),
                    kernel_regularizer = regularizer_l2(l = 0.0001)) %>%
        layer_dense(units = 512, activation = "relu",
                    kernel_initializer = initializer_he_normal(),
                    kernel_regularizer = regularizer_l2(l = 0.0001)) %>% 
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
            epochs = 1500,
            batch_size = 512,
            callbacks = callback_early_stopping(monitor = "loss", patience = 50))

history %>% 
        as_tibble() %>% 
        dplyr::filter(metric == "loss") %>% 
        ggplot(aes(x = epoch, y = value, col =  data)) +
        geom_line(size = 1) +
        labs(x = "Iteration", y = "MSE", title = "Deep Neural Network training", col = NULL) +
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
preds_cd = (preds_test[, 5] * std_cd) + center_cd
preds_d = (preds_test[, 6] * std_d) + center_d

# calculate rsquared values for each variables
source(".\\data_analysis\\my_functions\\rsq.R") # load the function rsq i created
rsq_cab = rsq(preds = preds_cab, actual = testing_label[, 1])
rsq_cw = rsq(preds = preds_cw, actual = testing_label[, 2])
rsq_cm = rsq(preds = preds_cm, actual = testing_label[, 3])
rsq_lai = rsq(preds = preds_lai, testing_label[, 4])
rsq_cd = rsq(preds = preds_cd, testing_label[, 5])
rsq_d = rsq(preds = preds_d, testing_label[, 6])

rsq_total = (rsq_cab + rsq_cw + rsq_cm + rsq_lai + rsq_cd + rsq_d) / 6 # ???

# compare predictions and testing labels
g_cab = ggplot(data.frame(x = testing_label[1:1000, 1],
                          y = preds_cab[1:1000]),
               aes(x, y)) + geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cab modelled (RTM)", y = "Cab predicted (ANN)", title = "Cab") +
        theme_bw()

g_cw = ggplot(data.frame(x = testing_label[1:1000, 2],
                         y = preds_cw[1:1000]),
              aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cw modelled (RTM)", y = "Cw predicted (ANN)", title = "Cw") +
        theme_bw()

g_cm = ggplot(data.frame(x = testing_label[1:1000, 3],
                         y = preds_cm[1:1000]),
              aes(x, y)) +
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cm modelled (RTM)", y = "Cm predicted (ANN)", title = "Cm") +
        theme_bw()

g_lai = ggplot(data.frame(x = testing_label[1:1000, 4],
                          y = preds_lai[1:1000]),
               aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "LAI modelled (RTM)", y = "LAI predicted (ANN)", title = "LAI") +
        theme_bw()

g_cd = ggplot(data.frame(x = testing_label[1:1000, 5],
                         y = preds_cd[1:1000]),
              aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "CD modelled (RTM)", y = "CD predicted (ANN)", title = "CD") +
        theme_bw()

g_d = ggplot(data.frame(x = testing_label[1:1000, 6],
                        y = preds_d[1:1000]),
             aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "D modelled (RTM)", y = "D predicted (ANN)", title = "D") +
        theme_bw()

# make predictions on the prisma data
preds_prisma = model %>% predict(prisma_df %>% as.matrix())
preds_cab_prisma = (preds_prisma[, 1] * std_cab) + center_cab
preds_cw_prisma = (preds_prisma[, 2] * std_cw) + center_cw
preds_cm_prisma = (preds_prisma[, 3] * std_cm) + center_cm
preds_lai_prisma = (preds_prisma[, 4] * std_lai) + center_lai
preds_cd_prisma = (preds_prisma[, 5] * std_cd) + center_cd
preds_d_prisma = (preds_prisma[, 6] * std_d) + center_d


# make biophysical maps for the NPHH
prisma_path = ".\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::brick(prisma_path)
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
        #filter(between(cab, 20, 60)) %>% 
        mutate(pixel_class = case_when(cab > 0 & cab < 100 ~ "normal",
                                       cab < 0 ~ "small",
                                       cab > 100 ~ "large")) %>% 
        ggplot(aes(x, y, fill = pixel_class)) +
        geom_raster() +
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
             subtitle = "Range in the simulation : 20 - 60") +
        theme_bw()
g_cab_map + g_cab_hist        

prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cab) %>% 
        dplyr::filter(cab > 0, cab < 100) %>%
        count()

# cw
g_cw_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cw) %>% 
        mutate(pixel_class = case_when(cw > 0.00035 & cw < 0.04 ~ "normal",
                                       cw < 0.00035 ~ "small",
                                       cw > 0.04 ~ "large")) %>% 
        #filter(cw > 0) %>% 
        ggplot(aes(x, y, fill = pixel_class)) +
        geom_raster() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cw map", fill = "Cw") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "vertical")


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
        mutate(pixel_class = case_when(cm > 0.0008 & cm < 0.04 ~ "normal",
                                       cm < 0.0008 ~ "small",
                                       cm > 0.04 ~ "large")) %>% 
        ggplot(aes(x, y, fill = pixel_class)) +
        geom_raster() +
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
        #filter(lai > 0) %>% 
        mutate(pixel_class = case_when(lai > 0 & lai < 10 ~ "normal",
                                       lai < 0 ~ "small",
                                       lai > 10 ~ "big")) %>% 
        ggplot(aes(x, y, fill = pixel_class)) +
        geom_raster() +
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

# cd
g_cd_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cd) %>% 
        #filter(lai > 0) %>% 
        ggplot(aes(x, y, fill = cd)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved CD map", fill = "CD") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cd_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cd) %>% 
        ggplot(aes(x = cd)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "CD", y = "Frequency", title = "Distribution of retrieved CD values",
             subtitle = "Range in the simulation : 1.5 - 8.5") +
        theme_bw()

# d
g_d_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, d) %>% 
        #filter(lai > 0) %>% 
        ggplot(aes(x, y, fill = d)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved D map", fill = "D") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_d_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(d) %>% 
        ggplot(aes(x = d)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "D", y = "Frequency", title = "Distribution of retrieved D values") +
        theme_bw()

