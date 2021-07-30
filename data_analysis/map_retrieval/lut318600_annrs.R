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

# prisma set
prisma_df = prisma_df %>% 
        as.matrix() %>% 
        scale(center = mn,
              scale = std)

# load the trained model
model = load_model_tf(filepath = "./data_analysis/models/ann_RS_lut316800_3h")

# make predictions on the prisma data
preds_prisma = model %>% predict(prisma_df %>% as.matrix())
preds_cab_prisma = (preds_prisma[, 1] * std_cab) + center_cab
preds_cw_prisma = (preds_prisma[, 2] * std_cw) + center_cw
preds_cm_prisma = (preds_prisma[, 3] * std_cm) + center_cm
preds_lai_prisma = (preds_prisma[, 4] * std_lai) + center_lai

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
prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cab) %>% 
        mutate(pixel_class = ifelse(cab > 20 & cab < 80, "normal", "abnormal")) %>% 
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
        mutate(pixel_class = case_when(lai > 0 & lai < 15 ~ "normal",
                                       lai < 0 ~ "small",
                                       lai > 15 ~ "big")) %>% 
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



















