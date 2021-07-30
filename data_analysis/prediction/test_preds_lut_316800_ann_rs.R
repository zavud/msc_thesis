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

# store the scaling factors of training labels
mns_testing_label = attributes(testing_label_scaled)[[3]]
stds_testing_label = attributes(testing_label_scaled)[[4]]

center_cab_testing = mns_testing_label[[1]]
std_cab_testing = stds_testing_label[[1]]

# scaling factors for cw
center_cw_testing = mns_testing_label[[2]]
std_cw_testing = stds_testing_label[[2]]

# scaling factors for cm
center_cm_testing = mns_testing_label[[3]]
std_cm_testing = stds_testing_label[[3]]

# scaling factors for lai
center_lai_testing = mns_testing_label[[4]]
std_lai_testing = stds_testing_label[[4]]

# prisma set
prisma_df = prisma_df %>% as.matrix()

# load the trained model
model = load_model_tf(filepath = "./data_analysis/models/ann_RS_lut316800_3h")

















