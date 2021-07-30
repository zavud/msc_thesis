# load libraries
library(keras)
library(tidyverse)
library(tidymodels)
library(latex2exp)

# load prisma wl bands
wl_path = ".\\data_analysis\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "cd", "d")

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

# store the scaling factors of testing labels
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

# load the trained model
model = load_model_tf(filepath = "./data_analysis/models/ann_RS_lut316800_3h")

# evaluate the model
eval_train = model %>% evaluate(training, training_label_scaled)
eval_val = model %>% evaluate(validation, validation_label_scaled)
eval_test = model %>% evaluate(testing, testing_label_scaled)

# predicstions on test set
preds_test = model %>% predict(testing)

# unscale the predictions
preds_cab_testing = (preds_test[, 1] * std_cab_testing) + center_cab_testing
preds_cw_testing = (preds_test[, 2] * std_cw_testing) + center_cw_testing
preds_cm_testing = (preds_test[, 3] * std_cm_testing) + center_cm_testing
preds_lai_testing = (preds_test[, 4] * std_lai_testing) + center_lai_testing

# calculate rsquared values for each variables
source(".\\data_analysis\\my_functions\\rsq.R") # load the function rsq i created
rsq_cab = rsq(preds = preds_cab_testing, actual = testing_label[, 1])
rsq_cw = rsq(preds = preds_cw_testing, actual = testing_label[, 2])
rsq_cm = rsq(preds = preds_cm_testing, actual = testing_label[, 3])
rsq_lai = rsq(preds = preds_lai_testing, testing_label[, 4])

# compare predictions and testing labels
set.seed(1)
samples = sample(x = nrow(testing_label_scaled),
                 size = 1500)
g_cab_annrs = ggplot(data.frame(x = testing_label[samples, 1],
                                 y = preds_cab_testing[samples]),
                      aes(x, y)) + geom_point(alpha = .5, col = "#D55E00", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$) modelled (RTM)"), 
             y = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$) predicted (ANN)"), 
             title = TeX("ANN with simulated PRISMA bands")) +
        theme_bw()

g_cw_annrs = ggplot(data.frame(x = testing_label[samples, 2],
                                y = preds_cw_testing[samples]),
                     aes(x, y)) + 
        geom_point(alpha = .5, col = "#D55E00", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{w} ($\\frac{g}{cm^2}) modelled (RTM)"), 
             y = TeX("C_{w} ($\\frac{g}{cm^2}) predicted (ANN)")) +
        theme_bw()

g_cm_annrs = ggplot(data.frame(x = testing_label[samples, 3],
                                y = preds_cm_testing[samples]),
                     aes(x, y)) +
        geom_point(alpha = .5, col = "#D55E00", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{m} ($\\frac{g}{cm^2}) modelled (RTM)"), 
             y = TeX("C_{m} ($\\frac{g}{cm^2}) predicted (ANN)")) +
        theme_bw()

g_lai_annrs = ggplot(data.frame(x = testing_label[samples, 4],
                                 y = preds_lai_testing[samples]),
                      aes(x, y)) + 
        geom_point(alpha = .5, col = "#D55E00", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("LAI_{s} ($\\frac{m^2}{m^2}$) modelled (RTM)"), 
             y = TeX("LAI_{s} ($\\frac{m^2}{m^2}$) predicted (ANN)")) +
        theme_bw()

g_cab_annpca + g_cab_annrs








