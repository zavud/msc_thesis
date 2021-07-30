# load libraries
library(keras)
library(tidyverse)
library(raster)
library(latex2exp)
library(patchwork)

# load training/val/test sets
database_path = ".\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "316800")

# training set
training = data_sets[3] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:6)]

# testing set
testing = data_sets[2] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, -c(1:6)]

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

# load the trained model
model = load_model_tf(filepath = "./data_analysis/models/ann_pca_lut316800_3h")

# evaluate the model on the testing set
model %>% evaluate(testing, testing_label_scaled)

# predict on test set
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
g_cab_annpca = ggplot(data.frame(x = testing_label[samples, 1],
                          y = preds_cab_testing[samples]),
               aes(x, y)) + geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$)  modelled (RTM)"), 
             y = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$) predicted (ANN)"), 
             title = "ANN with PCs") +
        theme_bw()

g_cw_annpca = ggplot(data.frame(x = testing_label[samples, 2],
                         y = preds_cw_testing[samples]),
              aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{w} ($\\frac{g}{cm^2}) modelled (RTM)"), 
             y = TeX("C_{w} ($\\frac{g}{cm^2}) predicted (ANN)")) +
        theme_bw()

g_cm_annpca = ggplot(data.frame(x = testing_label[samples, 3],
                         y = preds_cm_testing[samples]),
              aes(x, y)) +
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("C_{m} ($\\frac{g}{cm^2}) modelled (RTM)"), 
             y = TeX("C_{m} ($\\frac{g}{cm^2}) predicted (ANN)")) +
        theme_bw()

g_lai_annpca = ggplot(data.frame(x = testing_label[samples, 4],
                          y = preds_lai_testing[samples]),
               aes(x, y)) + 
        geom_point(alpha = .5, col = "brown", position = "jitter") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = TeX("LAI_{s} ($\\frac{m^2}{m^2}$) modelled (RTM)"), 
             y = TeX("LAI_{s} ($\\frac{m^2}{m^2}$) predicted (ANN)")) +
        theme_bw()

# cleaning up
rm(preds_test,
   testing, 
   testing_label,
   testing_label_scaled,
   training, 
   training_label, 
   training_label_scaled, 
   center_cab_testing, 
   center_cm_testing, 
   center_cw_testing,
   center_lai_testing,
   data_sets,
   database_path, 
   mns_testing_label,
   model, 
   preds_cab_testing, 
   preds_cm_testing,
   preds_cw_testing, 
   preds_lai_testing,
   rsq_cab, 
   rsq_cm, 
   rsq_cw,
   rsq_lai,
   std_cab_testing, 
   std_cm_testing, 
   std_cw_testing,
   std_lai_testing, 
   stds_testing_label,
   samples, rsq)
