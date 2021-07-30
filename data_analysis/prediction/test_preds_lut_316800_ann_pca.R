# load libraries
library(keras)
library(tidyverse)
library(raster)

# load training/val/test sets
database_path = ".\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "316800")

# training set
training = data_sets[3] %>% read_csv() %>% as.matrix()
training_label = training[, 1:4]
training_label_scaled = training_label %>% scale()
training = training[, -c(1:6)]

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

# prisma set
pca_prisma = data_sets[1] %>% read_csv() %>% as.matrix()

# load the trained model
model = load_model_tf(filepath = "./data_analysis/models/ann_pca_lut316800_3h")

# load the saved history
history = read_csv(file = "./data_analysis/models/history_ann_pca_lut316800_3h.txt")

# visualize the training history
history_annpc_mse = history %>% 
        filter(metric == "loss") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, max(history$value)) +
        labs(x = "Epoch number",
             y = "MSE",
             col = NULL,
             title = "Training ANN with PCs") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

history_annpc_mae = history %>% 
        filter(metric == "mean_absolute_error") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, .7) +
        labs(x = "Epoch number",
             y = "MAE",
             col = NULL,
             title = "Training ANN with PCs") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

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



























