# load libraries
library(keras)
library(tidyverse)

# load testing data
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "120000")
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, 5:10]

# extract the scaling factors to later unscale the predictions
centers = attributes(testing_label_scaled)[[3]]
stds = attributes(testing_label_scaled)[[4]]

# scaling factors for cab
center_cab = centers[[1]]
std_cab = stds[[1]]

# scaling factors for cw
center_cw = centers[[2]]
std_cw = stds[[2]]

# scaling factors for cm
center_cm = centers[[3]]
std_cm = stds[[3]]

# scaling factors for lai
center_lai = centers[[4]]
std_lai = stds[[4]]

# load the saved model
model = load_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut120000_test")

# make a prediction on the test data
model %>% evaluate(testing, testing_label_scaled)
preds_scaled = model %>% predict(testing)

# unscale the predictions
preds_cab = (preds_scaled[, 1] * std_cab) + center_cab
preds_cw = (preds_scaled[, 2] * std_cw) + center_cw
preds_cm = (preds_scaled[, 3] * std_cm) + center_cm
preds_lai = (preds_scaled[, 4] * std_lai) + center_lai

# calculate rsquared values for each variables
source("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\my_functions\\rsq.R") # load the function rsq i created
rsq_cab = rsq(preds = preds_cab, actual = testing_label[, 1])
rsq_cw = rsq(preds = preds_cw, actual = testing_label[, 2])
rsq_cm = rsq(preds = preds_cm, actual = testing_label[, 3])
rsq_lai = rsq(preds = preds_lai, testing_label[, 4])

rsq_total = (rsq_cab + rsq_cw + rsq_cm + rsq_lai) / 4 # ???

# make predictions on the prisma data
preds_prisma = model %>% predict(pca_prisma %>% as.matrix())
preds_cab_prisma = (preds_prisma[, 1] * std_cab) + center_cab
preds_cw_prisma = (preds_prisma[, 2] * std_cw) + center_cw
preds_cm_prisma = (preds_prisma[, 3] * std_cm) + center_cm
preds_lai_prisma = (preds_prisma[, 4] * std_lai) + center_lai

# make biophysical maps for the NPHH
prisma_img = raster::as.data.frame(prisma[[55]], xy = T, na.rm = T)
prisma_biomap_df = prisma_img %>% dplyr::select(1:2) %>% mutate(cab = preds_lai_prisma,
                                                                cw = preds_cw_prisma,
                                                                cm = preds_cm_prisma,
                                                                lai = preds_lai_prisma)
biomap_img = rasterFromXYZ(prisma_biomap_df, res = res(prisma[[55]]), crs = crs(prisma[[55]]))
plot(lai_img)

hist(lai_img)

# visualize the results
g_cab = ggplot(data.frame(x = testing_label[, 1],
                  y = preds_cab),
       aes(x, y)) + geom_point(alpha = .2, col = "brown") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cab modelled", y = "Cab predicted") +
        theme_light()

g_cw = ggplot(data.frame(x = testing_label[, 2],
                  y = preds_cw),
       aes(x, y)) + geom_point(alpha = .2, col = "brown") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cw modelled", y = "Cw predicted") +
        theme_light()

g_cm = ggplot(data.frame(x = testing_label[, 3],
                  y = preds_cm),
       aes(x, y)) + geom_point(alpha = .2, col = "brown") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "Cm modelled", y = "Cm predicted") +
        theme_light()

g_lai = ggplot(data.frame(x = testing_label[, 4],
                  y = preds_lai),
       aes(x, y)) + geom_point(alpha = .2, col = "brown") +
        geom_abline(slope = 1, intercept = 0, size = 1.2, col = "blue") +
        labs(x = "LAI modelled", y = "LAI predicted") +
        theme_light()




















