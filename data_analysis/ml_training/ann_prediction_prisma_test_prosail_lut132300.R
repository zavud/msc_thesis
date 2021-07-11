# load libraries
library(keras)
library(readr)
library(patchwork)

# load testing data
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T, pattern = "132300")
testing = data_sets[1] %>% read_csv() %>% as.matrix()
testing_label = testing[, 1:4]
testing_label_scaled = testing_label %>% scale()
testing = testing[, -c(1:4)]

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
model = load_model_tf("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\ml_training\\models\\ann_lut_prosail132300_test")

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
             subtitle = "Range in the simulation : 40 - 80") +
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

# visualize the results
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