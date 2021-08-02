# load libraries
library(keras)
library(tidyverse)
library(raster)
library(tidymodels)
library(ggspatial)
library(latex2exp)

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
map_cab = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cab) %>% 
        ggplot(aes(x, y, fill = cab)) +
        geom_raster() +
        scale_fill_viridis_c() +
        annotation_scale(plot_unit = "m", height = unit(.1, "cm")) +
        annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
        labs(y = "Lat", 
             x = "Long", 
             title = TeX("Predicted C_{ab} map"), 
             fill = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$)")) +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal",
              plot.title = element_text(hjust = .5),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())

hist_cab = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cab) %>% 
        ggplot(aes(x = cab)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$)"), 
             y = "Frequency", 
             title = TeX("Distribution of predicted C_{ab} values")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5))

# cw
map_cw = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cw) %>% 
        ggplot(aes(x, y, fill = cw)) +
        scale_fill_viridis_c() +
        geom_raster() +
        annotation_scale(plot_unit = "m", height = unit(.1, "cm")) +
        annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
        labs(y = "Lat", 
             x = "Long", 
             title = TeX("Predicted C_{w} map"),
             fill = TeX("C_{w} ($\\frac{g}{cm^2})")) +
        theme_bw() +
        theme(legend.position = c(.8, .15),
              legend.direction = "vertical",
              plot.title = element_text(hjust = .5),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())


hist_cw = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cw) %>% 
        ggplot(aes(x = cw)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = TeX("C_{w} ($\\frac{g}{cm^2})"), 
             y = "Frequency",
             title = TeX("Distribution of predicted C_{w} values")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

# cm
map_cm = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, cm) %>% 
        ggplot(aes(x, y, fill = cm)) +
        geom_raster() +
        annotation_scale(plot_unit = "m", height = unit(.1, "cm")) +
        annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
        scale_fill_viridis_c() +
        labs(y = "Lat",
             x = "Long", 
             title = TeX("Retrieved C_{m} map"),
             fill = TeX("C_{m} ($\\frac{g}{cm^2})")) +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal",
              plot.title = element_text(hjust = .5),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())


hist_cm = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(cm) %>% 
        ggplot(aes(x = cm)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cm", y = "Frequency", title = "Distribution of retrieved Cm values") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5))

# lai
map_lai = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(x, y, lai) %>% 
        ggplot(aes(x, y, fill = lai)) +
        geom_raster() +
        scale_fill_viridis_c() +
        annotation_scale(plot_unit = "m", height = unit(.1, "cm")) +
        annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering) +
        labs(title = TeX("Retrieved LAI_{s} map"), 
             fill = TeX("LAI_{s}")) +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal",
              plot.title = element_text(hjust = .5),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())

hist_lai = prisma_biomap_df %>% 
        as_tibble() %>% 
        dplyr::select(lai) %>% 
        ggplot(aes(x = lai)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = TeX("LAI_{s}"),
             y = "Frequency", 
             title = TeX("Distribution of predicted LAI_{s} values")) +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5))



















