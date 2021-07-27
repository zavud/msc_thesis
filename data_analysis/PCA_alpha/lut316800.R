# load libraries
library(tidymodels)
library(tidyverse)
library(raster)
library(patchwork)

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

# build a PCA model
pca_recipe = recipe(training, ~.) %>% 
        update_role(232:last_col(), new_role = "id") %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 5)
pca_prep = pca_recipe %>% prep()
pca_tidy = tidy(pca_prep, 2)

# variations explained
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
g_scree = tibble(component = unique(pca_tidy$component)[1:5],
       variation = percent_variation[1:5]) %>% 
        ggplot(aes(x = component, y = variation, fill = component)) +
        geom_col(show.legend = F) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        labs(title = "Variation explained by the first 5 PCs",
             x = "Principal Component",
             y = "% Variation explained") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5))

sum(percent_variation[1:5])

# cumulative variation
g_cum = tibble(pc = unique(pca_tidy$component)[1:5],
       cumvar = cumsum(percent_variation[1:5])) %>% 
        ggplot(aes(x = pc, y = cumvar, group = 1)) +
        geom_line(col = "#CC79A7", size = 1) +
        geom_point(col = "darkgreen", size = 2) +
        labs(x = "Principal Component",
             y = "% Variation explained",
             title = "Cumulative variance explained by the first 5 PCs") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5))

g_scree + g_cum + plot_annotation(tag_levels = "a")


# apply the transformation to validation and test sets
pca_training = bake(pca_prep, new_data = NULL)
pca_validation = bake(pca_prep, new_data = validation)
pca_testing = bake(pca_prep, new_data = testing)

# apply the transofrmation on the prisma image
pca_prisma = bake(pca_prep, new_data = prisma_df)

range(pca_prisma$PC1)
range(pca_testing$PC1)
range(pca_training$PC1)

# save the PCA data sets
write_csv(pca_training, file = ".\\data_analysis\\prisma_training_database\\pca_inform_alpha_training316800.txt")
write_csv(pca_validation, file = ".\\data_analysis\\prisma_training_database\\pca_inform_alpha_validation316800.txt")
write_csv(pca_testing, file = ".\\data_analysis\\prisma_training_database\\pca_inform_alpha_testing316800")
write_csv(pca_prisma, file = ".\\data_analysis\\prisma_training_database\\pca_inform_alpha_prisma316800.txt")

