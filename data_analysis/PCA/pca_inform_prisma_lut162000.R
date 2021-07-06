# load libraries
library(tidymodels)
library(tidyverse)
library(raster)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "d", "cd")

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases\\LUT_inform_prisma_162000_test.txt"
lut = read_csv(lut_path) %>% setNames(nms)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# add 5% random noise to each spectra
sds = apply(lut[, 1:231], 1, sd)
lut[, 1:231] = lut[, 1:231] + rnorm(n = nrow(lut[, 1:231]) * ncol(lut[, 1:231]),
                                    mean = 0,
                                    sd = sds * 0.02)

# divide the data into train/val/test sets
data_split = initial_split(lut, prop = .80)
training_val = training(data_split)
training = training_val[1:100000,]
validation = training_val[-c(1:100000), ]
testing = testing(data_split)
rm(training_val)

# build a PCA model
pca_recipe = recipe(training, ~.) %>% 
        update_role(232:237, new_role = "id") %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 5)
pca_prep = pca_recipe %>% prep()
pca_tidy = tidy(pca_prep, 2)

# variations explained
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
tibble(component = unique(pca_tidy$component)[1:6],
       variation = percent_variation[1:6]) %>% 
        ggplot(aes(x = component, y = variation, fill = component)) +
        geom_col(show.legend = F) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        labs(title = "PRISMA LUT - Variation explained by the first 6 PCs",
             subtitle = expression("Cumulative variation of 5 PCs " %~~% "??%"),
             x = "Principal Component",
             y = "Variation explained") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5))

# apply the transformation to validation and test sets
pca_training = bake(pca_prep, new_data = NULL)
pca_validation = bake(pca_prep, new_data = validation)
pca_testing = bake(pca_prep, new_data = testing)

# apply the same transformation to the prisma image
prisma_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::brick(prisma_path)
prisma_df = raster::as.data.frame(prisma, na.rm = T) %>% as_tibble() %>% setNames(wl)
pca_prisma = bake(pca_prep, new_data = prisma_df)

# save the PCA data sets
write_csv(pca_training, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_training_LUT_162000.txt")
write_csv(pca_validation, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_validation_LUT_162000.txt")
write_csv(pca_testing, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_testing_LUT_162000.txt")
