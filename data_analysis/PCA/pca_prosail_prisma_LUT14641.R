# load libraries
library(tidymodels)
library(tidyverse)
library(raster)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI")

# load prisma data
prisma_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::brick(prisma_path)
prisma_df = raster::as.data.frame(prisma, na.rm = T) %>% as_tibble() %>% setNames(wl)

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases"
lut_files = list.files(path = lut_path, pattern = "14641", full.names = T)
lut = read_csv(lut_files) %>% setNames(nms)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# add 3% noise to the simulated spectra
sds = apply(lut[, 1:231], 1, sd) # sd of each spectra
lut[, 1:231] = lut[, 1:231] + rnorm(n = ncol(lut[, 1:231]) * nrow(lut[, 1:231]), mean = 0, sd = sds * .05)

# compare simulated and image spectra
ggplot(data = data.frame(lut = lut[sample(x = nrow(lut), size = 1), 1:231] %>% as.numeric(),
                         prisma = prisma_df[sample(x = nrow(prisma_df), size = 1), ] %>% as.numeric(),
                         wl = wl),
       mapping = aes(x = wl)) +
        geom_line(aes(y = lut, color = "Lut"),  size = 1.2, alpha = 1/2) +
        geom_line(aes(y = prisma,  color = "PRISMA"), size = 1.2, alpha = 1/2) +
        scale_color_manual(values = c("Lut"="red", "PRISMA"="blue")) +
        labs(x = "Wavelength (nm)", y = "Reflectance")

# divide the data into train/val/test sets
data_split = initial_split(lut, prop = .8)
training_val = training(data_split)
training = training_val[1:10000,]
validation = training_val[-c(1:10000), ]
testing = testing(data_split)
rm(training_val)

# build a PCA model
pca_recipe = recipe(training, ~.) %>% 
        update_role(232:235, new_role = "id") %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 6)
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
             subtitle = expression("Cumulative variation of 6 PCs " %~~% "100%"),
             x = "Principal Component",
             y = "Variation explained") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5))

sum(percent_variation[1:6])

# apply the transformation to validation and test sets
pca_training = bake(pca_prep, new_data = NULL)
pca_validation = bake(pca_prep, new_data = validation)
pca_testing = bake(pca_prep, new_data = testing)

# apply the transofrmation on the prisma image
pca_prisma = bake(pca_prep, new_data = prisma_df)

range(pca_prisma$PC1)
range(pca_training$PC1)

# save the PCA data sets
write_csv(pca_training, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_prosail_training14641.txt")
write_csv(pca_validation, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_prosail_validation14641.txt")
write_csv(pca_testing, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_prosail_testinh14641.txt")
