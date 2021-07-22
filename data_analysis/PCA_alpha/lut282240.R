# load libraries
library(tidymodels)
library(tidyverse)
library(raster)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "cd", "d")

# load prisma data
prisma_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::brick(prisma_path)
prisma_df = raster::as.data.frame(prisma, na.rm = T) %>% as_tibble() %>% setNames(wl)

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\inform_prisma\\lut_database"
lut_file = list.files(path = lut_path, pattern = "282240", full.names = T)
lut_1 = read_csv(lut_file[1]) %>% setNames(nms)
lut_2 = read_csv(lut_file[2]) %>% setNames(nms)

# combine the 2 datasets
lut = bind_rows(lut_1, lut_2)

# remove the lut_1 and lut_2 from the environment
rm(lut_1, lut_2)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# add 5% noise to the simulated spectra
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


lut %>% 
        dplyr::select(-(232:last_col())) %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn = mean(reflectance),
                  mn_ps = mn + sd(reflectance),
                  mn_ms = mn - sd(reflectance)) %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "stat",
                     values_to = "stat_value") %>% 
        ggplot(aes(x = as.numeric(wl), y = stat_value, col = stat)) +
        geom_line(size = 1)

prisma_df %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn = mean(reflectance),
                  mn_ps = mn + sd(reflectance),
                  mn_ms = mn - sd(reflectance)) %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "stat",
                     values_to = "stat_value") %>% 
        filter(stat_value > 0) %>% 
        ggplot(aes(x = as.numeric(wl), y = stat_value, col = stat)) +
        geom_line()

# divide the data into train/val/test sets
data_split = initial_split(lut, prop = .90)
training_val = training(data_split)
training = training_val[1:230000,]
validation = training_val[-c(1:230000), ]
testing = testing(data_split)
rm(training_val)

# build a PCA model
pca_recipe = recipe(training, ~.) %>% 
        update_role(232:last_col(), new_role = "id") %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 4)
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
sum(percent_variation[1:4])

# apply the transformation to validation and test sets
pca_training = bake(pca_prep, new_data = NULL)
pca_validation = bake(pca_prep, new_data = validation)
pca_testing = bake(pca_prep, new_data = testing)

# apply the transofrmation on the prisma image
pca_prisma = bake(pca_prep, new_data = prisma_df)

range(pca_prisma$PC4)
range(pca_training$PC4)

# save the PCA data sets
write_csv(pca_training, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_inform_alpha_training302500.txt")
write_csv(pca_validation, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_inform_alpha_validation302500.txt")
write_csv(pca_testing, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_inform_alpha_testing302500")
write_csv(pca_prisma, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\pca_inform_alpha_prisma302500.txt")

