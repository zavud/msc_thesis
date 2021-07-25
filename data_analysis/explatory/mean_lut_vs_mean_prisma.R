# load libraries
library(tidyverse)
library(raster)
library(RColorBrewer)

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
lut_file = list.files(path = lut_path, pattern = "316800", full.names = T)
lut_1 = read_csv(lut_file[1]) %>% setNames(nms)
lut_2 = read_csv(lut_file[2]) %>% setNames(nms)

# combine the 2 datasets
lut = bind_rows(lut_1, lut_2)

# remove the lut_1 and lut_2 from the environment
rm(lut_1, lut_2)

# shuffle the data
lut = lut %>% slice(sample(1:n()))

# data exploration
mean_lut = lut %>% 
        dplyr::select(-(232:last_col())) %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn_lut = mean(reflectance))

mean_prisma = prisma_df %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn_prisma = mean(reflectance))

mean_merged = left_join(mean_lut, mean_prisma, by = "wl")

# comparison
brewer.pal(n = 3, name = "Paired")

mean_merged %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "mean") %>% 
        ggplot(aes(x = as.numeric(wl), y = mean, col = spectra)) +
        geom_line(size = 1, alpha = .9) +
        scale_color_manual(values = c("#1F78B4", "#E7298A"),
                           labels = c("LUT (simulated) spectra", "PRISMA image spectra (study area only)")) +
        labs(x = "Wavelength (nm)",
             y = "Average (mean) reflectance",
             col = "Average reflectance:",
             title = "Average LUT spectra vs average image spectra (study area only)") +
        theme_bw() +
        theme(legend.position = c(.8, .8),
              plot.title = element_text(hjust = .5))

