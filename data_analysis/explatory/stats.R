# load libraries
library(tidyverse)
library(raster)
library(patchwork)

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
lut_stats = lut %>% 
        dplyr::select(-(232:last_col())) %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn = mean(reflectance),
                  mn_msd = mn - sd(reflectance),
                  mn_psd = mn + sd(reflectance)) %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "stat",
                     values_to = "stat_value")
g_lut_stats = lut_stats %>% 
        ggplot(aes(x = as.numeric(wl),
                   y = stat_value, col = stat)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)",
             y = "Simulated canopy reflectance",
             col = NULL,
             title = "Statistics of simulated canopy reflectance") +
        scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                           labels = c("Mean", "Mean - Standard deviation", "Mean + Standard deviation")) +
        theme_bw() +
        theme(legend.position = c(.8, .8),
              legend.direction = "vertical",
              plot.title = element_text(hjust = .5))

prisma_stats = prisma_df %>% 
        pivot_longer(cols = everything(),
                     names_to = "wl",
                     values_to = "reflectance") %>% 
        group_by(wl) %>% 
        summarise(mn = mean(reflectance),
                  mn_msd = mn - sd(reflectance),
                  mn_psd = mn + sd(reflectance)) %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "stat",
                     values_to = "stat_value")
g_prisma_stats = prisma_stats %>% 
        ggplot(aes(x = as.numeric(wl),
                   y = stat_value, col = stat)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)",
             y = "Canopy reflectance",
             col = NULL,
             title = "Statistics of PRISMA image reflectance (only study area)") +
        scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73"),
                           labels = c("Mean", "Mean - Standard deviation", "Mean + Standard deviation")) +
        theme_bw() +
        theme(legend.position = c(.8, .8),
              legend.direction = "vertical",
              plot.title = element_text(hjust = .5))

# combine the plots for thesis
g_lut_stats / g_prisma_stats + plot_annotation(tag_levels = "a")

