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

# randomly pick a pixel from the image and a spectra from the lut
# i_lut = sample(x = nrow(lut), size = 1)
# i_prisma = sample(x = nrow(prisma_df), size = 1)

g1_no_noise = tibble(wl = wl,
       LUT = lut[147018, 1:231] |> as.numeric(),
       PRISMA = prisma_df[92640, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL,
             title = "Simulated spectra") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

g2_no_noise = tibble(wl = wl,
       LUT = lut[158610, 1:231] |> as.numeric(),
       PRISMA = prisma_df[44061, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL) +
        theme_bw() +
        theme(legend.position = c(.8, .8))

g3_no_noise = tibble(wl = wl,
       LUT = lut[46923, 1:231] |> as.numeric(),
       PRISMA = prisma_df[28420, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1, alpha = .8) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL,
             title = "Simulated spectra") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

# add 3% noise to the simulated spectra
sds = apply(lut[, 1:231], 1, sd) # sd of each spectra
lut[, 1:231] = lut[, 1:231] + rnorm(n = ncol(lut[, 1:231]) * nrow(lut[, 1:231]), mean = 0, sd = sds * .03)

g1_yes_noise = tibble(wl = wl,
                     LUT = lut[147018, 1:231] |> as.numeric(),
                     PRISMA = prisma_df[92640, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL,
             title = "Simulated spectra + 3% Gaussian noise") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

g2_yes_noise = tibble(wl = wl,
                     LUT = lut[158610, 1:231] |> as.numeric(),
                     PRISMA = prisma_df[44061, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL) +
        theme_bw() +
        theme(legend.position = c(.8, .8))

g3_yes_noise = tibble(wl = wl,
                     LUT = lut[46923, 1:231] |> as.numeric(),
                     PRISMA = prisma_df[28420, ] |> as.numeric()) %>% 
        pivot_longer(cols = -wl,
                     names_to = "spectra",
                     values_to = "reflectance") %>% 
        ggplot(aes(x = wl, y = reflectance, col = spectra)) +
        geom_line(size = 1, alpha = .8) +
        labs(x = "Wavelength (nm)", 
             y = "Reflectance",
             col = NULL,
             title = "Simulated spectra + 3% Gaussian noise") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

#g1_no_noise / g1_yes_noise + plot_annotation(tag_levels = "a")
#g2_no_noise / g2_yes_noise + plot_annotation(tag_levels = "a")
g3_no_noise / g3_yes_noise + plot_annotation(tag_levels = "a")
