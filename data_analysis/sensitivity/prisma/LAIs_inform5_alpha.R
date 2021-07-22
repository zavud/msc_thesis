# load libraries
library(ccrtm)
library(tidyverse)
library(hsdar)

# load PRISMA sensor metadata
fwhm_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\sensor_metadata\\fwhm_prisma.txt"
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
fwhm = readr::read_csv(fwhm_path, col_names = F)[[1]]
df_prisma_sensor = data.frame(center = wl, fwhm = fwhm)
rm(fwhm_path, wl_path, fwhm)

# inform (alpha)
informpars <-  ccrtm:::defaults.inform5()

# variables of prospect5 -- canopy
informpars$prospect5$canopy[1] = 3 # N
informpars$prospect5$canopy[2] = 40 # cab
informpars$prospect5$canopy[3] = 8 # cartenoid
informpars$prospect5$canopy[4] = 0.011700 # Cw 
informpars$prospect5$canopy[5] = 0.03 # Cm

# variables of foursail --- canopy
informpars$foursail$canopy[1] = 0.5 # psoil
LAI = seq(0.001, 6.5, length.out = 15)
#informpars$foursail$canopy[2] = 6 # LAI
informpars$foursail$canopy[3] = 2 # TypeLidf
informpars$foursail$canopy[4] = 65 # ALA / Lidfa
informpars$foursail$canopy[6] = 0.02 # hspot
informpars$foursail$canopy[7] = 45.43 # tts
informpars$foursail$canopy[8] = 0 # tto
informpars$foursail$canopy[9] = 181.41 # psi

# variables of foursail --- understorey
informpars$foursail$understorey[2] = 0.5

# variables of flim
informpars$flim[1] = 6 # cd
informpars$flim[2] = 20 # h
informpars$flim[3] = 700 # d
informpars$flim[5] = 0 # tto
informpars$flim[6] = 45.43 # tts
informpars$flim[7] = 181.41 # psi

# run inform
lut = matrix(NA, nrow = length(LAI), ncol = length(wl) + 2)
for (i in seq_len(length(LAI))) {
        informpars$foursail$canopy[2] = LAI[i]
        m = ccrtm:::rtm.inform5(informpars)
        
        # resample the bands into prisma bands
        m = speclib(spectra = m, wavelength = 400:2500)
        m = spectralResampling(m, sensor = df_prisma_sensor)
        m = as.vector(spectra(m))
        
        lut[i, ] = c(m, LAI[i], i)
}

lut %>% 
        as_tibble() %>% 
        setNames(c(wl, "LAI", "sim_number")) %>% 
        pivot_longer(cols = 1:231, 
                     names_to = "wl", 
                     values_to = "reflectance", 
                     names_transform = list(wl = as.numeric)) %>% 
        ggplot(aes(x = wl, y = reflectance, col = LAI, group = sim_number %>% factor())) +
        scale_color_viridis_c() +
        geom_line(size = 1) +
        theme_bw()
