# load libraries
library(ccrtm)
library(tidyverse)
library(hsdar)
library(latex2exp)

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
Cab = seq(20, 60, length.out = 15)
informpars$prospect5$canopy[3] = 8 # cartenoid
informpars$prospect5$canopy[4] = 0.011700 # Cw 
informpars$prospect5$canopy[5] = 0.03 # Cm

# variables of foursail --- canopy
informpars$foursail$canopy[1] = 0.5 # psoil
informpars$foursail$canopy[2] = 6 # LAI
informpars$foursail$canopy[3] = 2 # TypeLidf
informpars$foursail$canopy[4] = 65 # ALA / Lidfa
informpars$foursail$canopy[6] = 0.02 # hspot
informpars$foursail$canopy[7] = 45.43 # tts
informpars$foursail$canopy[8] = 0 # tto
informpars$foursail$canopy[9] = 181.41 # psi

# variables of foursail --- understorey
informpars$foursail$understorey[2] = 0.5

# variables of flim
informpars$flim[1] = 5 # cd
informpars$flim[2] = 20 # h
informpars$flim[3] = 700 # d
informpars$flim[5] = 0 # tto
informpars$flim[6] = 15 # tts
informpars$flim[7] = 45 # psi

# run inform
lut = matrix(NA, nrow = length(Cab), ncol = length(wl) + 2)
for (i in seq_len(length(Cab))) {
        informpars$prospect5$canopy[2] = Cab[i]
        m = ccrtm:::rtm.inform5(informpars)
        
        # resample the bands into prisma bands
        m = speclib(spectra = m, wavelength = 400:2500)
        m = spectralResampling(m, sensor = df_prisma_sensor)
        m = as.vector(spectra(m))
        
        lut[i, ] = c(m, Cab[i], i)
}

g_cab = lut %>% 
        as_tibble() %>% 
        setNames(c(wl, "Cab", "sim_number")) %>% 
        pivot_longer(cols = 1:231, 
                     names_to = "wl", 
                     values_to = "reflectance", 
                     names_transform = list(wl = as.numeric)) %>% 
        ggplot(aes(x = wl, y = reflectance, col = Cab, group = sim_number)) +
        geom_line() +
        labs(x = "Wavelength (nm)", y = "Simulated Canopy Reflectance",
             col = TeX("C_{ab} ($\\frac{\\mu g}{cm^2}$)"),
             title = TeX("a) Chlorophyll content (C_{ab} $\\frac{\\mu g}{cm^2}$)")) +
        scale_color_viridis_c() +
        theme_bw() +
        theme(legend.position = c(.8, .9),
              legend.direction = "horizontal")
