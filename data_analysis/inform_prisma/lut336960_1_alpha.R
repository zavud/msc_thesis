# load libraries
library(ccrtm)
library(hsdar)
library(doParallel)

# set wd
#wd = ".\\data_analysis\\inform_prisma"
#setwd(wd); rm(wd)

# load PRISMA sensor metadata
fwhm_path = ".\\data_analysis\\sensor_metadata\\fwhm_prisma.txt"
wl_path = ".\\data_analysis\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
fwhm = readr::read_csv(fwhm_path, col_names = F)[[1]]
df_prisma_sensor = data.frame(center = wl, fwhm = fwhm)
rm(fwhm_path, wl_path, wl, fwhm)

# load combinations_1 data
combinations = readr::read_csv(".\\data_analysis\\inform_prisma\\combinations_data\\combinations_336960_1.txt")

# inform (alpha)
informpars <-  ccrtm:::defaults.inform5()

# variables of prospect5 -- canopy
#informpars$prospect5$canopy[1] = 3 # N
#informpars$prospect5$canopy[2] = 40 # cab
informpars$prospect5$canopy[3] = 8 # cartenoid
#informpars$prospect5$canopy[4] = 0.011700 # Cw 
#informpars$prospect5$canopy[5] = 0.03 # Cm

# variables of foursail --- canopy
informpars$foursail$canopy[1] = 0.5 # psoil
#informpars$foursail$canopy[2] = 6 # LAI
informpars$foursail$canopy[6] = 0.02 # hspot
informpars$foursail$canopy[7] = 45.43 # tts
informpars$foursail$canopy[8] = 0 # tto
informpars$foursail$canopy[9] = 181.41 # psi

# variables of foursail --- understorey
informpars$foursail$understorey[2] = 0.5

# variables of flim
#informpars$flim[1] = 5 # cd
informpars$flim[2] = 20 # h
#informpars$flim[3] = 700 # d
informpars$flim[5] = 0 # tto
informpars$flim[6] = 45.43 # tts
informpars$flim[7] = 181.41 # psi

# parallel computation
ncores <- detectCores() # number of cores in my computer = 8
registerDoParallel(ncores); rm(ncores)

s_foreach_loop = Sys.time(); s_foreach_loop
lut = foreach(i = 1:nrow(combinations), .packages = c("ccrtm", "hsdar"), .combine = rbind, .inorder = FALSE) %dopar% {
        
        # prospect5
        informpars$prospect5$canopy[1] = combinations[[i, "N"]]
        informpars$prospect5$canopy[2] = combinations[[i, "Cab"]]
        informpars$prospect5$canopy[4] = combinations[[i, "Cw"]]
        informpars$prospect5$canopy[5] = combinations[[i, "Cm"]]
        
        # foursail
        informpars$foursail$canopy[2] = combinations[[i, "LAI"]]
        
        # flim
        informpars$flim[1] = combinations[[i, "cd"]]
        informpars$flim[3] = combinations[[i, "d"]]
        
        # m is modelled spectra
        m = ccrtm:::rtm.inform5(informpars)
        
        # resample the bands into prisma bands
        m = speclib(spectra = m, wavelength = 400:2500)
        m = spectralResampling(m, sensor = df_prisma_sensor)
        m = as.vector(spectra(m))
        
        # add the simulation to the LUT
        c(m,
          combinations[[i, "N"]],
          combinations[[i, "Cab"]], 
          combinations[[i, "Cw"]], 
          combinations[[i, "Cm"]], 
          combinations[[i, "LAI"]],
          combinations[[i, "cd"]],
          combinations[[i, "d"]])
        
}

stopImplicitCluster()
e_foreach_loop = Sys.time()
e_foreach_loop - s_foreach_loop

# save the LUT
dimnames(lut) = NULL
s_writing = Sys.time()
readr::write_csv(dplyr::as_tibble(lut), file = ".\\data_analysis\\inform_prisma\\lut_database\\lut336960_1_inform5_alpha.txt")
e_writing = Sys.time()
e_writing - s_writing