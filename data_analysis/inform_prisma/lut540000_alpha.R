# load libraries
library(ccrtm)
library(hsdar)
library(doParallel)

# variables for PROSPECT5
N = seq(1, 3, length.out = 4)
Cab = seq(20, 80, length.out = 9)
#Car = seq(1, 15, length.out = 3)
Cw = seq(0.0035, 0.035, length.out = 5)
Cm = seq(0.008, 0.03, length.out = 5)

# variables for 4SAIL
LAI = seq(0.001, 6.5, length.out = 10)
psoil = seq(0.01, 1, length.out = 3)

# variables for flim
cd = seq(1.5, 8.5, length.out = 4)
d = seq(200, 5000, length.out = 5)

# combinations
combinations = expand.grid(N = N, Cab = Cab, Cw = Cw, Cm = Cm, LAI = LAI, psoil = psoil, cd = cd, d = d) 

# divide the full data into 2 parts ---- this will help with not overusing CPU and Memory
#combinations_1 = combinations[1:(nrow(combinations) / 2),]
#combinations_2 = combinations[-(1:(nrow(combinations) / 2)),]

# save the data
#readr::write_csv(combinations, 
#                 file = ".\\data_analysis\\inform_prisma\\combinations_data\\combinations_468000.txt")

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

# inform (alpha)
informpars <-  ccrtm:::defaults.inform5()

# variables of prospect5 -- canopy
#informpars$prospect5$canopy[1] = 3 # N
#informpars$prospect5$canopy[2] = 40 # cab
informpars$prospect5$canopy[3] = 8 # cartenoid
#informpars$prospect5$canopy[4] = 0.011700 # Cw 
#informpars$prospect5$canopy[5] = 0.03 # Cm

# variables of foursail --- canopy
#informpars$foursail$canopy[1] = 0.5 # psoil
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
        informpars$foursail$canopy[1] = combinations[[i, "psoil"]]
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
readr::write_csv(dplyr::as_tibble(lut), file = ".\\data_analysis\\inform_prisma\\lut_database\\lut540000_inform5_alpha.txt")
e_writing = Sys.time()
e_writing - s_writing