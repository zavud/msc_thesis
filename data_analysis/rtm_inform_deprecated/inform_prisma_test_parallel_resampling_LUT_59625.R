# load libraries
library(ccrtm)
library(hsdar)
library(doParallel)

# set wd
wd = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform"
setwd(wd); rm(wd)

# load PRISMA sensor metadata
fwhm_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\fwhm_prisma.txt"
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
fwhm = readr::read_csv(fwhm_path, col_names = F)[[1]]
df_prisma_sensor = data.frame(center = wl, fwhm = fwhm)
rm(fwhm_path, wl_path, wl, fwhm)

# load default soil data provided by ccrtm
soil_path = paste(getwd(), "input_data\\soil.rda", sep = "//")
load(soil_path)
soil = soil[,1]
rm(soil_path)

# variables for PROSPECT5
Cab = seq(30, 90, length.out = 15)
Cw = seq(0.0035, 0.035, length.out = 15)
Cm = seq(0.008, 0.03, length.out = 15)

# variables for 4SAIL
LAI = seq(0.001, 10, length.out = 15)


# variables for flim
#cd = seq(1.5, 8.5, length.out = 10)

# combinations
combinations = expand.grid(Cab = Cab, Cw = Cw, Cm = Cm, LAI = LAI) 

# calculation of Rg and Rc for FLIM based on Schlerf & Atzberger 2006
Rg_prospect = prospect5(param = c(N = 3,
                                  Cab = 50,
                                  Car = 7, # constant based median value of range 0-15 (range taken from Berger etal)
                                  Cbrown = 0,
                                  Cw = .02,
                                  Cm = .03))

Rg_sail = foursail(rho = Rg_prospect[,1],
                   tau = Rg_prospect[,2],
                   bgr = soil,
                   param = c(LIDFa = 65,
                             TypeLidf = 2,
                             LAI = 6,
                             hspot = 0.02,
                             tts = 45.43,
                             tto = 0,
                             psi = 181.41,
                             psoil = 0.5))
Rg = Rg_sail[,4]
rm(Rg_prospect, Rg_sail)

Rc_prospect = prospect5(param = c(N = 3,
                                  Cab = 50,
                                  Car = 7, # constant based median value of range 0-15 (range taken from Berger etal)
                                  Cbrown = 0,
                                  Cw = .02,
                                  Cm = .03))

Rc_sail = foursail(rho = Rc_prospect[,1],
                   tau = Rc_prospect[,2],
                   bgr = soil,
                   param = c(LIDFa = 65,
                             TypeLidf = 2,
                             LAI = 16,
                             hspot = 0.02,
                             tts = 45.43,
                             tto = 0,
                             psi = 181.41,
                             psoil = 0.5))

Rc = Rc_sail[,4]
rm(Rc_prospect, Rc_sail)

# parallel computation
ncores <- detectCores() # number of cores in my computer = 8
registerDoParallel(ncores); rm(ncores)

s_foreach_loop = Sys.time(); s_foreach_loop
LUT = foreach(i = 1:nrow(combinations), .packages = c("ccrtm", "hsdar"), .combine = rbind, .inorder = FALSE) %dopar% {
        
        # prospect5
        m = prospect5(param = c(N = 3,
                                Cab = combinations[[i, "Cab"]],
                                Car = 8,
                                Cbrown = 0,
                                Cw = combinations[[i, "Cw"]],
                                Cm = combinations[[i, "Cm"]]))
        
        # foursail
        m = foursail(rho = m[,1],
                     tau = m[,2],
                     bgr = soil,
                     param = c(LIDFa = 65,
                               TypeLidf = 2,
                               LAI = combinations[[i, "LAI"]],
                               hspot = 0.02,
                               tts = 45.43,
                               tto = 0,
                               psi = 181.41,
                               psoil = 0.5))
        m = m[, 5:6] # keep only taus (5) and tauo (6)
        
        # flim
        m = flim(Rc = Rc,
                 Rg = Rg,
                 To = m[, 2], # tauo
                 Ts = m[, 1], # taus
                 params = c(d = 3500,
                            cd = 5,
                            h = 20,
                            tts = 45.43,
                            tto = 0,
                            psi = 181.41))
        
        # simulated reflectance
        m = as.vector(m[[1]])
        
        # current variable values
        parameters = c(combinations[[i, "Cab"]],
                       combinations[[i, "Cw"]],
                       combinations[[i, "Cm"]],
                       combinations[[i, "LAI"]])
        
        # resample the bands into prisma bands
        m = speclib(spectra = m, wavelength = 400:2500)
        m = spectralResampling(m, sensor = df_prisma_sensor)
        m = as.vector(spectra(m))
        
        # add the simulation to the LUT
        c(m, parameters)
        
}
stopImplicitCluster()
e_foreach_loop = Sys.time()
e_foreach_loop - s_foreach_loop

# save the LUT
dimnames(LUT) = NULL
s_writing = Sys.time()
readr::write_csv(as.data.frame(LUT), file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases\\LUT_inform_50625_test.txt")
e_writing = Sys.time()
e_writing - s_writing
