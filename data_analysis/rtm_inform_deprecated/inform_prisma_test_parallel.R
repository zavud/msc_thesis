# load libraries
library(ccrtm)
library(tidyverse)
library(foreach)
library(doParallel)

# set wd
wd = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform"
setwd(wd)

# load default soil data provided by ccrtm
soil_path = getwd() %>% paste(., "input_data\\soil.rda", sep = "//")
load(soil_path)
soil_average = soil %>% 
        as_tibble() %>% 
        mutate(soil_average = (drySoil + wetSoil) / 2) %>% 
        pull(soil_average)

# variables for PROSPECT5
N = 3
Cab = seq(40, 80, length.out = 10)
Car = 8
Cbrown = 0
Cw = seq(0.0035, 0.035, length.out = 10)
Cm = seq(0.008, 0.03, length.out = 10)

# variables for 4SAIL
LIDFa = 65
TypeLidf = 2
LAI = seq(3, 9, length.out = 10)
hspot = 0.02
tts = 45.43
tto = 0
psi = 181.41
psoil = 0.5

# variables for flim
d = 3500
cd = seq(1.5, 8.5, length.out = 10)
h = 20

# combinations
combinations = expand.grid(Cab = Cab, Cw = Cw, Cm = Cm, LAI = LAI, cd = cd) %>% as_tibble()

# calculation of Rg and Rc for FLIM based on Schlerf & Atzberger 2006
Rg_prospect = prospect5(param = c(N = 3,
                                  Cab = 50,
                                  Car = 7, # constant based median value of range 0-15 (range taken from Berger etal)
                                  Cbrown = 0,
                                  Cw = .02,
                                  Cm = .03)) %>% 
        as_tibble()
Rg_sail = foursail(rho = Rg_prospect %>% pull(rho),
                   tau = Rg_prospect %>% pull(tau),
                   bgr = soil_average,
                   param = c(LIDFa = LIDFa,
                             TypeLidf = TypeLidf,
                             LAI = 6,
                             hspot = hspot,
                             tts = tts,
                             tto = tto,
                             psi = psi,
                             psoil = psoil)) %>% 
        as_tibble()
Rg = Rg_sail %>% pull(rsot)

Rc_prospect = prospect5(param = c(N = 3,
                                  Cab = 50,
                                  Car = 7, # constant based median value of range 0-15 (range taken from Berger etal)
                                  Cbrown = 0,
                                  Cw = .02,
                                  Cm = .03)) %>% 
        as_tibble()
Rc_sail = foursail(rho = Rc_prospect %>% pull(rho),
                   tau = Rc_prospect %>% pull(tau),
                   bgr = soil_average,
                   param = c(LIDFa = LIDFa,
                             TypeLidf = TypeLidf,
                             LAI = 16,
                             hspot = hspot,
                             tts = tts,
                             tto = tto,
                             psi = psi,
                             psoil = psoil)) %>% 
        as_tibble()
Rc = Rc_sail %>% pull(rsot)

# remove unnecessary variables from the memory
rm(Rg_prospect, Rg_sail, Rc_prospect, Rc_sail)

# inform simulation
#LUT_columns = c(400:2500, "Cab", "Cw", "Cm", "LAI", "cd")
#LUT = matrix(NA, nrow = nrow(combinations), ncol = length(LUT_columns))

# parallel computation
ncores <- detectCores() # number of cores in my computer
registerDoParallel(ncores)



s_foreach_loop = Sys.time()
LUT = foreach(i = 1:nrow(combinations), .packages = c("ccrtm", "tidyverse"), .combine = rbind, .inorder = FALSE) %dopar% {
        
        # prospect5
        m_prospect5 = prospect5(param = c(N = N,
                                          Cab = combinations[[i, "Cab"]],
                                          Car = Car,
                                          Cbrown = Cbrown,
                                          Cw = combinations[[i, "Cw"]],
                                          Cm = combinations[[i, "Cm"]])) %>% 
                as_tibble()
        
        # foursail
        m_foursail = foursail(rho = m_prospect5 %>% pull(rho),
                              tau = m_prospect5 %>% pull(tau),
                              bgr = soil_average,
                              param = c(LIDFa = LIDFa,
                                        TypeLidf = TypeLidf,
                                        LAI = combinations[[i, "LAI"]],
                                        hspot = hspot,
                                        tts = tts,
                                        tto = tto,
                                        psi = psi,
                                        psoil = psoil)) %>% 
                as_tibble() %>% 
                select(taus, tauo)
        
        # flim
        m_flim = flim(Rc = Rc,
                      Rg = Rg,
                      To = m_foursail %>% pull(tauo),
                      Ts = m_foursail %>% pull(taus),
                      params = c(d = d,
                                 cd = combinations[[i, "cd"]],
                                 h = h,
                                 tts = tts,
                                 tto = tto,
                                 psi = psi))
        
        # simulated reflectance
        ref = m_flim[[1]] %>% as.vector()
        parameters = c(combinations[[i, "Cab"]],
                       combinations[[i, "Cw"]],
                       combinations[[i, "Cm"]],
                       combinations[[i, "LAI"]],
                       combinations[[i, "cd"]])
        
        # add the simulation to the LUT
        c(ref, parameters)
        
}
stopImplicitCluster()
e_foreach_loop = Sys.time()
e_foreach_loop - s_foreach_loop

