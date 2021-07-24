Parameter = c("Canopy reflectance at infinite depth",
              "Background reflectance",
              "Transmission in viewing direction",
              "Transmission in sun direction",
              "Stand density",
              "Crown diameter",
              "Mean crown height",
              "Solar zenith angle",
              "Observer zenith angle",
              "Sun-sensor azimuth angle")

Symbol = c("$R_{c}$",
           "$R_{g}$",
           "$T_{o}$",
           "$T_{s}$",
           "SD", 
           "CD", 
           "H",
           "tts",
           "tto",
           "psi")

Unit = c(rep("$nm$", 4),
         "$ha^{-1}$", 
         "$m$", 
         "$m$",
         rep("$^{\\circ}$", 3))
flim_df = data.frame(Parameter, Symbol, Unit)
rm(Parameter, Symbol, Unit)
