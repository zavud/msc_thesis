Parameter = c("Average leaf inclination angle",
              "Leaf area index", "Hot spot parameter", 
              "Solar zenith angle", "Observer zenith angle", "Sun-sensor azimuth angle",
              "Soil brightness", "Leaf reflectance", "Leaf transmittance", "Backroung reflectance")

Symbol = c("ALIA", "LAI", "Hot", "tts", 
           "tto", "psi", "$\\alpha_{soil}$",
            "$\\rho$", "$\\tau$", "bgr")

Unit = c("$^{\\circ}$", "$\\frac{m^2}{m^2}$", "$\\frac{m}{m}$",
         "$^{\\circ}$", "$^{\\circ}$", "$^{\\circ}$", "-",
         "$nm$", "$nm$", "$nm$")

foursail_df = data.frame(Parameter, Symbol, Unit)
rm(Parameter, Symbol, Unit)
