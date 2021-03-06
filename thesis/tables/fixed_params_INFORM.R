Parameter = c("Leaf structure parameter", 
              "Chlorophyll content", 
              "Leaf carotenoid content",
              "Brown Pigment Content", 
              "Equivalent water thickness",
              "Leaf dry matter content",
              "Average leaf inclination angle",
              "Leaf area index (single)",
              "Leaf area index (understorey)",
              "Hot spot parameter", 
              "Solar zenith angle", 
              "Observer zenith angle", 
              "Sun-sensor azimuth angle",
              "Soil brightness", 
              "Stem density",
              "Crown diameter", 
              "Mean Height",
              "Fraction of diffuse incoming",
              "Soil reflectance spectrum")

Abbr = c("$N$", 
                 "$C_{ab}$", 
                 "$C_{ar}$", 
                 "$C_{brown}$", 
                 "$C_{w}$", 
                 "$C_{m}$", 
                 "$ALIA$",
                 "$LAI_{s}$",
                 "$LAI_{u}$",
                 "$Hot$", 
                 "$tts$", 
                 "$tto$", 
                 "$psi$", 
                 "$\\alpha_{soil}$", 
                 "$SD$", 
                 "$CD$", 
                 "$H$",
                 "$skyl$",
                 "$B_{g}$")
Unit = c("$-$", 
         "$\\frac{\\mu g}{cm^2}$", 
         "$\\frac{\\mu g}{cm^2}$",
         "$-$",
         "$\\frac{g}{cm^2}$", 
         "$\\frac{g}{cm^2}$",
         "$^{\\circ}$", 
         "$\\frac{m^2}{m^2}$",
         "$\\frac{m^2}{m^2}$",
         "$\\frac{m}{m}$",
         "$^{\\circ}$",
         "$^{\\circ}$", 
         "$^{\\circ}$", 
         "$-$",
         "$ha^{-1}$", 
         "$m$", 
         "$m$",
         "$-$",
         "$-$")
Value = c(3, 
          40, 
          8, 
          0.001, 
          0.011700, 
          0.03, 
          65,
          6,
          0.5,
          0.02,
          45.43, 
          0, 
          181.41, 
          0.5, 
          700, 
          5, 
          20,
          0.1,
          "default")

fixed_df = data.frame(Parameter, Abbr, Unit, Value = as.character(Value))
rm(Parameter, Abbreviation, Unit, Value)





