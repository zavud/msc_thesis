#options(scipen=100000)

Parameter = c("Chlorophyll content",
              "Equivalent water thickness",
              "Leaf dry matter content", 
              "Leaf area index (single)", 
              "Stem density",
              "Crown diameter")
Abbreviation = c("$C_{ab}$", "$C_{w}$", "$C_{m}$", "$LAI_{s}$", "$SD$", "$CD$")
Unit = c("$\\frac{\\mu g}{cm^2}$", 
          "$\\frac{g}{cm^2}$",
         "$\\frac{g}{cm^2}$", 
         "$\\frac{m^2}{m^2}$", 
         "$ha^{-1}$",
         "$m$")
Minimum = c(20, 0.0035, 0.008, 0, 200, 1.5)
Maximum = c(60, 0.035, 0.03, 7, 5000, 8.5)

var_df = data.frame(Parameter, "Abbrev." = Abbreviation, Unit, Min = as.character(Minimum), Max = as.character(Maximum))
rm(Parameter, Abbreviation, Unit, Minimum, Maximum)
