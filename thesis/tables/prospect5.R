Parameter = c("Leaf structure parameter", "Chlorophyll content", "Leaf carotenoid content",
              "Brown Pigment Content", "Equivalent water thickness",
              "Leaf dry matter content")
Symbol = c("N", "Cab", "Car", "Cbrown", "Cw", "Cm")
Unit = c("-", "$\\frac{\\mu g}{cm^2}$", "$\\frac{\\mu g}{cm^2}$", "-",
         "$\\frac{g}{cm^2}$", "$\\frac{g}{cm^2}$")

prospect5_df = data.frame(Parameter, Symbol, Unit)
rm(Parameter, Symbol, Unit)