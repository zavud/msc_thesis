prosail = DiagrammeR::grViz("digraph {
                  
graph [layout = dot, rankdir = LR, nodesep = 0.6]                  
                  
# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen, fontname = Helvetica, penwidth = 2.0]                  

# prospect5
leafp [label = 'Leaf \n parameters', shape = folder, fillcolor = Beige]
p5 [label = 'PROSPECT5']
leaf_ref [label = 'Leaf \n reflectance', shape = cylinder]
leaf_tr [label = 'Leaf \n transmittance', shape = cylinder]

# 4sail
canopyp [label = 'Canopy & Geometric \n parameters', shape = folder, fillcolor = Beige]
foursail [label = '4SAIL']
simref [label = 'Canopy \n reflectance', shape = cylinder]

#prosail
leafp -> p5 -> {leaf_ref leaf_tr} -> foursail -> simref
canopyp -> foursail                  
                  
}", height="100%", width="100%")