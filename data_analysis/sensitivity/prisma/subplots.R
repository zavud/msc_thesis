library(patchwork)
files = list.files("C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\sensitivity\\prisma",
                   full.names = T, pattern = "inform")
purrr::walk(files, source)
(g_cab + g_cw) / (g_cm + g_lais) / (g_cd + g_d)
