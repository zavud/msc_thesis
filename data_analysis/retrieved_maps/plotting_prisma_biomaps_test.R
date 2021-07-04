# plot prisma Cab image
g_cab_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(x, y, cab) %>% 
        ggplot(aes(x, y, fill = cab)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cab map", fill = "Cab") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cab_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(cab) %>% 
        ggplot(aes(x = cab)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cab", y = "Frequency", title = "Distribution of retrieved Cab values",
             subtitle = "Range in the simulation : 40 - 80") +
        theme_bw()
g_cab_map + g_cab_hist        

# cw
g_cw_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(x, y, cw) %>% 
        ggplot(aes(x, y, fill = cw)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cw map", fill = "Cw") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cw_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(cw) %>% 
        ggplot(aes(x = cw)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cw", y = "Frequency", title = "Distribution of retrieved Cw values",
             subtitle = "Range in the simulation : 0.0035 - 0.035") +
        theme_bw()
g_cw_map + g_cw_hist

# cm
g_cm_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        filter(cm  > 0) %>% 
        select(x, y, cm) %>% 
        ggplot(aes(x, y, fill = cm)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved Cm map", fill = "Cm",
             subtitle = "Note: pixels with Cm < 0 was removed") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_cm_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(cm) %>% 
        ggplot(aes(x = cm)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cm", y = "Frequency", title = "Distribution of retrieved Cm values",
             subtitle = "Range in the simulation : 0.008 - 0.03") +
        theme_bw()

g_cm_map + g_cm_hist

# lai
g_lai_map = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(x, y, lai) %>% 
        filter(lai < 20) %>% 
        ggplot(aes(x, y, fill = lai)) +
        geom_raster() +
        scale_fill_viridis_c() +
        labs(y = "Lat", x = "Long", title = "Retrieved LAI map", fill = "LAI",
             subtitle = "Note: pixels with LAI > 20 was removed") +
        theme_bw() +
        theme(legend.position = c(.8, .1),
              legend.direction = "horizontal")


g_lai_hist = prisma_biomap_df %>% 
        as_tibble() %>% 
        select(lai) %>% 
        ggplot(aes(x = lai)) +
        geom_histogram(bins = 50, fill = "darkgreen") +
        labs(x = "Cm", y = "Frequency", title = "Distribution of retrieved LAI values",
             subtitle = "Range in the simulation : 3 - 9") +
        theme_bw()
g_lai_map + g_lai_hist















