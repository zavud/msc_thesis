# load libraries 
library(tidymodels)
library(tidyverse)
library(raster)

# desis wl
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_desis.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]

# load the desis 04 data
desis_path = list.files(path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\DESIS",
                        pattern = ".envi$",
                        full.names = T)
desis_4 = raster::brick(desis_path[1])

# load the study area polygon
shp = rgdal::readOGR("C:\\Users\\zavud\\Desktop\\LIST\\vector_study_area_rectangle")
shp = shp[shp$fid==1,]
crs(shp) = crs(desis_4[[55]])

# mask out the out-of study area pixels
desis_4_masked = raster::mask(desis_4, mask = shp)

# convert the desis image to df
desis_4_df = desis_4_masked %>% raster::as.data.frame(na.rm = T) %>% as_tibble() %>% setNames(wl)

# make a recipe
pca_rec = recipe(~., desis_4_df) %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 6)
pca_prep = pca_rec %>% prep()
pca_tidy = tidy(pca_prep, 2)

# variations explained
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
tibble(component = unique(pca_tidy$component)[1:6],
       variation = percent_variation[1:6]) %>% 
        ggplot(aes(x = component, y = variation, fill = component)) +
        geom_col(show.legend = F) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        labs(title = "DESIS - Variation explained by the first 6 PCs",
             subtitle = expression("Cumulative variation of 6 PCs " %~~% "97%"),
             x = "Principal Component",
             y = "Variation explained") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5))
pca_desis = bake(pca_prep, new_data = NULL)
