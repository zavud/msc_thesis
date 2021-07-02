library(tidyverse)
library(tidymodels)
library(raster)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]

# PCA on PRISMA data
prisma_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\PRISMA\\2020_09_11_masked\\prisma_cropped_masked_study_area.envi"
prisma = raster::stack(prisma_path)
prisma_df = raster::as.data.frame(prisma) %>% as_tibble() %>% drop_na() %>% setNames(wl)

pca_recipe = recipe(prisma_df, ~.) %>%
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 6)
pca_prep = pca_recipe %>% prep()
pca_tidy = tidy(pca_prep, 2)

# variations explained
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
tibble(component = unique(pca_tidy$component)[1:6],
       variation = percent_variation[1:6]) %>% 
        ggplot(aes(x = fct_inorder(component), y = variation, fill = component)) +
        geom_col(show.legend = F) +
        scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
        labs(title = "Variation explained by the first 6 PCs",
             subtitle = expression("Cumulative variation of 6 PCs " %~~% "99%"),
             x = "Principal Component",
             y = "Variation explained") +
        theme_bw() +
        theme(plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(hjust = .5))


# get the full transformed data
pca_prisma = bake(pca_prep, new_data = NULL)






















