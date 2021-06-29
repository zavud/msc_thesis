# load libraries
library(tidymodels)
library(tidyverse)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "cd")

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases\\LUT_inform_test.txt"
lut = read_csv(lut_path) %>% setNames(nms)

# build PCA model
pca_recipe = recipe(~., data = lut) %>%
        update_role(232:236, new_role = "id") %>% 
        step_normalize(all_predictors()) %>% 
        step_pca(all_predictors(), num_comp = 10)

pca_prep = prep(pca_recipe)

pca_tidy = tidy(pca_prep, 2)

# variations explained
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
tibble(component = unique(pca_tidy$component)[1:5],
       variation = percent_variation[1:5]) %>% 
        ggplot(aes(x = component, y = variation)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent_format())
predict(pca_prep, lut[1:5, ])
