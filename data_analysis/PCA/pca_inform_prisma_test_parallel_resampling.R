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
        step_pca(all_predictors())

pca_prep = prep(pca_recipe)

pca_tidy = tidy(pca_prep, 2)

# extract the sd information
sdev = pca_prep$steps[[2]]$res$sdev
percent_variation = sdev^2 / sum(sdev^2)
df = data.frame(PC=paste0("PC",1:10),
             var_explained=percent_variation[1:10],
             stringsAsFactors = FALSE)
df %>%
        mutate(PC = fct_inorder(PC)) %>%
        ggplot(aes(x=PC,y=var_explained))+geom_col()
