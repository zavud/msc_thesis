library(tidyverse)
ann_results = data.frame(NN = c("NN with PCs", "NN with sim. bands"),
           train_mse = c("0.1049012", "0.07522584"),
           train_mae = c("0.1710693", "0.13174930"),
           val_mse = c("0.1068904", "0.07499193"),
           val_mae = c("0.1740899", "0.13256425"),
           test_mse = c("0.1071568", "0.07670474"),
           test_mae = c("0.1739102", "0.13459726")) %>% 
        as_tibble() %>% 
        pivot_longer(cols = 2:last_col(),
                     names_to = "metric",
                     values_to = "loss") %>% 
        pivot_wider(names_from = NN, values_from = loss) %>% 
        separate(col = metric, into = c("Dataset", "Metric"), sep = "_") %>% 
        mutate(Metric = str_to_upper(Metric),
               Dataset = str_to_title(Dataset),
               Dataset = ifelse(str_detect(Dataset, "Val"), "Validation", Dataset))
