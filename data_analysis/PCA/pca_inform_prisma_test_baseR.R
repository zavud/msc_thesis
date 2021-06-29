# load libraries
library(tidyverse)

# load training and testing data sets
database_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database"
data_sets = list.files(path = database_path, full.names = T)
training = data_sets[2] %>% read_csv()
testing = data_sets[1] %>% read_csv()

# run pca with base r
pc = prcomp(training[, 1:231], center = T, scale. = T)

training_pca = pc$x %>% as_tibble() %>% select(PC1, PC2) %>% bind_cols(training[, 232:ncol(training)])
testing_pca = predict(pc, newdata = testing[, 1:231]) %>% as_tibble() %>% select(PC1, PC2) %>% bind_cols(testing[, 232:ncol(testing)])

# save the data
write_csv(training_pca, file = paste0(database_path, "\\prisma_training_pca.txt"))
write_csv(testing_pca, file = paste0(database_path, "\\prisma_testing_pca.txt"))
