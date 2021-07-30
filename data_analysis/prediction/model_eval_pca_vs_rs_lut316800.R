# load libraries
library(tidyverse)
library(keras)
library(patchwork)

# load the saved histories
history_annpca = read_csv(file = "./data_analysis/models/history_ann_pca_lut316800_3h.txt")
history_annrs = read_csv(file = "./data_analysis/models/history_ann_RS_lut316800_3h.txt")

# visualize the training histories
history_annpc_mse = history_annpca %>% 
        filter(metric == "loss") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, 1.5) +
        labs(x = "Epoch number",
             y = "MSE",
             col = NULL,
             title = "Training ANN with PCs") +
        theme_bw() +
        theme(legend.position = c(.8, .8),
              plot.title = element_text(hjust = .5))

history_annpc_mae = history_annpca %>% 
        filter(metric == "mean_absolute_error") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, .7) +
        labs(x = "Epoch number",
             y = "MAE",
             col = NULL,
             title = "Training ANN with PCs") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

history_annrs_mse = history_annrs %>% 
        filter(metric == "loss") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, 0.65) +
        labs(x = "Epoch number",
             y = "MSE",
             col = NULL,
             title = "Training ANN with simulated PRISMA bands") +
        theme_bw() +
        theme(legend.position = c(.8, .8),
              plot.title = element_text(hjust = .5))

history_annrs_mae = history_annrs %>% 
        filter(metric == "mean_absolute_error") %>% 
        ggplot(aes(x = epoch, y = value, col = data)) +
        geom_line(alpha = .7) +
        ylim(0, .43) +
        labs(x = "Epoch number",
             y = "MAE",
             col = NULL,
             title = "Training ANN with PCs") +
        theme_bw() +
        theme(legend.position = c(.8, .8))

history_annpc_mse
history_annrs_mse

