library(tidyverse)

# load prisma wl bands
wl_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt"
wl = readr::read_csv(wl_path, col_names = F)[[1]]
nms = c(wl, "Cab", "Cw", "Cm", "LAI", "cd")

# load the lut data
lut_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\LUT_databases\\LUT_inform_test.txt"
lut = read_csv(lut_path) %>% setNames(nms)

# shuffle the whole dataset randomly
lut = lut %>% slice(sample(1:n()))

# divide the training data into train/test
training = lut[1:70000, ]
testing = lut[-c(1:70000), ]

# save the data sets
write_csv(training, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\prisma_training.txt")
write_csv(testing, file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\prisma_training_database\\prisma_testing.txt")
