library(tidyverse)

# load the desis hdr
hdr_path = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\satellite_data\\DESIS\\data\\DESIS-HSI-L2A-DT0444838140_013-20200422T071751-V0210\\DESIS-HSI-L2A-DT0444838140_013-20200422T071751-V0210-SPECTRAL_IMAGE.hdr"
hdr = readLines(hdr_path)

# extract the wl of desis
wl_desis = hdr[253:487] %>% 
        str_remove_all(pattern = "[,}]") %>% 
        as.numeric()

# extract the fwhm of desis
fwhm_desis = hdr[489:723] %>% 
        str_remove_all(pattern = "[,}]") %>% 
        as.numeric()

# save the data
write.table(wl_desis,
            file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_desis.txt",
            sep = ",",
            row.names = F,
            col.names = F)
write.table(fwhm_desis,
            file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\fwhm_desis.txt",
            sep = ",",
            row.names = F,
            col.names = F)
