library(tidyverse)

# load the prisma hdr
hdr_path = "C:\\Users\\zavud\\Desktop\\large_data_for_msc\\PRISMA\\2020_09_11_masked\\PRS_L2D_STD_20200911103206_20200911103210_0001_HCO_FULL_manual_and_Fmasked.hdr"
hdr = readLines(hdr_path)

# extract the wl of prisma
wl_prisma = hdr[41:79] %>% 
        str_trim(side = "both") %>% 
        gsub(pattern = "}", replacement = "") %>% 
        str_split(pattern = ",") %>% 
        unlist() %>% 
        as.numeric()
wl_prisma = wl_prisma[!is.na(wl_prisma)]

# extract the fwhm of prisma
fwhm_prisma = hdr[81:113] %>% 
        str_trim(side = "both") %>% 
        gsub(pattern = "}", replacement = "") %>% 
        str_split(pattern = ",") %>% 
        unlist() %>% 
        as.numeric()
fwhm_prisma = fwhm_prisma[!is.na(fwhm_prisma)]

# save the data
write.table(wl_prisma,
            file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\wl_prisma.txt",
            sep = ",",
            row.names = F,
            col.names = F)
write.table(fwhm_prisma,
            file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\rtm_inform\\sensor_metadata\\fwhm_prisma.txt",
            sep = ",",
            row.names = F,
            col.names = F)
