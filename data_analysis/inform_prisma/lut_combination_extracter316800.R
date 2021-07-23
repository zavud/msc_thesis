# variables for PROSPECT5
Cab = seq(20, 60, length.out = 15)
Cw = seq(0.0035, 0.035, length.out = 10)
Cm = seq(0.008, 0.03, length.out = 11)

# variables for 4SAIL
LAI = seq(0.001, 6.5, length.out = 16)

# variables for flim
cd = seq(1.5, 8.5, length.out = 3)
d = seq(200, 5000, length.out = 4)

# combinations
combinations = expand.grid(Cab = Cab, Cw = Cw, Cm = Cm, LAI = LAI, cd = cd, d = d) 

# divide the full data into 2 parts ---- this will help overusing CPU and Memory
combinations_1 = combinations[1:(nrow(combinations) / 2),]
combinations_2 = combinations[-(1:(nrow(combinations) / 2)),]

# save the data
readr::write_csv(combinations_1, 
                 file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\inform_prisma\\combinations_data\\combinations_1.txt")
readr::write_csv(combinations_2, 
                 file = "C:\\Users\\zavud\\Desktop\\msc_thesis\\data_analysis\\inform_prisma\\combinations_data\\combinations_2.txt")
