# variables for PROSPECT5
N = seq(1, 2.5, length.out = 3)
Cab = seq(30, 80, length.out = 15)
Cw = seq(0.0035, 0.035, length.out = 8)
Cm = seq(0.008, 0.03, length.out = 8)

# variables for 4SAIL
LAI = seq(0.001, 7, length.out = 13)

# variables for flim
cd = seq(1.5, 8.5, length.out = 3)
d = seq(200, 5000, length.out = 3)

# combinations
combinations = expand.grid(N = N, Cab = Cab, Cw = Cw, Cm = Cm, LAI = LAI, cd = cd, d = d) 

# divide the full data into 2 parts ---- this will help with not overusing CPU and Memory
combinations_1 = combinations[1:(nrow(combinations) / 2),]
combinations_2 = combinations[-(1:(nrow(combinations) / 2)),]

# save the data
readr::write_csv(combinations_1, 
                 file = ".\\data_analysis\\inform_prisma\\combinations_data\\combinations_168480_1.txt")
readr::write_csv(combinations_2, 
                 file = ".\\data_analysis\\inform_prisma\\combinations_data\\combinations_168480_2.txt")
