## create frequency vector from Brown corpus

data(Brown.tfl, package = "zipfR")

data_integer_brownfreq <- Brown.tfl$f
names(data_integer_brownfreq) <- Brown.tfl$type

save(data_integer_brownfreq, file = "R_package/data/data_integer_brownfreq.RData", compression_level = 9)
