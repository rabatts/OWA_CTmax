#Code written by Matt Sasaki 
# To generate the setup guide:
# Update initial of the user perorming the CTmax assay as well as treatment names and desired occurances with the assay by change lineages

wd <- "C:/Users/eliza/Documents/Research/Warming_plasticity/Data/CTmax"
setwd(wd)
user = "RAB" #First, middle, and last initial

date = Sys.Date() 
lineages = rep(c("OW-1", "AM-2"), times = 5) 
n = 10

guide = data.frame(user, date, tube = c(1:n),
                   lineage = sample(lineages, size = n, replace = F),
                   replicate = NA)

file_name = gsub(x = date, pattern = "-", replacement = "_")

write.csv(guide, file = paste(file_name, "_B_setup", ".csv", sep = ""), row.names = F) #Letters are added if more than one CTmax was perfomred in a day
