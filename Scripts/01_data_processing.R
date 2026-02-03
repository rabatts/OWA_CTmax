#adapted from code written by Matt Sasaki


#This script process the raw data and saves the processed data
#Packages are loading in script 02_controller.R


#Here each of the three set of transplants and measurements in source environment is 
#a different data set
data_sets <- dir(path="Data/raw")

cumul_data = data.frame()

for (i in 1:length(data_sets)){

  # i=1
  
  full_data = data.frame()
  temp_record = data.frame()
  ramp_record = data.frame()
  
  temp_data <- dir(path=paste("Data/raw/",data_sets[i], "/temp_data/", sep =""))
  data_files <- str_remove(temp_data, pattern = "_temp.CSV") #returns just date from run
  
  #runs for each run that has a date in temp data  
  for(j in 1:length(data_files)){
    #j=3
    paste("j is", j)
    file_name <- data_files[j] 
    #print(file_name)
    
   
    temp_data = read_csv(paste("Data/raw/",data_sets[i], "/temp_data/", file_name, "_temp.CSV", collapse = "", sep = ""), show_col_types = FALSE) 
    
    cols <- colnames(temp_data)
    cols_new <- c("Date"=cols[1],"Time"=cols[2], "Temp1"=cols[3], 
                  "Temp2"=cols[4], "Temp3"=cols[5])
    
    #if the date went from 11:59 pm to 12:01am accounds for the fact that Time resets at 00:00

    temp_data <- temp_data %>% 
      rename( ., all_of(cols_new))%>%
      mutate("Time" = lubridate::hms(Time),
             "Date" = lubridate::as_date(Date)) 
    
    if(temp_data$Time[1] > temp_data$Time[length(temp_data$Time)]){
      for(l in 1:length(temp_data$Time)){
        if(temp_data$Time[l]<temp_data$Time[1]){
          temp_data$Time[l] <- temp_data$Time[l]+period(hours = 24)
        }
      }
    }
    
    temp_data <- temp_data  %>% 
      mutate("time_point" = row_number(), # Assigns each time point a sequential value
             "second_passed" = lubridate::time_length(Time - first(Time)), # Calculates the time passed in seconds since logging began
             "minute_passed" = second_passed / 60,
             "minute_interval" = floor(second_passed / 60)) %>% # Integer math to convert from seconds since logging began to minute time interval 
      pivot_longer(cols = c(Temp1, Temp2, Temp3), # Pivots data set so there's only one column of temperature data
                   names_to = "sensor",
                   values_to = "temp_C") %>% ungroup()
    
    
    
    #Important:: a delay is only needed if the temperature logger has a raspberryPi. The temperature logger was switch to a version without
    #a rasberryPi at the begining of May. 
    # If a delay is applied but not needed, then the highest time tubes in a data set may have an NA for their CTmax values 
     
    trial_date <- ymd(file_name)
    cutoff_date <- ymd("2023_05_01") #after which there was no delay
    
    if(trial_date > cutoff_date){
      delay = FALSE
    }else{
      delay = TRUE
    }
    
    if(delay==TRUE){
      time_data = read_csv(paste("Data/raw/",data_sets[i], "/time_data/", file_name, "_time.csv", collapse = "", sep = ""), show_col_types = FALSE) %>% 
        drop_na(minute) %>%
        mutate(time = (minute + (second / 60)) - 2.1, # Accounts for the two minute start up delay in the temperature logger, changed from 2 to 2.1 to account for delay between plugging in temp logger and starting stop watch
               "rank" = dense_rank(desc(time)))
    }else{
      time_data = read_csv(paste("Data/raw/",data_sets[i], "/time_data/", file_name, "_time.csv", collapse = "", sep = ""), show_col_types = FALSE) %>% 
        drop_na(minute) %>%
        mutate(time = (minute + (second / 60)), 
               "rank" = dense_rank(desc(time)))
        }
    
        
    setup <- read.csv(paste("Data/raw/",data_sets[i], "/setups/", file_name, "_setup.csv", collapse = "", sep = ""), na.strings='')
  
    #preliminary data fram from which ramp_rate is calculated
    min_ramp = temp_data  %>% 
      group_by(sensor, minute_interval) %>% 
      group_modify(~ data.frame(
        "ramp_per_second" = unclass(
          coef(lm(data = .x, temp_C ~ second_passed))[2]))) %>% # Calculates rate of change for each sensor during each of the minute time intervals
      mutate(ramp_per_minute = ramp_per_second * 60, # Converts from change per second to change per minute
             run = j) # Gives each run a unique numeric ID
    
    # added an = to preven the time interval from which CTmax was calculated from being too small and falling between time intervals new logger had a 10 sec interval v 5 second of original 
    
    ind_measurements = time_data %>% 
      group_by(tube) %>% 
      summarise("ctmax" = mean(filter(temp_data, minute_passed >= (time - (0.1 * rank)) & minute_passed < time)$temp_C), # Average temperature of the uncertainty window for each individual
                "ramp_rate" = mean(filter(min_ramp, minute_interval > (time - 5) & minute_interval < time)$ramp_per_minute))
    
    #needed if the logging interval is greater than 6 seconds
    for(k in 1:nrow(ind_measurements)){
      if(is.na(ind_measurements$ctmax[k])){
        
        ind_measurements$ctmax[k] <- mean(filter(temp_data, minute_passed >= (time_data$time[k] - 1/6) & minute_passed <= time_data$time[k]+ 1/6)$temp_C)
      }
    }
    
    # "ctmax" = mean(filter(temp_data, minute_passed > (time_data$time - (0.1 * rank)) & minute_passed < time_data$time)$temp_C)
     #temp_data %>% filter(minute_passed >= (time_data$time[i] - 1/6) & minute_passed <= time_data$time[i]+ 1/6)
    
    ct_data = inner_join(time_data, ind_measurements, by = c("tube")) %>% 
      inner_join(dplyr::select(setup, -date), by = c("tube")) %>% 
      mutate(date = lubridate::as_date(str_replace_all(file_name, pattern = "_", "-")),
             run = j) %>% 
      mutate(data_set = data_sets[i], unique_date = file_name) %>% 
      dplyr::select(date, unique_date, data_set, user, run, tube, lineage, replicate, rank, time, ramp_rate, ctmax)
    
    #write.csv(ct_data, file = paste("Outputs/Data/",data_sets[i], "/", file_name, "_ctmax.csv", sep = "", collapse = ""), row.names = F)
    
    if(data_sets[i] == "4_lineages"){
         length_data = read_csv(paste("Data/raw/4_lineages/length_data/", file_name, "_length.csv", collapse = "", sep = ""))
      }else{
        length_data = tibble("tube" = time_data$tube,
                             "length" = NA)
        }
    ct_data <- ct_data %>% 
      inner_join(length_data, by="tube")
    
    cumul_data = bind_rows(cumul_data, ct_data) 
    
    temp_data$run = j
    temp_record = bind_rows(temp_record, temp_data)
    
    ramp_record = bind_rows(ramp_record, min_ramp)
    }

  full_data = cumul_data
 
}

#output includes a row for each observation from every data set
write.csv(x = full_data, file = paste("Data/processed/full_data.csv", sep = ""),row.names = F)
write.csv(x = temp_record, file = paste("Data/processed/temp_record.csv", sep = ""), row.names = F)
write.csv(x = ramp_record, file = paste("Data/processed/ramp_record.csv", sep = ""), row.names = F)


