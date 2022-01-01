# set the path of  your project folder 
setwd("...")

# grab our list of file-names to combine together all the tweets collected for the last few weeks 
filenames <- list.files(path = "./raw_data/", pattern='*.csv')

# read in all those files into one giant data.frame
df <- do.call("rbind", lapply(filenames, function(f){read.csv(paste("./raw_data/" ,f,sep = ""))}))

# check how many unique tweet we have 

length(unique(df$status_id))

# Remove duplicates based on status_id columns
df_new <- df[!duplicated(df$status_id),]

# export the combined data-frame to CSv file
write.csv(df_new, "processed_data/combined_csv.csv", row.names = FALSE)


