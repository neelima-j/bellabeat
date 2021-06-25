#Reading all files in input directory into distinct dataframes
for ( file in list.files(path = "~/Documents/case_study_capstone/Raw Data/Fitabase Data 4.12.16-5.12.16")){
  path <- paste0("~/Documents/case_study_capstone/Raw Data/Fitabase Data 4.12.16-5.12.16/",file) #path of file
  name <- paste0(substr(file,1,nchar(file)-4) )                    #name of dataframe
  assign(name, read_csv(path))
  print(name)}