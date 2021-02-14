#Bring in Libraries
library(rvest)
library(dplyr)
library(plyr)
library(stringr)
library(data.table)

#URL: 
url <- "https://www.fueleconomy.gov/feg/PowerSearch.do?action=PowerSearch&year1=2000&year2=2022&cbmkJeep=Jeep&minmsrpsel=0&maxmsrpsel=0&city=0&highway=0&combined=0&cbvtgasoline=Gasoline&YearSel=2000-2022&MakeSel=&MarClassSel=&FuelTypeSel=&VehTypeSel=Gasoline&TranySel=&DriveTypeSel=&CylindersSel=&MpgSel=000&sortBy=&Units=&url=SearchServlet&opt=new&minmsrp=0&maxmsrp=0&minmpg=0&maxmpg=0&sCharge=&tCharge=&startstop=&cylDeact=&rowLimit=200"

#PUll in html data
html <- read_html(url)

#find and create raw table
table_raw = html_table(html_nodes(html,"table"),header = T, fill = T)

#export import information to data frame
df_raw = data.frame(table_raw[1])

#[row,column]

#drop extra columns
df_new <- df_raw[,-grep("NA.",colnames(df_raw))]
df_new <- df_new[,(names(df_new)) %in% c(names(df_new)[1:4])]

#to add extra row rbind(dt,c("Vehicle","NumCyl","EngineSize","Transmission","Turbo","MpgCombined", "MpgCity","MpgHwy"))

#create an empty data frame to populate via loop

df <- data.frame(matrix(ncol = 11, nrow = 0))
x <- c("Year","VehicleName","WD","Turbo","EngineSize","NumCyl","Transmission","FuelType","MpgCombined", "MpgCity","MpgHwy")
colnames(df) <- x

#Parse out sub car details from smushed together elements, year, name, wd
get_car_sub_details <- function(x){
  first_element_length = str_length(x[1])
  year = substr(x[1],1,4)
  if(str_to_upper(substr(x[1],first_element_length-1,first_element_length))=="WD"){
    wheelDrive = substr(x[1],first_element_length-2,first_element_length)
    name = substr(x[1],5,first_element_length-3)
  }else{
    wheelDrive = "NA"
    name = substr(x[1],5,first_element_length)
  }
  return(c(year,name,wheelDrive))
}

#Begin building out the beginning of the car entry to the dataframe
build_car_entry <- function(x){
  car_details = strip_spec_chars(x)
  #[yearnamewheel,enginesize,cylinders,transmission,optional(turbo),type]
  print(paste0("Working on ", car_details[1]))
  sub_car_details = get_car_sub_details(car_details)
  #[year,name,wheeldrive]
  if(length(car_details)==6&&car_details[5]=="Turbo"){
    sub_car_details<- c(sub_car_details,"1")
    car_details <- car_details[-5]
  }else{
    sub_car_details<- c(sub_car_details,"0")
  }
  car_details <- car_details[-1] #remove the first element because we split it up earlier
  car_details <- c(sub_car_details,car_details)
  return(car_details)
}

#Strip random html elements that are still hanging around. 
strip_spec_chars <- function(x){
  new_str = str_remove_all(x,"[\t\r]")
  new_str = str_remove_all(new_str,"[ ]")
  new_str = str_replace_all(new_str,"[\n]",",")
  str_list = strsplit(new_str,",")[[1]]
  return(str_list)
}


#looping through rows
#going to need an index to keep track of current car in new data frame
for(i in 1:length(df_new[,1])){
  if(i == 1){
    curr_car = df_new[1,1]
    curr_df_index = 1
    car_dets <- build_car_entry(curr_car)
    #[year,name,wd,class(turbo),engine,cyl,transmission,gastype]
    df[curr_df_index,]<-c(car_dets,"MpgCombined","City","HWY")
  }
  if(df_new[i,1]!="" 
     && df_new[i,1]!=curr_car
     && str_length(df_new[i,1])>25
     )
    {
    curr_df_index = curr_df_index+1
    curr_car = df_new[i,1]
    car_dets <- build_car_entry(curr_car)
    df[curr_df_index,] <- c(car_dets,"MpgCombined","City","HWY")
    
  }
  
  #Pattern if it says city on row i and column j, then the value for city mpg is on row i-1 and column j
  for(j in 1:length(df_new[1,])){
    if(str_to_upper(df_new[i,j])=="CITY"){
      city_mpg = df_new[i-1,j]
      print(paste0("city mpg is: ",city_mpg))
      df[curr_df_index,10] = city_mpg
    }
    if(str_to_upper(df_new[i,j])=="HWY"){
      hwy_mpg = df_new[i-1,j]
      print(paste0("HWY mpg is: ",hwy_mpg))
      df[curr_df_index,11] = hwy_mpg
    }
    if(str_to_lower(substr(df_new[i,j],1,str_length("combined")))=="combined"){
      combined_mpg = df_new[i-1,j]
      print(paste0("combined mpg is: ",combined_mpg))
      df[curr_df_index,9] = combined_mpg
    }
  }
}



df_raw[4,2]
df_raw[5,2]
