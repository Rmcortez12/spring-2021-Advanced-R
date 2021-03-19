#Bring in Libraries
library(rvest)
library(dplyr)
library(plyr)
library(stringr)
library(data.table)
install.packages("xlsx")
library("xlsx")

#URL: 
#this url actually has almost all the data we need
#we just need to loop through the pages
#url <- "https://www.fueleconomy.gov/feg/PowerSearch.do?action=noform&year1=2000&year2=2022&cbmkJeep=Jeep&minmsrpsel=0&maxmsrpsel=0&city=0&hwy=0&comb=0&cbvtgasoline=Gasoline&YearSel=2000-2022&make=&mclass=&vfuel=&vtype=Gasoline&trany=&drive=&cyl=&MpgSel=000&sortBy=&Units=&url=SearchServlet&opt=new&minmsrp=0&maxmsrp=0&minmpg=0&maxmpg=0&sCharge=&tCharge=&startstop=&cylDeact=&rowLimit=200&pageno=1&tabView=0"
#url1 <- "https://www.fueleconomy.gov/feg/PowerSearch.do?action=noform&year1=2000&year2=2022&cbmkJeep=Jeep&minmsrpsel=0&maxmsrpsel=0&city=0&hwy=0&comb=0&cbvtgasoline=Gasoline&YearSel=2000-2022&make=&mclass=&vfuel=&vtype=Gasoline&trany=&drive=&cyl=&MpgSel=000&sortBy=&Units=&url=SearchServlet&opt=new&minmsrp=0&maxmsrp=0&minmpg=0&maxmpg=0&sCharge=&tCharge=&startstop=&cylDeact=&rowLimit=200&pageno=2&tabView=0"
#There's 111 pages.... 
#pageno=2&tabView=0

#create an empty data frame to populate via loop
# df <- data.frame(matrix(ncol = 11, nrow = 0))
# x <- c("Year","VehicleName","WD","EngineMod","EngineSize","NumCyl","Transmission","FuelType","MpgCombined", "MpgCity","MpgHwy")
# colnames(df) <- x


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
  #[yearnamewheel,enginesize,cylinders,transmission,optional(turbo/supercharged),type]
  #print(paste0("Working on ", car_details[1]))
  sub_car_details = get_car_sub_details(car_details)
  #[year,name,wheeldrive]
  if(length(car_details)==6&&car_details[5]=="Turbo"){
    sub_car_details<- c(sub_car_details,"1")
    car_details <- car_details[-5]
  }else if (length(car_details)==6 &&car_details[5]=="Supercharger"){
    sub_car_details<- c(sub_car_details,"2")
    car_details <- car_details[-5]
  } else{
    sub_car_details<- c(sub_car_details,"0")
  }
  if(!str_detect(car_details[3],"cyl")){
    car_details <- c(car_details[1:2],"2cyl",car_details[3:length(car_details)])
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

#PUll in html data and parse into dataframe
web_scraper <- function(x){
  x <- url(x,"rb")
  html <- read_html(x)
  close(x)
  print("done pulling data")
  
  #find and create raw table
  table_raw = html_table(html_nodes(html,"table"),header = T, fill = T)
  
  #export import information to data frame
  df_raw = data.frame(table_raw[1])
  #print("I'm here now!")
  #[row,column]
  #to make this run faster just cut it off here and put the below into a another function that we can run once all the data has been pulled

  #drop extra columns
  df_new <- df_raw[,-grep("NA.",colnames(df_raw))]
  df_new <- df_new[,(names(df_new)) %in% c(names(df_new)[1:4])]
  #print("here I am")
  #to add extra row rbind(dt,c("Vehicle","NumCyl","EngineSize","Transmission","Turbo","MpgCombined", "MpgCity","MpgHwy"))
  
  #looping through rows
  #going to need an index to keep track of current car in new data frame
  for(i in 1:length(df_new[,1])){
    if(i == 1){
      curr_car = df_new[1,1]
      curr_df_index = 1
      car_dets <- build_car_entry(curr_car)
      #print(car_dets)
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
      #print(car_dets)
      df[curr_df_index,] <- c(car_dets,"MpgCombined","City","HWY")
      
    }
    
    #Pattern if it says city on row i and column j, then the value for city mpg is on row i-1 and column j
    for(j in 1:length(df_new[1,])){
      if(str_to_upper(df_new[i,j])=="CITY"){
        city_mpg = df_new[i-1,j]
        #print(paste0("city mpg is: ",city_mpg))
        df[curr_df_index,10] = city_mpg
      }
      if(str_to_upper(df_new[i,j])=="HWY"){
        hwy_mpg = df_new[i-1,j]
        #print(paste0("HWY mpg is: ",hwy_mpg))
        df[curr_df_index,11] = hwy_mpg
      }
      if(str_to_lower(substr(df_new[i,j],1,str_length("combined")))=="combined"){
        combined_mpg = df_new[i-1,j]
        #print(paste0("combined mpg is: ",combined_mpg))
        df[curr_df_index,9] = combined_mpg
      }
    }
  }
  return(df)
}

#there are 197 pages

#base_url <- "https://www.fueleconomy.gov/feg/PowerSearch.do?action=noform&year1=1976&year2=2022&cbmkJeep=Jeep&minmsrpsel=0&maxmsrpsel=0&city=0&hwy=0&comb=0&cbvtgasoline=Gasoline&YearSel=2000-2022&make=&mclass=&vfuel=&vtype=Gasoline&trany=&drive=&cyl=&MpgSel=000&sortBy=&Units=&url=SearchServlet&opt=new&minmsrp=0&maxmsrp=0&minmpg=0&maxmpg=0&sCharge=&tCharge=&startstop=&cylDeact=&rowLimit=200&pageno="

#init_url <- paste0(base_url,1)

#data <- web_scraper(init_url)
# 
# for(i in 2:197){
#   x <- paste0(base_url,i)
#   print(paste0("working on page ",i)) 
#   data <- rbind(data,web_scraper(x))
#   #insert a sleep for closures and to mimic person behavior
#   #Sys.sleep(2)
# }



test_data <- data

#Regex to get make and then get model
pattern_function <- function(y,x){
  reg_exp_attributes = regexpr(pattern=y,x)
  match_length = attr(reg_exp_attributes,'match.length')
  model = regmatches(x,regexpr(pattern=y,x))
  make = substr(x,match_length+1,str_length(x))
  make = ifelse(str_length(make)>0,make,model)
  return(c(model,make))
}

get_make <-function(x){
  a = make_model_func(x)
  return(a[1])
}
get_model <-function(x){
  a = make_model_func(x)
  return(a[2])
}


make_model_func <- function(x){
  
  
  if(substring(x, 0 ,3)=="BMW"){
    pattern = "[B][M][W]"
    info_list <- pattern_function(pattern,x)
  }
  else if(substring(x, 0 ,3)=="GMC"){
    pattern <-  "[G][M][C]"
    info_list <- pattern_function(pattern,x)
  }
  else if(str_to_upper(substring(x,0,4))=="MINI"){
    pattern <- "[M][I][N][I]"
    info_list <- pattern_function(pattern,x)
  }else if(str_to_upper(substring(x,0,5))=="SMART"){
    pattern <- "[s][m][a][r][t][f][o][r][t][w][o]"
    info_list <- pattern_function(pattern,x)
  }
  else if(str_to_upper(substring(x,0,5))=="SCION"){
    pattern <- "[S][c][i][o][n]"
    info_list <- pattern_function(pattern,x)
  }
  else if(str_to_upper(substring(x,0,4))=="AUDI"){
    pattern <- "[A][u][d][i]"
    info_list <- pattern_function(pattern,x)
  }
  else if(str_to_upper(substring(x,0,5))=="ISUZU"){
    pattern <- "[I][s][u][z][u]"
    info_list <- pattern_function(pattern,x)
  }
  else{
    pattern = "[A-Z]+([a-z]+[-][A-Z][a-z]+|[a-z]+)"
    info_list <- pattern_function(pattern, x)
  }
  return(info_list)
}
test_data <- read.csv("webdata.csv")

test_data$VehicleMake <- sapply(test_data$VehicleName, get_make)
test_data$VehicleModel <- sapply(test_data$VehicleName,get_model)

#notice there's still some makes and models together
#BMW is a big one
unique(sort(test_data$VehicleMake))

ford_Df = test_data[which(test_data$VehicleMake=="Ford"),]

#write data to a csv so we don't have to re-pull every time
write.csv(test_data,file = "webdata.csv")


#Retrieve data from wikipedia for CAFE Standards
wiki <- read_html("https://en.wikipedia.org/wiki/Corporate_average_fuel_economy#:~:text=The%20program%20covered%20model%20year,of%2025%20miles%20per%20gallon.")
wiki_table = html_table(html_nodes(wiki,"table"),header = T, fill = T)
wiki_df = data.frame(wiki_table[3])
names(wiki_df)
wiki_names <- c('Year','PassengerDomCars','PassengerImportCars','LightTrucks2wd','LightTrucks4wd','LightTrucksCombined')
names(wiki_df)<-wiki_names
wiki_df <-wiki_df[2:41,]
wiki_df[,1]<- sapply(wiki_df[,1],function(x){substr(x,1,4)}) # need to get rid of special characters and just keep years

#problem classifying cars vs trucks vs vans

#Found better data set at https://afdc.energy.gov/data/10562 for cafe
cafe_df <- read.xlsx2('cafe.xlsx',2,header = T)
detach("package:xlsx", unload = TRUE)
plot(as.character(cafe_df$Year),as.character(cafe_df$Passenger),type="l",xlab = "Year", ylab = "CAFE MPG Standard")
points(as.character(cafe_df$Year),as.character(cafe_df$LightTrucks), type = "l",col="red")
points(ford_Df$Year,ford_Df$MpgCombined)
grid(NULL,NULL,lty = 6)

#need to get mean year mpg for all brands

make_stats <- data.frame()
df_make <- data.frame()

for(make in unique(sort(test_data$VehicleMake))){
  df_make <- test_data[which(test_data$VehicleMake==make),]
  for(year in unique(sort(df_make$Year))){
    d <- as.numeric(df_make$MpgCombined[df_make$Year==year])
    make_stats <- rbind(make_stats,data.frame(year,make,mean(d),var(d),median(d)))
  }
}

make_stats$year <- as.character(make_stats$year)
sort_data_year <- make_stats[order(as.character(make_stats$year)),]
plot(as.factor(sort_data_year$year),sort_data_year$mean.d.,ylab = "Combined MPG",xlab = "Year")
plot(sort_data_year[which(sort_data_year$make == "Acura"),]$year,sort_data_year[which(sort_data_year$make == "Acura"),]$mean.d.,ylab = "Combined MPG",xlab = "Year", main="Acura",type="l")

plot(sort_data_year[which(sort_data_year$make == "Ford"),]$year,sort_data_year[which(sort_data_year$make == "Ford"),]$mean.d.,ylab = "Combined MPG",xlab = "Year", main="Ford",type="l")


require(data.table)
dt <- setDT(sort_data_year)

dt_avg <- dt[,.(yearly_avg=mean(mean.d.)),by="year"]
dt_avg <- sapply(dt_avg,as.numeric)

plot(dt_avg,xlab="Year",ylab="MPG",main = "MPG by Year For all Makes and Models")
lines(lowess(dt_avg,delta = .00001),col="purple")
abline(v=2008, lty=2,col="blue")
#abline(v=2012, lty=2,col="red")
abline(v=2016, lty=2,col="red")
abline(v=2020, lty=2,col="blue")

#enhancements
#get number of cars sold
#break into categories for cars/trucks/suv's
