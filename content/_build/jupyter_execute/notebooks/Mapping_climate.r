# This download will take a couple of minutes as the files are about 60mb each! 

Years_to_download = c(2015,2016)

for (year in Years_to_download){
    
    # download the files that we require
    download.file(paste("ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_precip/precip.",year,".nc", sep=""), paste("../data/precip.",year,".nc",sep=""))
    download.file(paste("ftp://ftp.cdc.noaa.gov/Datasets/cpc_global_temp/tmax.",year,".nc", sep=""), paste("../data/tmax.",year,".nc",sep=""))
    
    # Use R to go to the terminal and call the cdo remapping 
    system(paste("cdo -z zip sellonlatbox,-180,180,-90,90 ../data/precip.", year, ".nc ../data/precip.", year, ".new.nc", sep=""))
    system(paste("cdo -z zip sellonlatbox,-180,180,-90,90 ../data/tmax.", year, ".nc ../data/tmax.", year, ".new.nc", sep=""))
    
}

#install.packages(ncdf4)
#install.packages(chron)
#install.packages(tidyr)

library(ncdf4)
library(chron)
library(tidyr)


# read in the data file without climate data 
Data_no_climate <- read.csv("../data/example_import_data.csv")

Data_climate <- Data_no_climate

# To keep things organised, let's rename the columns 
names(Data_climate)[35] <- "Initial_latitude"
names(Data_climate)[36] <- "Initial_longitude"
names(Data_climate)[28] <- "Collection.date.range"

##
## Please note that the indexes above may be a column out 
## due to including row names column when writing the csv 
## 
## If this doesnt work, please try the following code instead and re run:
#
#names(Data_climate)[34] <- "Initial_latitude"
#names(Data_climate)[35] <- "Initial_longitude"
#names(Data_climate)[27] <- "Collection.date.range"


# A function to round our co-ordinates to the nearest .25 value 
rounding <- function(value){
  # round to the nearest integer
  x <- round(value, digits = 0)
  # if rounded up (x>value) reduce by 0.25
  # else add 0.25 
  new = ifelse(x>value, x-0.25, x+0.25)
  return(new)
}

# A function to round to the other nearest 0.25 
rounding_opposite <- function(value){
  # round to the nearest integer
  x <- round(value, digits = 0)
  # if rounded up (x>value) reduce by 0.25
  # else add 0.25 
  new = ifelse(x>value, x+0.25, x-0.25)
  return(new)
}

# A function to force rounding up if required 
force_round_up <- function(value){
  # round UP the nearest integer 
  # minus 0.25 to back towards value 
  x <- ceiling(value) - 0.25
  return(x)
}


# Round the latitude and longitude using our previous functions
# The new co-ordinates are saved as new columns 
Data_climate$latitude <- rounding(as.numeric(Data_climate$Initial_latitude))
Data_climate$longitude <- rounding(as.numeric(Data_climate$Initial_longitude))


# convert the date 
Data_climate$Collection.date.range <- as.Date(Data_climate$Collection.date.range)

# create a column for the Julian date for later slicing  
Data_climate$Julian <- as.numeric(format(Data_climate$Collection.date.range, "%j"))

# extract year fro mthe date 
Data_climate$Year <- as.numeric(format(Data_climate$Collection.date.range, "%Y"))

# order by date and reset indexes 
Data_climate <- Data_climate[order(Data_climate$Collection.date.range, decreasing = FALSE),]
rownames(Data_climate) <- NULL

# Create a column of NAs to populate 
Data_climate$Precipitation <- NA

# Create a column of NAs to populate 
Data_climate$Max.Temp <- NA



i <- 0
Year1 <- 0

# for each unique year in the dataframe 
for (Year in unique(Data_climate$Year)){
  print(paste("Adding climate data for the year ", Year, sep=""))
  # Draw out the index values in the dataset that correspond to that year 
  Year_Index <- which(Data_climate[,"Year"] == Year)
  
  ###############################################
  # Extract daily precipitation 
  ###############################################
  
  print("Opening NetCDF precipitation file")
  # Define the netCDF file for that year stored in the data directory (please ask for this data as it is very large)
  ncpath_prp <- "../data/"
  ncfname_prp <- paste(ncpath_prp, "precip.", Year, ".new", ".nc", sep="")
  dname_prp <- "precip" 
  
  # open the netCDF file
  ncin_prp <- nc_open(ncfname_prp)
  
  # get longitude and latitude for the factor 
  lon_prp <- ncvar_get(ncin_prp,"lon")
  lat_prp <- ncvar_get(ncin_prp,"lat")
  
  # get the time for that factor 
  time <- ncvar_get(ncin_prp,"time")
  tunits <- ncatt_get(ncin_prp,"time","units")
  nt <- dim(time)
  
  # Create an array for the precipitation values 
  prp_array <- ncvar_get(ncin_prp,dname_prp)
  
  # other factors that may be required/useful
  dlname <- ncatt_get(ncin_prp,dname_prp,"long_name")
  dunits <- ncatt_get(ncin_prp,dname_prp,"units")
  fillvalue <- ncatt_get(ncin_prp,dname_prp,"_FillValue")
  
  # replace netCDF fill values with NA's
  prp_array[prp_array==fillvalue$value] <- NA
  
  # create a matrix for the lon/lats of that file 
  lonlat_prp <- as.matrix(expand.grid(lon_prp,lat_prp))
  
  ###############################################
  # Extract max daily temperature
  ###############################################
  
  print("Opening NetCDF temperature file")
  ncpath_tmp <- "../data/"
  ncfname_tmp <- paste(ncpath_tmp, "tmax.", Year, ".new", ".nc", sep="")
  dname <- "tmax" 
  
  
  # open the netCDF file
  ncin_tmp <- nc_open(ncfname_tmp)
  
  # get longitude and latitude for factor 
  lon_tmp <- ncvar_get(ncin_tmp,"lon")
  lat_tmp <- ncvar_get(ncin_tmp,"lat")
  
  # extract time
  time <- ncvar_get(ncin_tmp,"time")
  tunits <- ncatt_get(ncin_tmp,"time","units")
  nt <- dim(time)
  
  # get temperature 
  tmp_array <- ncvar_get(ncin_tmp,dname)
  
  dlname <- ncatt_get(ncin_tmp,dname,"long_name")
  dunits <- ncatt_get(ncin_tmp,dname,"units")
  fillvalue <- ncatt_get(ncin_tmp,dname,"_FillValue")
  
  # replace netCDF fill values with NA's
  tmp_array[tmp_array==fillvalue$value] <- NA
  
  # define a matric with temperature lon/lats
  lonlat_tmp <- as.matrix(expand.grid(lon_tmp,lat_tmp))
  print(paste("Running for the year ", Year,". There are ", length(Year_Index), " rows, from ", min(Year_Index), "to", max(Year_Index)))
  
  # loop to extract climate data and save
  for (row in Year_Index){
    # define the Julian day for that row 
    j <- Data_climate[row, "Julian"]
    # define the lon/lats of the row 
    lon_n <- Data_climate[row, "longitude"]
    lat_n <- Data_climate[row, "latitude"]
    
    # If the julian day differs, or the year differs from the previous row
    if (j!=i | Year != Year1){
      # redefine i as j 
      i <- j
      # Slice the temp data into the day required
      tmp_slice <- tmp_array[,,i]
      # store as a vector
      tmp_vec <- as.vector(tmp_slice)
      # bind as a dataframe with the lon/lats of the factor 
      tmp_df01 <- data.frame(cbind(lonlat_tmp,tmp_vec))
      # add the temp value for the rows long/lats on tht day  
      Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
      
      # Same again but for precipitation 
      prp_slice <- prp_array[,,i]
      prp_vec <- as.vector(prp_slice)
      prp_df01 <- data.frame(cbind(lonlat_prp,prp_vec))
      Data_climate[row, "Precipitation"] <- with(prp_df01, prp_vec[Var2 == lat_n & Var1 == lon_n])
      
    }
    # otherwise, if Julian day does not differ NOR does the year 
    else{
      # Go straight in and extract from the existing slice for that lon/lat
      Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
      Data_climate[row, "Precipitation"] <- with(prp_df01, prp_vec[Var2 == lat_n & Var1 == lon_n])
      
    }
    # redefine the previous year once the years index has been completed
    
    if (is.na(Data_climate[row, "Max.Temp"])){
      # Try a different latitude
      Data_climate[row, "latitude"] <- rounding(Data_climate[row, "Initial_latitude"])
      Data_climate[row, "longitude"] <- rounding_opposite(Data_climate[row, "Initial_longitude"])
      
      # define the lon/lats of the row
      lon_n <- Data_climate[row, "longitude"]
      lat_n <- Data_climate[row, "latitude"]
      #print(paste("Attempting again for row ", row, "at lon/lat ", lon_n, ",",lat_n))
      
      
      # add the temp value for the rows long/lats on tht day
      Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
      
      
      if (is.na(Data_climate[row,"Max.Temp"])){
        # try a different longitude
        Data_climate[row, "latitude"] <- rounding_opposite(Data_climate[row, "Initial_latitude"])
        Data_climate[row, "longitude"] <- rounding(Data_climate[row, "Initial_longitude"])
        
        # define the lon/lats of the row
        lon_n <- Data_climate[row, "longitude"]
        lat_n <- Data_climate[row, "latitude"]
        #print(paste("Attempting second time for row ", row, "at lon/lat ", lon_n, ",", lat_n))
        
        Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
        
        if (is.na(Data_climate[row,"Max.Temp"])){
          # try a different longitude
          Data_climate[row, "latitude"] <- rounding_opposite(Data_climate[row, "Initial_latitude"])
          Data_climate[row, "longitude"] <- rounding_opposite(Data_climate[row, "Initial_longitude"])
          
          # define the lon/lats of the row
          lon_n <- Data_climate[row, "longitude"]
          lat_n <- Data_climate[row, "latitude"]
          print(paste("Attempting third time for row ", row, "at lon/lat ",lon_n,",",lat_n))
          
          Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
          
          if (is.na(Data_climate[row,"Max.Temp"])){
            # Try a different latitude
            Data_climate[row, "latitude"] <- rounding(Data_climate[row, "Initial_latitude"])
            Data_climate[row, "longitude"] <- force_round_up(Data_climate[row, "Initial_longitude"])
            
            # define the lon/lats of the row
            lon_n <- Data_climate[row, "longitude"]
            lat_n <- Data_climate[row, "latitude"]
            #print(paste("Attempting again for row ", row, "at lon/lat ", lon_n, ",",lat_n))
            
            
            # add the temp value for the rows long/lats on tht day
            Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
            
            if (is.na(Data_climate[row, "Max.Temp"])){
              
              Data_climate[row, "latitude"] <- force_round_up(Data_climate[row, "Initial_latitude"])
              Data_climate[row, "longitude"] <- rounding(Data_climate[row, "Initial_longitude"])
              
              # define the lon/lats of the row
              lon_n <- Data_climate[row, "longitude"]
              lat_n <- Data_climate[row, "latitude"]
              #print(paste("Attempting second time for row ", row, "at lon/lat ", lon_n, ",",lat_n))
              
              
              # add the temp value for the rows long/lats on tht day
              Data_climate[row, "Max.Temp"] <- with(tmp_df01, tmp_vec[Var2 == lat_n & Var1 == lon_n])
            }
          }
        }
      }
    }
    
    Year1 == Year
  } 
  
}



summary(Data_climate$Precipitation)
summary(Data_climate$Max.Temp)

# write.csv(Data_climate, "../data/example_import_data_with_climate.csv", row.names=FALSE)
