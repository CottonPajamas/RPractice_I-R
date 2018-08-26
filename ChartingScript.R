

#!/usr/bin/env Rscript
rm(list=ls())


# 1. Set directory
tempwd <- temp
#setwd(tempwd)
#Check: tempwd
print(tempwd)


# 2. Set library directory
.libPaths(paste0(tempwd, "/RLibraries"))
libLocation <- .libPaths()[1]
#Check: libLocation
print(libLocation)



# 3. Install packages to the designated package and libraries folder
#Retrieve list of all installed packages
listPack <- installed.packages(lib.loc = libLocation)[,"Package"]
#Check: listPack


#If package not found, it will be installed accordingly
if (!"RODBC" %in% listPack) {
  install.packages("RODBC", repos='http://cran.us.r-project.org', lib=libLocation,
                   destdir=(paste(tempwd,"\\RPackages", sep = "")))
  print("Installing package: RODBC")
}
if (!"forecast" %in% listPack) {
  install.packages("forecast", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"), lib=libLocation,
                   destdir=(paste(tempwd,"\\RPackages", sep = "")))
  print("Installing package: forecast")
}




# 4. Initialize all the required libraries
library(RODBC, lib.loc=libLocation)
library(forecast, lib.loc=libLocation)


# 5. Set our item
tempID <- 'C006'




# 7. Retrieving the entire list of past weekly demand for the current item
#Open connection
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;
                                  Server=127.0.0.1; Database=Bookshop; Uid=<<user>>; Pwd=<<password>>; trusted_connection=yes")

#Retrieving our primary data
sData <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[ActualDataByItem] @ItemID=", tempID))

#Checking total values retrieved are zero or less than 26 seasons (aka a new or relatively new product - 26 weeks is apprx 6mths)
if (nrow(sData) == 0 || nrow(sData) < 26) {
  
  #For such items, they will be compared to existing items in the database that already has a reasonable amt of past data 
  #and belong to a similar product category
  
  #Retrieve its categoryID
  itemCatID <- as.vector(listItems$CategoryID)[x]
  
  #Compare against those already in the list to identify any with a similar categoryID
  for (y in 1:nrow(listItems)) {
    
    #Ensure its not comparing against itself
    if (x != y) {
      tempCatID <- as.vector(listItems$CategoryID)[y]
      
      if (itemCatID == tempCatID) {
        
        #If similar, will use the similar pdt's past data
        sData <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[ActualDataByItem] @ItemID=", as.vector(listItems$ItemID)[y]))
        
        #If data is retrieved successfully (aka not null), then escape from the loop
        if (nrow(sData) != 0) {
          
          #Retrieve the second item's ID and name
          tempID2 <- as.vector(listItems$ItemID)[y]
          tempName2 <- as.vector(listItems$Description)[y]
          
          #Retrieving earliest period from the earliest season
          ePeriod <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[Earliest_PnS] @ItemID=", tempID2))
          
          #Retrieving latest period from the latest season
          lPeriod <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[Latest_PnS] @ItemID=", tempID2))
          
          check <- TRUE
          break
        }
      }
    }
  }
} else {
  #Retrieving earliest period from the earliest season
  ePeriod <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[Earliest_PnS] @ItemID=", tempID1))
  
  #Retrieving latest period from the latest season
  lPeriod <- sqlQuery(dbconnection, paste0("EXEC [SA45Team12AD].[dbo].[Latest_PnS] @ItemID=", tempID1))
}


#Close connection
odbcClose(dbconnection)








