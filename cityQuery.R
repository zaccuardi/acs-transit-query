# Function: given input of a vector of city names [data frame of city names + state abbrevs (could + msa codes?)], 
# and an optional input of city boundary ("city" OR "metro") --> produce output of summary census statistics

cityQuery <- function (city_list, bound="city") {
  require(acs)
  city_long <- paste(city_list$city,"city",sep = " ") 
  # OPTIONAL add weird government naming exceptions
  city_list[nrow(city_list)+1,] <- c("Nashville","TN",34980)
  city_long[length(city_long)+1] <- "Nashville-Davidson metropolitan government"
  
  cities <- geo.make(state=city_list$state, place=city_long)
  metros <- geo.make(msa=city_list$msa)

  # Create empty dataframe, eventually for export
  q.data <- data.frame(city_list$city)
  row.names(q.data) <- city_list$city
  q.data[,1] <- NULL
  
  # Fill dataframe with calls to acs.fetch
  
  if (bound == "metro") {
    pop <- acs.fetch(endyear=2014, span = 1, geography=metros, table.number="B02001", col.names=race.vars)
    q.data[,1:10] <- estimate(pop)
    colnames(q.data) <- race.vars
    
    hispanic <- acs.fetch(endyear=2014,span = 1,geography=metros,variable="B03002_012", check = T, col.names = "hispanic")
    q.data[,11] <- estimate(hispanic)
    colnames(q.data)[11] <- c("hispanic")
    
    income <- acs.fetch(endyear=2014,span=1,geography=metros,table.number="B19013", check = T, col.names = "income")
    q.data[,12] <- estimate(income)
    colnames(q.data)[12] <- c("income")
    
    mode <- acs.fetch(endyear=2011, span=5, geography=metros, variable=transit.vars,col.names=c("total.mode","auto","transit"))
    mode <- apply(mode[,2:3], MARGIN=1, FUN=divide.acs, denominator=mode[,1], method="proportion", verbose=F) # convert to percentage using divide.acs function
    q.data[,13:14] <- estimate(mode)
    colnames(q.data)[13:14] <- c("auto","transit")
    
    industry <- acs.fetch(endyear=2011, span=5, geography=metros, variable = industry.vars, col.names = c("total","male","m.extract","m.const","m.mfr","m.whole",
                                                                                                          "m.retail","m.freight","m.info","m.money","m.prof","m.eduetc",
                                                                                                          "m.artetc","m.oth","m.pub","female","f.extract","f.const","f.mfr",
                                                                                                          "f.whole","f.retail","f.freight","f.info","f.money","f.prof",
                                                                                                          "f.eduetc","f.artetc","f.oth","f.pub"))
    industry <- apply(industry[,2:29], MARGIN=1, FUN=divide.acs, denominator=industry[,1], 
                      method="proportion", verbose=F) # convert to percentage using divide.acs function
    q.data[,15:42] <- estimate(industry)
    colnames(q.data)[15:42] <- c("male","m.extract","m.const","m.mfr","m.whole","m.retail","m.freight","m.info","m.money","m.prof","m.eduetc",
                                 "m.artetc","m.oth","m.pub","female","f.extract","f.const","f.mfr","f.whole","f.retail","f.freight","f.info","f.money","f.prof",
                                 "f.eduetc","f.artetc","f.oth","f.pub")
    
    commute <- acs.fetch(endyear=2013, span=5, geography=metros, table.number="B08303", col.names = c("total","less5","thru9","thru14","thru19","thru24","thru29",
                                                                                                      "thru34","thru39","thru44","thru59","thru89","plus90"))
    commute <- apply(commute[,2:13], MARGIN=1, FUN=divide.acs, denominator=commute[,1], method="proportion", verbose=F)
    q.data[,43:54] <- estimate(commute)
    colnames(q.data)[43:54] <- c("less5","thru9","thru14","thru19","thru24","thru29","thru34","thru39","thru44","thru59","thru89","plus90")
    
    
      } else {
    pop <- acs.fetch(endyear=2014, span = 1, geography=cities, table.number="B02001", col.names=race.vars)
    q.data[,1:10] <- estimate(pop)
    colnames(q.data) <- race.vars
    
    hispanic <- acs.fetch(endyear=2014,span = 1,geography=cities,variable="B03002_012", check = T, col.names = "hispanic")
    q.data[,11] <- estimate(hispanic)
    colnames(q.data)[11] <- c("hispanic")
    
    income <- acs.fetch(endyear=2014,span=1,geography=cities,table.number="B19013", check = T, col.names = "income")
    q.data[,12] <- estimate(income)
    colnames(q.data)[12] <- c("income")
    
    mode <- acs.fetch(endyear=2011, span=5, geography=cities, variable=transit.vars,col.names=c("total.mode","auto","transit"))
    mode <- apply(mode[,2:3], MARGIN=1, FUN=divide.acs, denominator=mode[,1], method="proportion", verbose=F) # convert to percentage using divide.acs function
    q.data[,13:14] <- estimate(mode)
    colnames(q.data)[13:14] <- c("auto","transit")
    
    industry <- acs.fetch(endyear=2011, span=5, geography=cities, variable = industry.vars, col.names = c("total","male","m.extract","m.const","m.mfr","m.whole",
                                                                                                          "m.retail","m.freight","m.info","m.money","m.prof","m.eduetc",
                                                                                                          "m.artetc","m.oth","m.pub","female","f.extract","f.const","f.mfr",
                                                                                                          "f.whole","f.retail","f.freight","f.info","f.money","f.prof",
                                                                                                          "f.eduetc","f.artetc","f.oth","f.pub"))
    industry <- apply(industry[,2:29], MARGIN=1, FUN=divide.acs, denominator=industry[,1], 
                      method="proportion", verbose=F) # convert to percentage using divide.acs function
    q.data[,15:42] <- estimate(industry)
    colnames(q.data)[15:42] <- c("male","m.extract","m.const","m.mfr","m.whole","m.retail","m.freight","m.info","m.money","m.prof","m.eduetc",
                                 "m.artetc","m.oth","m.pub","female","f.extract","f.const","f.mfr","f.whole","f.retail","f.freight","f.info","f.money","f.prof",
                                 "f.eduetc","f.artetc","f.oth","f.pub")
    
    commute <- acs.fetch(endyear=2013, span=5, geography=cities, table.number="B08303", col.names = c("total","less5","thru9","thru14","thru19","thru24","thru29",
                                                                                                      "thru34","thru39","thru44","thru59","thru89","plus90"))
    commute <- apply(commute[,2:13], MARGIN=1, FUN=divide.acs, denominator=commute[,1], method="proportion", verbose=F)
    q.data[,43:54] <- estimate(commute)
    colnames(q.data)[43:54] <- c("less5","thru9","thru14","thru19","thru24","thru29","thru34","thru39","thru44","thru59","thru89","plus90")
      }
  
  # Export resultant table as a CSV file -- CHANGE DIRECTORY AS APPROPRIATE
  
  write.csv(q.data, file=paste("~/Dropbox (TransitCenter)/TransitCenter/Accuardi/hacking/",
                               if (bound == "metro") {"metro"} else {"city"}, "Data_",
                               Sys.Date(),".csv",sep=""))
}

# To run the function, create dataframe 'city_list' with columns "city" and "state" and MSA code (look these up manually)

# THERE'S SOMETHING WRONG WITH CALLING LOS ANGELES' METRO DATA FOR THE "MODE" VARIABLE
# NEED TO UPDATE LIST OF MSAs

# city <- c("Seattle","Sacramento","Denver","Chicago","Indianapolis","Raleigh","New York","Pittsburgh","Miami",
#           "Atlanta","Tampa","Houston","Phoenix","Los Angeles","Minneapolis","Cleveland","Memphis","Kansas City",
#           "Baltimore","Omaha","Oklahoma City")
# state <- c("WA","CA","CO","IL","IN","NC","NY","PA","FL","GA","FL","TX","AZ","CA","MN","OH","TN","MO","MD","NE","OK")
# msa <- c(42660,23420,40900,19740,46060,16980,19820,26900,39580,35620,38300,33100,12060,27260,45300,
#          16740,41180,26420,19100,41700,12420,38060,41620,29820,31080,33460,17460,18140,17140,40060,
#          47900,32820,28140,25540,39300,14460,15380,37980,12580,36540,38900,21660,35380,36420,36740)

# city <- c("Seattle","Fresno","Sacramento","Denver","Tucson","Chicago","Detroit","Indianapolis",
#           "Raleigh","New York","Pittsburgh","Miami","Atlanta","Jacksonville","Tampa",
#           "Charlotte","St. Louis","Houston","Dallas","San Antonio","Austin","Phoenix","Salt Lake City",
#           "Las Vegas","Los Angeles","Minneapolis","Cleveland","Columbus","Cincinnati","Richmond","Washington",
#           "Memphis","Kansas City","Hartford","Providence","Boston","Buffalo","Philadelphia","Baltimore",
#           "Omaha","Portland","Eugene","New Orleans","Oklahoma City","Orlando")
# state <- c("WA","CA","CA","CO","AZ","IL","MI","IN","NC","NY","PA","FL","GA","FL","FL","NC","MO","TX","TX",
#            "TX","TX","AZ","UT","NV","CA","MN","OH","OH","OH","VA","DC","TN","MO","CT","RI","MA","NY","PA",
#            "MD","NE","OR","OR","LA","OK","FL")
# msa <- c(42660,23420,40900,19740,46060,16980,19820,26900,39580,35620,38300,33100,12060,27260,45300,
#          16740,41180,26420,19100,41700,12420,38060,41620,29820,31080,33460,17460,18140,17140,40060,
#          47900,32820,28140,25540,39300,14460,15380,37980,12580,36540,38900,21660,35380,36420,36740)

city <- c("Houston","Dallas","San Antonio","Austin","Phoenix","Salt Lake City",
          "Las Vegas","Minneapolis","Cleveland","Columbus","Cincinnati","Richmond","Washington",
          "Memphis","Kansas City","Hartford","Providence","Boston","Buffalo","Philadelphia","Baltimore",
          "Omaha","Portland","Eugene","New Orleans","Oklahoma City","Orlando")
state <- c("TX","TX",
           "TX","TX","AZ","UT","NV","MN","OH","OH","OH","VA","DC","TN","MO","CT","RI","MA","NY","PA",
           "MD","NE","OR","OR","LA","OK","FL")
msa <- c(26420,19100,41700,12420,38060,41620,29820,33460,17460,18140,17140,40060,
         47900,32820,28140,25540,39300,14460,15380,37980,12580,36540,38900,21660,35380,36420,36740)

# msa <- c(42660,23420,40900,19740,46060,16980,19820,26900,39580,34980,35620,38300)
# city <- c("Seattle","Fresno","Sacramento","Denver","Tucson","Chicago","Detroit","Indianapolis","Raleigh","Nashville","New York","Pittsburgh")
# state <- c("WA","CA","CA","CO","AZ","IL","MI","IN","NC","TN","NY","PA")

city_list <- data.frame(city,state,msa)
city_list$city <- as.character(city_list$city)
city_list$state <- as.character(city_list$state)
city_list$msa <- as.numeric(city_list$msa)

# Create lists of important variables to be called within the function

library(acs)

race.vars <- race.vars <- c("pop","white","black","native","asian","pacific",
                            "other","two.plus","two.plus.other","two.plus.three")
transit.vars <- acs.lookup(table.number="B08301")[c(1,2,10)] # total + drive (alone or carpool) + public transit columns ONLY here
industry.vars <- acs.lookup(table.number="C24030")[c(1,
                                                  2,3,6,7,8,9,10,13,14,17,21,24,27,28,
                                                  29,30,33,34,35,36,37,40,41,44,48,51,54,55)]

# Call the function itself! 
cityQuery(city_list,"city")
cityQuery(city_list,"metro")

