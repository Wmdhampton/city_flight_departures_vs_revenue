
#Load city revenue data 
#(https://www.lincolninst.edu/research-data/data-toolkits/fiscally-standardized-cities/search-database).
file_names <- list.files(pattern = ".csv")
fisc <- read.csv(file_names[3], header = TRUE, colClasses = c("numeric", 
            "character",rep("NULL", 3),"numeric",rep("NULL", 656)))
#Get 'city_name' categories
fisc1 <- data.frame("city_name" = fisc$city_name)
fisc_cities <- levels(fisc1$city_name)

#Load key for airport codes matched to city
#(https://www.codediesel.com/data/international-airport-codes-download/)
airport_codes <- read.csv(file_names[1], header = FALSE,
                          colClasses = c("character", "character") )

#Clean city names
fisc_city_cl <- as.character(c(rep(NA, length(fisc_cities))))
for (i in 1:length(fisc_cities)){
fisc_city_cl[i] <- substr(fisc_cities[i],5,nchar(fisc_cities[i]))
}

#Check which cities have airports (Some have multiple).
#Make key for these cities with their airport code(s)
air_code_dim <- dim(airport_codes)
fisc_codes <- as.character(c(rep(NA, air_code_dim[1])))
fisc_code_c <- as.character(c(rep(NA, air_code_dim[1])))
k = 1
for (i in 1:length(fisc_city_cl)){
  for (j in 1:air_code_dim[1]){
    if (fisc_city_cl[i] == substr(airport_codes$V1[j],1,nchar(fisc_city_cl[i]))){
      fisc_codes[k] <- airport_codes$V2[j]
      fisc_code_c[k] <- fisc_city_cl[i]
      k <- k + 1
    }
  }
}
fisc_codes <- fisc_codes[!is.na(fisc_codes)]
fisc_code_c <- fisc_code_c[!is.na(fisc_code_c)]
fisc_key <- data.frame("city" = fisc_code_c, "code" = fisc_codes)

#Clean city names in revenue data
city_names <- levels(fisc_key$city)
for (i in 1:length(fisc_city_cl)){
  fisc$city_name[which(fisc$city_name == fisc_cities[i])] <- fisc_city_cl[i]
}
fisc1 <- data.frame("year" = fisc$year, "city_name" = fisc$city_name, "rev_total_city" = fisc$rev_total_city)
levels(fisc1$city_name)

#get revenue by year (2010-2016 is chosen because this is when revenue data overlaps with flight data) for
#cities with airports that have revenue data
years <- as.numeric(2010:2016)
revenue <- matrix(as.numeric(0), length(levels(fisc_key$city)), length(years))
rownames(revenue) <- city_names
colnames(revenue) <- years
city_year_ind <- 1977:2016
for (i in 1:length(city_names)){
  city_year_ind <- which(fisc1$city_name == city_names[i])
  revenue[i,1:length(years)] <- fisc1$rev_total_city[city_year_ind[(length(city_year_ind)-
                                                                      length(years)+1):length(city_year_ind)]]
}

#Look at fraction of change of revenue from one year to the next for all cities over years collected.
#Revenue(Year)/Revenue(Year - 1)
years <- as.numeric(2011:2016)
revenue_ch <- matrix(as.numeric(0), length(levels(fisc_key$city)), length(years))
rownames(revenue_ch) <- city_names
colnames(revenue_ch) <- years
for (i in 1:length(years)){
  revenue_ch[1:length(city_names),i] <- revenue[1:length(city_names),(i+1)]/revenue[1:length(city_names),(i)]
}

#Get flight data by quarter. All datasets downloaded from 
# https://www.bts.dot.gov/topics/airlines-and-airports/origin-and-destination-survey-data to working directory.
#Includes all quarters from 2010 to 2018. 
total_MB <- 400*36 #Approximate size in MB of all 36 files
file_names <- list.files(pattern = ".asc")
dep_quarts <- as.character(c(rep(NA, length(file_names))))
for (i in 1:length(file_names)){
  dep_quarts[i] <- substr(file_names[i],13,18)
}
departs <- matrix(as.integer(0), length(levels(fisc_key$city)), length(years+1)*4)
rownames(departs) <- city_names
colnames(departs) <- dep_quarts
city_row <- as.integer(NA)
departures <- as.integer(NA) 
for (i in 1:length(file_names)){
  data <- read.table(file_names[i], header = FALSE, sep = "|", fill = TRUE,
                     colClasses = c(rep("NULL", 5), "character", rep("NULL", 14)))
  for (j in 1:length(fisc_codes)){ #Uses all airports found in revenue data
    city_row <- which(city_names == fisc_code_c[j])
    #counts number of departures from an airport in the flight data. Adds this to the
    #departures from a city, since some cities have multiple airports
    departs[city_row,i] <- departs[city_row,i] + length(which(data$V6 == fisc_codes[j]))
  }
}

#Combines quarterly departure data in yearly departure data
departs_y <- matrix(as.numeric(0), length(levels(fisc_key$city)), (length(years) + 1))
rownames(departs_y) <- city_names
colnames(departs_y) <- 2010:2016
for (i in 1:(length(years)+1)){
  departs_y[1:length(city_names),i] <- rowSums(departs[1:length(city_names),(4*i-3):(4*i)])
}

#Look at fraction of change of departures from one year to the next for all cities over years collected.
#Departures(Year)/Departures(Year - 1)
departs_ch <- matrix(as.numeric(0), length(levels(fisc_key$city)), length(years))
rownames(departs_ch) <- city_names
colnames(departs_ch) <- years
for (i in 1:length(years)){
  departs_ch[1:length(city_names),i] <- departs_y[1:length(city_names),(i+1)]/departs_y[1:length(city_names),(i)]
}

#Some of the cities start with zero departures, but then start having departures. This may be due to an 
#airport being built during the time period tested. For simplicity of initial analysis these cities are
#ommited
departures_keep <- which(departs_y[1:length(city_names),1] != 0)
departs_y <- departs_y[departures_keep,1:(length(years) + 1)]
departs_ch <- departs_ch[departures_keep,1:(length(years))]
revenue <- revenue[departures_keep,1:(length(years) + 1)]
revenue_ch <- revenue_ch[departures_keep,1:(length(years))]

#Builds a linear regression model for all 6 years comparing change in revenue to change in departures for each city.
#The slopes of these lines were looked at, but no pattern emerged. A histogram of the coefficient of determination
#for each city shows no correlation between year-to-year change of revenue to departures for the majority of cities
rev_dims <- dim(revenue_ch)
params <- matrix(as.numeric(0), rev_dims[1], 2)
for (i in 1:rev_dims[1]){
  dep_rev <- data.frame("dep" = departs_ch[i,1:length(years)], "rev" = revenue_ch[i,1:length(years)])
  dep_rev.lm <- lm(rev ~ dep, data = dep_rev)
  coeffs <- coefficients(dep_rev.lm)
  correl <- cor(dep_rev$rev, dep_rev$dep)
  params[i,1:2] <- c(coeffs[2], correl)
}

#A histogram of the coefficient of determination for each city shows no correlation between year-to-year change of 
#revenue to departures for the majority of cities
hist(params[,2]*params[,2],xlab="R^2 for each city found across 6 years: [rev_(Y)/rev_(Y-1) vs dep_(Y)/dep_(Y-1)](Y=2011:2016)",
     ylab="Cities", main="R^2's for Yearly Change of Revenue to Departures by city")

#Looks at correlation between revenue and departure within a given year.
#Revenue does not seem to correlate with departures across cities (not plotted)
params_t <- matrix(as.numeric(0), (length(years)+1), 2)
for (i in 1:(length(years)+1)){
  dep_rev_t <- data.frame("dep" = departs_y[1:rev_dims[1],i], "rev" = revenue[1:rev_dims[1],i])
  dep_rev_t.lm <- lm(rev ~ dep, data = dep_rev_t)
  coeffs <- coefficients(dep_rev_t.lm)
  correl <- cor(dep_rev_t$rev, dep_rev_t$dep)
  params_t[i,1:2] <- c(coeffs[2], correl)
}


#Look at total change in revenue and departures from 2010 to 2016
departs_tch <- departs_y[1:rev_dims[1],7]/departs_y[1:rev_dims[1],1]
revenue_tch <- revenue[1:rev_dims[1],7]/revenue[1:rev_dims[1],1]

#Check if total change in revenue and departures is correlated across cities
dep_rev_tch <- data.frame("dep" = departs_tch, "rev" = revenue_tch)
dep_rev_tch.lm <- lm(rev ~ dep, data = dep_rev_tch)
coeffs <- coefficients(dep_rev_tch.lm)
correl <- cor(dep_rev_tch$rev, dep_rev_tch$dep)
params_tch <- c(coeffs[2], correl)

#Plot for each city: 
#y = revenue from 2010 to 2016
#x = departures from 2010 to 2016
#include linear regression with R^2 to show no correlation
plot(departs_tch,revenue_tch,xlab="Num Departures 2016/2010",
     ylab="Revenue 2016/2010", main="Flight Departures vs City Revenue by city")
abline(coeffs)
legend("topright", bty="n", legend=paste("R^2 is", format(summary(dep_rev_tch.lm)$r.squared, digits=4)))