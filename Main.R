#yes
install.packages("dplyr")
## for os specific requirments and directions see https://github.com/r-spatial/sf
#install.packages("tmap") #helps create simple choropleths
#install.packages("plotly") #helps create scatterplots
#install.packages("fuzzyjoin") ##makevars =SHLIB_OPENMP_CFLAGS= SHLIB_OPENMP_CXXFLAGS=
library(sf)
library(tmap)
library(plotly)
library(dplyr)

library("fuzzyjoin")


#County_Aggregate_Data <- st_read("./data/County1990ussm/")
#summary(County_Aggregate_Data)

###read a merge census
#Census_Data <- read.csv("./data/County1990_Data/nhgis0027_ts_nominal_county.csv", as.is=TRUE)

## 2010 American Community Survey
acs_Data <- read.csv("./data/nhgis0048_csv/nhgis0048_ds177_20105_2010_county.csv", as.is=TRUE)
AsiaPop10k <- ((acs_Data$JWOE047/acs_Data$JWAE001)*10000)
Test_Data<-data.frame(AsiaPop10k)
##fields to include
acs_Data$COUNTY
Test_Data$Foreign10K <- ((acs_Data$JWUE003/acs_Data$JWUE001)*10000)
Test_Data$IncomeIneq <- acs_Data$J4TE001
Test_Data$EuropePop10k <- ((acs_Data$JWOE002/acs_Data$JWAE001)*10000)
Test_Data$GISJOIN<-acs_Data$GISJOIN


## 2010 Census
##nhgis0048_ds172_2010_county_codebook
##fields to include
cen_Data <- read.csv("./data/nhgis0048_csv/nhgis0048_ds172_2010_county.csv", as.is=TRUE)
Urban10k <- ((cen_Data$H7W002/cen_Data$H7V001)*10000)
Test_Data2<-data.frame(Urban10k)
Test_Data2$GISJOIN<-cen_Data$GISJOIN
Test_Data2$white10k <- ((cen_Data$H7X002/cen_Data$H7V001)*10000)
Test_Data2$TotalPop <- cen_Data$H7V001
Test_Data2$County<-cen_Data$COUNTY
Test_Data2$State<-cen_Data$STATE
Test_Data2$fips<-paste(cen_Data$STATEA,sprintf("%03d",cen_Data$COUNTYA), sep="")
County_Aggregate_Data <- merge(Test_Data,Test_Data2,by.x="GISJOIN", by.y="GISJOIN")



## 2010 Census
health_Data <- read.csv("./data/nhgis0049_csv/nhgis0049_ds175_2010_county.csv", as.is=TRUE)
insured18to35_per10k <- ((health_Data$JIFE018/health_Data$JIFE001)*10000)
Test_Data3<-data.frame(insured18to35_per10k)
Test_Data3$insured35to64_per10k <- ((health_Data$JIFE050/health_Data$JIFE001)*10000)
Test_Data3$GISJOIN<-health_Data$GISJOIN
County_Aggregate_Data <- merge(County_Aggregate_Data,Test_Data3,by.x="GISJOIN", by.y="GISJOIN",all.x = TRUE)


## more census income and ed nhgis0052_csv
more_census <- read.csv("./data/nhgis0052_csv/nhgis0052_ds176_20105_2010_county.csv", as.is=TRUE)
##IXMM022 bachelor degree  totla =IXMM001
bachelor_degreeM_per10k <- ((more_census$JN9E015/more_census$JN9E002)*10000)
perCapitaIncome <- more_census$JOIE001
Test_Data4<-data.frame(perCapitaIncome)
Test_Data4$bachelor_degreeM_per10k<-bachelor_degreeM_per10k;
perCapitaIncome <- more_census$JOIE001
Test_Data4$GISJOIN<-more_census$GISJOIN
County_Aggregate_Data <- merge(County_Aggregate_Data,Test_Data4,by.x="GISJOIN", by.y="GISJOIN",all.x = TRUE)


## rural urban
even_more_census <- read.csv("./data/nhgis0053_csv/nhgis0053_ds172_2010_county.csv", as.is=TRUE)
##IXMM022 bachelor degree  totla =IXMM001
##Test_Data5$GISJOIN<-even_more_census$GISJOIN
GISJOIN <- even_more_census$GISJOIN
Test_Data5<-data.frame(GISJOIN)
Test_Data5$UrbanPer10k <- ((even_more_census$H7W002/even_more_census$H7W001)*10000)
County_Aggregate_Data <- merge(County_Aggregate_Data,Test_Data5,by.x="GISJOIN", by.y="GISJOIN",all.x = TRUE)


## 2010 Census AGE
age_Data <- read.csv("./data/nhgis0050_csv/nhgis0050_ds176_20105_2010_county.csv", as.is=TRUE)
med_age<-age_Data$JL0E001
Test_Data4<-data.frame(med_age)
Test_Data4$GISJOIN<-age_Data$GISJOIN
County_Aggregate_Data <- merge(County_Aggregate_Data,Test_Data4,by.x="GISJOIN", by.y="GISJOIN",all.x = TRUE)

##Prepare the Export
fips_ma <- read.csv("./data/county_fips_master.csv", as.is=TRUE)
new_fips<- merge(fips_ma,County_Aggregate_Data,by.x="fips", by.y="fips",all.x = TRUE)
fips_exp<- c("fips", "County","State","state","county","GISJOIN","TotalPop","AsiaPop10k","EuropePop10k","white10k","insured18to35_per10k","insured35to64_per10k","IncomeIneq","med_age","bachelor_degreeM_per10k","UrbanPer10k","perCapitaIncome")
fips_out <- new_fips[fips_exp]
##decimal places
is.num <- sapply(fips_out, is.numeric)
fips_out[is.num] <- lapply(fips_out[is.num], round, 3)

write.csv(fips_out,'county_fips_revised.csv')

new_fips$med_age



###try new data sprintf("%03d", 104)
COVID_Data <- read.csv("./data/cv.csv", as.is=TRUE)
County_Aggregate_Data <- County_Aggregate_Data %>%
  select(fips, everything())
County_Merge <-NULL
County_Merge <- merge(County_Aggregate_Data,COVID_Data,by.x="fips", by.y="fips",all.x = TRUE)
###let look for corelations

County_Merge_fil<-County_Merge
County_Merge_fil$ConfirmedPer10K <- as.integer(((County_Merge_fil$Confirmed/County_Merge_fil$TotalPop)*100000))
County_Merge_fil$county_name<-NULL
County_Merge_fil$state_abbr<-NULL
County_Merge_fil$division_name<-NULL
County_Merge_fil$long_name<-NULL
County_Merge_fil$region<-NULL
County_Merge_fil$state_name<-NULL
County_Merge_fil$State_Name<-NULL
County_Merge_fil$County_Name<-NULL
County_Merge_fil$Last_Update<-NULL
County_Merge_fil$crosswalk<-NULL
County_Merge_fil$New<-NULL
County_Merge_fil$region_name<-NULL
County_Merge_fil$Latitude<-NULL
County_Merge_fil$Longitude<-NULL
County_Merge_fil$id<-NULL
County_Merge_fil$fips<-NULL
County_Merge_fil$GISJOIN<-NULL
County_Merge_fil$sumlev<-NULL
County_Merge_fil$division<-NULL
County_Merge_fil$state<-NULL
County_Merge_fil$county<-NULL
County_Merge_fil$TotalPop<-NULL
County_Merge_fil$Confirmed<-NULL
County_Merge_fil$Confirmed<-NULL


County_Merge$insured35to64_per10k
myvars <- c("fips", "Foreign10K","IncomeIneq","EuropePop10k","white10k","EuropePop10k","insured18to35_per10k","insured35to64_per10k","perCapitaIncome")
newdata <- County_Merge_fil[myvars]

export<-County_Merge_fil
export$TotalPop<-County_Merge$TotalPop
export$fips<-County_Merge$fips
export$county_name<-County_Merge$county_name
write.csv(export,'export.csv')
#install.packages("corrplot")
#install.packages("glm")

##take a look
library(corrplot)

M <- cor(County_Merge_fil,use="pairwise.complete.obs") # get correlations
library('corrplot') #package corrplot
library('glm') #package corrplot
corrplot(M, method = "circle") #plot matrix

#look for corelations to one var
library(corrr)
County_Merge_fil %>% correlate() %>% focus(Death)

plot(County_Merge_fil$ConfirmedPer10K,County_Merge_fil$Urban10k)

model1 <-glm(formula = County_Merge_fil$ConfirmedPer10K ~ County_Merge_fil$Urban10k, family = poisson)
summary(model1)
####
plot(County_Merge_fil$IncomeIneq,County_Merge_fil$ConfirmedPer10K)
myvars <- c("ConfirmedPer10K", "IncomeIneq")
newdata <- County_Merge_fil[myvars]

model <- lm(formula = ConfirmedPer10K ~ IncomeIneq, data = newdata)
model2 <-glm(formula = ConfirmedPer10K ~ IncomeIneq, data = newdata, family = poisson)
new.ineq <- data.frame(
  IncomeIneq = c(.3, .5, 1)
)
predict(model, newdata = new.ineq)
predict(model2, newdata = new.ineq)

geocoded_addresses <- read.csv("./data/03-09-2020us.csv", as.is=TRUE)
geocoded_addresses <- geocoded_addresses[!is.na(geocoded_addresses$Latitude) & !is.na(geocoded_addresses$Longitude),]
points <- st_as_sf(geocoded_addresses, coords= c("Longitude","Latitude"),crs = 4326, agr = "constant")
County_Aggregate_Data <- st_transform(County_Aggregate_Data, st_crs(points))
st_intersects(County_Aggregate_Data,points) # show which counties each point falls into
#plot(County_Aggregate_Data$geometry,axes=TRUE)
#plot(points[which(points$State %in% c("WI","IL","IN","MN")),]$geometry,col = "green", pch=20,cex=.5, axes=TRUE,add=TRUE)
County_Aggregate_Data$CountMembers <- sapply(st_intersects(County_Aggregate_Data,points), function(z) if (length(z)==0) NA_integer_ else length(z))

County_Aggregate_Data$RelativeTotal= ((County_Aggregate_Data$AV0AA1990/10000)/County_Aggregate_Data$CountMembers )

qtm(shp = County_Aggregate_Data, fill = "RelativeTotal")


newdata <-cen_data[ which(cen_data$STATE=='ALASKA'), ]

al = cen_Data$GISJOIN[cen_Data$STATE=="Hawaii"]
xs <- toString(shQuote(al, type = "cmd"))
cat(xs, "\n")



fips_ma <- read.csv("./data/county_fips_master.csv", as.is=TRUE)
new_fips<- merge(fips_ma,export,by.x="fips", by.y="fips",all.x = TRUE)
new_fips$Death<-NULL
new_fips$ConfirmedPer10K<-NULL
new_fips$sumlev<-NULL
new_fips$county_name.y<-NULL
new_fips$county_name.x<-NULL
new_fips$Fatality_Rate<-NULL

write.csv(new_fips,'county_fips_revised.csv')




### google data
ga_data <- read.csv("./data/mobility-data-ts_all.csv", as.is=TRUE)
library(purrr)
library(stringr)
library(reshape2)
library(dplyr)
library(stringr)

##tmp <- merge(DF1,DF2, by=c("A","B"), all.x=TRUE, all.y=FALSE)
#tmp<- ga_data
#tmp$county  <- gsub("\\county\\b", "", df$value,ignore.case = TRUE)
#tmp$county <- gsub('\\s+', '', tmp$county)
#df['Column'] = df['Column'].str.replace('^\d+','')
#tmp2$county_name<-tmp2$county
#tmp2s$tate_name<-tmp2$state
#tmp2 <- full_join(data1, data2, by = c("state", "county"), all.x=TRUE, all.y=FALSE)
#tmp2 <- merge(tmp,fips_ma, by=c("A","B"), all.x=TRUE, all.y=FALSE)


CreateFips = function() {
  for (row in 1:nrow(ga_data)) {
    state <- ga_data[row, "state"]
    county <- ga_data[row, "county"]
    #print(paste("Looking:", state, " ::", county))
    #print(paste("Looking:", state, " ::", county))  grepl(fips_ma$county_name, county)
    ## %like% "%county%"
    #grep("mb", Name)
    newdata <- fips_ma[ which(fips_ma$state_name==state & str_detect(fips_ma$county_name, county)), ]
   # newdata <- subset(fips_ma, state_name == state & county_name ==county, select=c(fips))
    if (NROW(newdata) > 0) {
      if (row %% 10 == 0){
        print(paste("Row:", row));
      }
     # print(paste("Puttings:", ga_data[row,], " ::", newdata$fips))
      ga_data[row,'fips']=newdata[1,1]
    } else {
    }
  }
}
##
#ga_data['Doña Ana',]=NULL
#ga_data_new<- head(ga_data,-1)
ga_data_new <- ga_data

#ga_data[1,'fips']
#data_wide <- dcast(ga_data, state + county ~ seg, value.var="value")

Create_files  = function(DF) {
  DF2<-DF
  DF$date<-NULL
  data_wide <- dcast(DF, state + county + fips ~ seg, value.var="value",fun.aggregate=mean)
  write.csv(data_wide,paste0("",unique(DF2$date),".csv"),row.names=FALSE)
  return(DF)
}
ga_data_new <- ga_data
###here is what we need to do
CreateFips() 

ga_data_new %>% 
  group_by(date) %>% 
  do(Create_files(.))

###

### google data
ga_data <- read.csv("./data/Global_Mobility_Report.csv", as.is=TRUE)
library(purrr)
library(stringr)
library(reshape2)
library(dplyr)
library(stringr)
str(fips_ma)

CreateFips = function() {
  for (row in 1:nrow(ga_data2)) {
    state <- ga_data2[row, "state"]
    county <- ga_data2[row, "county"]
    newdata <- fips_ma[ which(fips_ma$state_name==state & str_detect(fips_ma$county_name, county)), ]
    # newdata <- subset(fips_ma, state_name == state & county_name ==county, select=c(fips))
    if (NROW(newdata) > 0) {
      if (row %% 100 == 0){
        print(paste("Row:", row));
      }
       #print(paste("Puttings:", ga_data[row,], " ::", newdata$fips))
      ga_data2[row,'fips']=newdata[1,1]
    } else {
    }
  }
}

##
library(purrr)
library(stringr)
library(reshape2)
library(dplyr)
library(stringr)
######################
## Start excuation here
######################
ga_data <- read.csv("./data/Global_Mobility_Report.csv", as.is=TRUE)
ga_data2<-ga_data[ga_data$country_region_code == 'US', ]
ga_data2<- rename(ga_data2, c("sub_region_1"="state", "sub_region_2"="county"))
ga_data2$fips<-NA
stopwords = c("County","county","Parish","parish")
ga_data2$county <- gsub(paste0(stopwords,collapse = "|"),"", ga_data2$county)
ga_data2$county <- trimws(ga_data2$county)
CreateFips() #function now work run manually
##full data now has
str(ga_data2)
#full_data<-ga_data2[complete.cases(ga_data2), ]
full_data<-ga_data2[!(is.na(ga_data2$county) | ga_data2$county==""), ]
# Split dataframe by city
split_df <- split(full_data, list(full_data$date))

# Write out separate CSV for each city

for (date in names(split_df)) {
  write.csv(split_df[[date]],row.names = FALSE ,paste0("./Data/ga/",date, ".csv"))
}
health_data <- read.csv("./data/2020Health.csv", as.is=TRUE)
health_data_compact<-health_data[ , c("FIPS", "County","p_Fair.or.Poor.Health","Primary.Care.Physicians.Rate","p_Smokers",
                                      "Preventable.Hospitalization.Rate","p_Adults.with.Obesity",
                                      "p_Vaccinated","High.School.Graduation.Rate","Social.Association.Rate","p_Uninsured")] 
health_data_compact$fips<-health_data_compact$FIPS
health_data_compact$FIPS<-NULL
fips_out2<-merge(x=fips_out,y=health_data_compact,by="fips",all.x=TRUE)
fips_out2$County<-fips_out2$County.x
fips_out2$County.x<-NULL
write.csv(fips_out2,'county_fips_revised.csv')

