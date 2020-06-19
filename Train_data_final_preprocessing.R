library(tidyverse)
library(modelr)
library(dplyr)

#1a)Read the data set in R. 
airbnb=read.csv("C:/Users/baner/OneDrive/Desktop/SEM 1/R/Project/airbnb_train_x.csv",na.strings = c(""))
train_y <-read.csv("C:/Users/baner/OneDrive/Desktop/SEM 1/R/Project/airbnb_train_y.csv",,na.strings = c(""))
total_data <- merge(airbnb,train_y,by="X")



df = subset(total_data, select = -c(access,description,host_about,host_name,house_rules,interaction, name,neighborhood_overview,notes,space,street,summary,transit))
df= subset(df, select = -c(X, experiences_offered, host_acceptance_rate,is_business_travel_ready,jurisdiction_names,license,monthly_price,square_feet,weekly_price))
df1 = df[-c(16246, 30584, 47615, 56281, 75208, 65792, 72540, 92585, 96068),]
df2= df1[-c(13584 ,24316, 29878, 44688, 51368 ,63482 ,76211 ,85304 ,90602 ,92021),]



#accomodates
df2$accommodates = as.numeric(as.character(df2$accommodates))

#Amenities
df2$amenities_count <- str_count(df2$amenities, ',')
summary(df2$amenities_count)

#availability
df2$availability_30=as.numeric(as.character(df2$availability_30))
df2$availability_60=as.numeric(as.character(df2$availability_60))
df2$availability_90=as.numeric(as.character(df2$availability_90))

# Cleaning Bathroom and replacing with mode and converting to numeric
df2$bathrooms[is.na(df2$bathrooms)] = '1.0'
df2$bathrooms=as.numeric(as.character(df2$bathrooms))
summary(df2$bathrooms)

#bed_type
df2$bed_type <- as.factor(df2$bed_type)

#Cleaning Bedsrooms(92 Nulls with 1 bed)
df2$bedrooms=as.numeric(as.character(df2$bedrooms))
df2$bedrooms[is.na(df2$bedrooms)] = '1.0'
summary(df2$bedrooms)

# Cleaning Beds(83 Nulls with 1 bed)
df2$beds[is.na(df2$beds)] = '1.0'
summary(df2$beds)
class(df2$beds)
df2$beds=as.numeric(as.character(df2$beds))

#Cleaning cancellation policy
df2$cancellation_policy[df2$cancellation_policy == "no_refunds"] <- 'super_strict_30'
#df2$cancellation_policy[df2$cancellation_policy == "super_strict_60"] <- 'super_strict_30'
summary(df2$cancellation_policy)

#cleaning fee
df2$cleaning_fee[is.na(df2$cleaning_fee)] <- "$0.00"
df2$cleaning_fee<-gsub(".{2}$", "", df2$cleaning_fee)
df2$cleaning_fee <-gsub("\\D", "", df2$cleaning_fee)
df2$cleaning_fee<-as.numeric(df2$cleaning_fee)

summary(df2$cleaning_fee)

# Cleaning Country
which(df2$country == "Uruguay")
which(df2$country == "Mexico")
df2 <- df2[-c(548, 95955), ]
summary(df2$country)
df2$country<-as.factor(df2$country)


#cleaning extra people by putting nulls as 0 (IT IS ALREADY CLEAN, converted to numeric)
df2$extra_people<-gsub(".{2}$", "", df2$extra_people)
df2$extra_people <-gsub("\\D", "", df2$extra_people)
df2$extra_people<-as.numeric(df2$extra_people)


#host response rate  === check again. avoid. 
summary(df2$host_response_rate)
df2$host_response_rate[is.na(df2$host_response_rate)] <- "$0.00"
df2$host_response_rate = gsub("\\%", "", df2$host_response_rate)
df2$host_response_rate<- as.numeric(as.character(df$host_response_rate))



#host response time(Did not touch Nulls)
summary(df2$host_response_time)
df2$host_response_time[is.na(df2$host_response_time)] <- "within an hour"

#host_identity_verfied(Did not touch Nulls)
summary(df2$host_identity_verified)
df2$host_identity_verified[is.na(df2$host_identity_verified)] = 't'

#host profile pic(changed Nulls to True)
summary(df2$host_has_profile_pic)
df2$host_has_profile_pic[is.na(df2$host_has_profile_pic)] = 't'

#HOST SUPERHOST
summary(df2$host_is_superhost)
df2$host_is_superhost[is.na(df2$host_is_superhost)] = 'f'

#host listing count
df2$host_listings_count[is.na(df2$host_listings_count)] = '1.0'
df2$host_listings_count <-as.numeric(as.character(df2$host_listings_count))

#host total listing
df2$host_total_listings_count[is.na(df2$host_total_listings_count)] <- "1.0"
df2$host_total_listings_count = as.numeric(as.character(df2$host_total_listings_count))

#count of host verificatons 
df2$host_verifications_count <- str_count(df2$host_verifications, ',')
summary(df2$host_verifications_count)

#instant bookable and is location exact is clean
summary(df2$instant_bookable)
summary(df2$is_location_exact)

#Latitude and Longitude have been converted to numeric
df2$latitude=as.numeric(as.character(df2$latitude))
df2$longitude=as.numeric(as.character(df2$longitude))
summary(df2$longitude)
summary(df2$latitude)


#maximum_nights(NO NULLS, CONVERTED TO NUMERIC)
df2$maximum_nights=as.numeric(as.character(df2$maximum_nights))
summary(df2$maximum_nights)

#minimum_nights(NO NULLS, CONVERTED TO NUMERIC)
df2$minimum_nights=as.numeric(as.character(df2$minimum_nights))
summary(df2$minimum_nights)

# price (converted to numeric, no nulls)
df2$price<-gsub(".{2}$", "", df2$price)
df2$price <-gsub("\\D", "", df2$price)
df2$price<-as.numeric(df2$price)
summary(df2$price)

#property_type(cleaned 1NA with mode)
df2$property_type[is.na(df2$property_type)] = 'Apartment'
summary(df2$property_type)

#guest phone verification, guest profile picture, license, room type, all are clean
summary(df2$require_guest_phone_verification)
summary(df2$require_guest_profile_picture)
summary(df2$requires_license)
summary(df2$room_type)


#security and cleaning mean/2
df2$security_deposit[is.na(df2$security_deposit)] <- "$0.00"
df2$security_deposit<-gsub(".{2}$", "", df2$security_deposit)
df2$security_deposit <-gsub("\\D", "", df2$security_deposit)
df2$security_deposit<-as.numeric(df2$security_deposit)
summary(df2$security_deposit)
df2$extra_charges <- rowSums(df2[,c("cleaning_fee", "security_deposit")], na.rm=TRUE)
summary(df2$extra_charges)

#Cleaning State
df2$state[df2$state == "Baja California"] <- 'CA'
df2$state[df2$state == "Ca"] <- 'CA'
df2$state[df2$state == "ca"] <- 'CA'
df2$state[df2$state == "il"] <- 'IL'
df2$state[df2$state == "New York"] <- 'NY'
df2$state[df2$state == "ny"] <- 'NY'
df2$state[df2$state == "Ny"] <- 'NY'


row.names(df2) <- NULL

df2[24636,47] = 'CA'
df2[41925,47] = 'NY'
df2[93896,47] = 'TX'
df2[97024,47] = 'NY'
which(df2$state == "secc Terrazas")
df2 <- df2[-c(75586), ]
summary(df2$state)


#cleaned smart_location 1 null with actual location
df2$smart_location[is.na(df2$smart_location)] = 'Austin, TX'
summary(df2$smart_location)




write.csv(df2,'C:/Users/baner/OneDrive/Desktop/SEM 1/R/Project/train_data_final.csv', row.names = FALSE)