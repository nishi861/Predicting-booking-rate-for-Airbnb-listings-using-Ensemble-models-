library(tidyverse)
library(modelr)
library(dplyr)
library(ROCR)
library(tree)
library(class)

getwd()
setwd("C:/Users/baner/OneDrive/Desktop/SEM 1/R/Project")

# cleaning test data
test_x <- read.csv("airbnb_test_x.csv", na.strings = c(""))

#Accomodates is clean
summary(test_x$accommodates)
test_x$accommodates[test_x$accommodates == "t"] <- '2'
test_x$accommodates= as.character(test_x$accommodates)
test_x$accommodates=as.numeric(test_x$accommodates)


#amenities
summary(test_x$amenities)
test_x$amenities_count <- str_count(test_x$amenities, ',')
summary(test_x$amenities_count)

#availability
test_x$availability_30=as.numeric(as.character(test_x$availability_30))
test_x$availability_60=as.numeric(as.character(test_x$availability_60))
test_x$availability_90=as.numeric(as.character(test_x$availability_90))
test_x$availability_30[is.na(test_x$availability_30)] <- 8
test_x$availability_60[is.na(test_x$availability_60)] <- 22
test_x$availability_90[is.na(test_x$availability_90)] <- 41
summary(test_x$availability_365)
#bathrooms
summary(test_x$bathrooms)
test_x$bathrooms[is.na(test_x$bathrooms)] = '1.0'
test_x$bathrooms[which(test_x$bathrooms == "Del Rey")] <- "1.0"
test_x$bathrooms[which(test_x$bathrooms == "Downtown")] <- "1.0"
test_x$bathrooms <- as.numeric(as.character(test_x$bathrooms))

#bed type
summary(test_x$bed_type)
test_x$bed_type[which(test_x$bed_type == "100%")] <- "Real Bed"
test_x$bed_type[which(test_x$bed_type == "81%")] <- "Real Bed"
test_x$bed_type<- as.factor(test_x$bed_type)
test_x$bed_type <- droplevels(test_x$bed_type)

#bedrooms
summary(test_x$bedrooms)
test_x$bedrooms[which(test_x$bedrooms == "within an hour")] <- "1.0"
test_x$bedrooms[is.na(test_x$bedrooms)] = '1.0'
test_x$bedrooms <- as.numeric(as.character(test_x$bedrooms))

#beds
summary(test_x$beds)
test_x$beds[which(test_x$beds == "2011-07-18")] <- "1.0"
test_x$beds[which(test_x$beds == "2013-01-20")] <- "1.0"
test_x$beds[is.na(test_x$beds)] = '1.0'
test_x$beds <- as.numeric(as.character(test_x$beds))

#cancellation policy
test_x$cancellation_policy[test_x$cancellation_policy == "2.0"] <- 'strict'
test_x$cancellation_policy[test_x$cancellation_policy == "5.0"] <- 'strict'
test_x$cancellation_policy <- as.factor(test_x$cancellation_policy)
test_x$cancellation_policy = droplevels(test_x$cancellation_policy )
summary(test_x$cancellation_policy)

#city_name
summary(test_x$city_name)
test_x$city_name <- as.character(test_x$city_name)
test_x$city_name[which(nchar(test_x$city_name) > 30)] <- 'New York'
test_x$city_name <- as.factor(test_x$city_name)
test_x$city_name[is.na(test_x$city_name)] <- 'New York'

#cleaning fee
test_x$cleaning_fee[is.na(test_x$cleaning_fee)] <- "$0.00"
test_x$cleaning_fee<-gsub(".{2}$", "", test_x$cleaning_fee)
test_x$cleaning_fee <-gsub("\\D", "", test_x$cleaning_fee)
test_x$cleaning_fee<-as.numeric(as.character(test_x$cleaning_fee))
summary(test_x$cleaning_fee)
test_x$cleaning_fee[is.na(test_x$cleaning_fee)] <- 0.00
#cleaning country
summary(test_x$country)
test_x$country[which(test_x$country != "United States")] <- "United States"
test_x$country <- as.factor(test_x$country)
test_x$country= droplevels(test_x$country)

#extra people - run again.

summary(test_x$extra_people)
test_x$extra_people[which(test_x$extra_people == '34.0321479706')] <- "$34.03"
test_x$extra_people[which(test_x$extra_people == '34.1099047213')] <- "$34.10"
test_x$extra_people[is.na(test_x$extra_people)] <- "$34.00"
test_x$extra_people<-gsub(".{2}$", "", test_x$extra_people)
test_x$extra_people <-gsub("\\D", "", test_x$extra_people)
test_x$extra_people<-as.numeric(test_x$extra_people)

#guests included
summary(test_x$guests_included)
test_x$guests_included[which(test_x$guests_included < 0)] <- 1.00

#host_has_profile_pic
summary(test_x$host_has_profile_pic)
test_x$host_has_profile_pic[is.na(test_x$host_has_profile_pic)] = 't'
test_x$host_has_profile_pic[which(test_x$host_has_profile_pic == "1")] = 't'
test_x$host_has_profile_pic <- as.factor(test_x$host_has_profile_pic)
test_x$host_has_profile_pic= droplevels(test_x$host_has_profile_pic)

#host_identity_verified
summary(test_x$host_identity_verified)
test_x$host_identity_verified[is.na(test_x$host_identity_verified)] <- 't'
test_x$host_identity_verified <- as.factor(test_x$host_identity_verified)
test_x$host_identity_verified= droplevels(test_x$host_identity_verified)

#host_is_superhost
test_x$host_is_superhost[is.na(test_x$host_is_superhost)] <- 'f'
test_x$host_is_superhost[which(test_x$host_is_superhost == 'Bed,Bath&Bike in Sunny Santa Monica')] <- 'f'
test_x$host_is_superhost[which(test_x$host_is_superhost == 'Pristine Mid-Century Modern w 180° Canyon View!')] <- 'f'
test_x$host_is_superhost <- as.factor(test_x$host_is_superhost)
summary(test_x$host_is_superhost)
test_x$host_is_superhost= droplevels(test_x$host_is_superhost)


#host_Response_rate
summary(test_x$host_response_rate)
test_x$host_response_rate = as.numeric(gsub("\\%", "", test_x$host_response_rate))
test_x$host_response_rate[is.na(test_x$host_response_rate)] <- 100

#host_listings_count (check) (it is correct)
summary(test_x$host_listings_count)
test_x$host_listings_count[is.na(test_x$host_listings_count)] = '1.0'
test_x$host_listings_count <-as.numeric(as.character(test_x$host_listings_count))


#host_response_time(Did not touch Nulls) (check logic)
summary(test_x$host_response_time)
test_x$host_response_time[is.na(test_x$host_response_time)] <- "within an hour"
test_x$host_response_time[which(test_x$host_response_time == 'f')] <- "within an hour"
test_x$host_response_time= droplevels(test_x$host_response_time)

#host_total listings
summary(test_x$host_total_listings_count)
test_x$host_total_listings_count[is.na(test_x$host_total_listings_count)] <- "1.0"
test_x$host_total_listings_count = as.numeric(as.character(test_x$host_total_listings_count))
test_x$host_total_listings_count[is.na(test_x$host_total_listings_count)] <- 1

#host_verifications
test_x$host_verifications_count <- str_count(test_x$host_verifications, ',')
summary(test_x$host_verifications_count)
test_x$host_verifications_count[is.na(test_x$host_verifications_count)] <- 3

#instant_bookable
summary(test_x$instant_bookable)
test_x$instant_bookable[which(test_x$instant_bookable == '$150.00')] <- 'f'
test_x$instant_bookable[is.na(test_x$instant_bookable)] <- 'f'
test_x$instant_bookable <- as.factor(test_x$instant_bookable)
test_x$instant_bookable=droplevels(test_x$instant_bookable)

#is_location_exact
summary(test_x$is_location_exact)
test_x$is_location_exact[is.na(test_x$is_location_exact)]<-'t'

#maximum_nights(NO NULLS, CONVERTED TO NUMERIC)
test_x$maximum_nights=as.numeric(as.character(test_x$maximum_nights))
summary(test_x$maximum_nights)
test_x$maximum_nights[is.na(test_x$maximum_nights)] <- 1125
test_x$maximum_nights=as.numeric(test_x$maximum_nights)

#minimum_nights(NO NULLS, CONVERTED TO NUMERIC)
test_x$minimum_nights=as.numeric(as.character(test_x$minimum_nights))
summary(test_x$minimum_nights)
test_x$minimum_nights[is.na(test_x$minimum_nights)] <- 2
test_x$minimum_nights=as.numeric(minimum_nights)
#price
summary(test_x$price)
test_x$price<-gsub(".{2}$", "", test_x$price)
test_x$price <-gsub("\\D", "", test_x$price)
test_x$price<-as.numeric(test_x$price)
test_x$price[is.na(test_x$price)] <- 109

#property type
test_x$property_type[is.na(test_x$property_type)] <- "Apartment"
test_x$property_type <- as.factor(test_x$property_type)
test_x$property_type= droplevels(test_x$property_type )
summary(test_x$property_type)

#guest phone
summary(test_x$require_guest_phone_verification)
test_x$require_guest_phone_verification[is.na(test_x$require_guest_phone_verification)] <- "f"

#guest profile picture
summary(test_x$require_guest_profile_picture)
test_x$require_guest_profile_picture[is.na(test_x$require_guest_profile_picture)] <- "f"

#require license
summary(test_x$requires_license)
test_x$requires_license[is.na(test_x$requires_license)] <- "f"

#room type
test_x$room_type[is.na(test_x$room_type)] <- "Entire home/apt"
test_x$room_type <- as.factor(test_x$room_type)
summary(test_x$room_type)

#security and cleaning mean
summary(test_x$security_deposit)
test_x$security_deposit[is.na(test_x$security_deposit)] <- "$0.00"
test_x$security_deposit<-gsub(".{2}$", "", test_x$security_deposit)
test_x$security_deposit <-gsub("\\D", "", test_x$security_deposit)
test_x$security_deposit<-as.numeric(test_x$security_deposit)

test_x$extra_charges <- rowSums(test_x[,c("cleaning_fee", "security_deposit")], na.rm=TRUE)
summary(test_x$extra_charges)

#state
test_x$state[test_x$state == "Ca"] <- 'CA'
test_x$state[test_x$state == "ca"] <- 'CA'
test_x[775,65] = 'CA'
test_x[1539,65] = 'NY'
test_x[4586,65] = 'CA'
test_x[10274,65] = 'CA'
test_x[12096,65] = 'CO'
test_x$state<- as.factor(test_x$state)
test_x$state= droplevels(test_x$state)
summary(test_x$state)

#cleaned smart_location 1 null with actual location
test_x$smart_location[is.na(test_x$smart_location)] = 'Los Angeles, CA'
summary(test_x$smart_location)


#summary(test_x$latitude)
#test_x$latitude[is.na(test_x$latitude)] = ''
#test_x$latitude[1539,44] = 40.6782
#test_x$longitude[1539,46] = 73.9442
test_x$latitude <- as.character(test_x$latitude)
summary(test_x$latitude)
test_x$latitude <- as.numeric(test_x$latitude)
test_x$latitude[is.na(test_x$latitude)] = 37.94

test_x$longitude <- as.numeric(as.character(test_x$longitude))
summary(test_x$longitude)
test_x$longitude[is.na(test_x$longitude)] = -95.14
write.csv(test_x,"C:/Users/baner/OneDrive/Desktop/SEM 1/R/Project/Test_data_final.csv")
summary(test_x$property_type)
summary(test_x$minimum_nights)
