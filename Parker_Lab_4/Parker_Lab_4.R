library(tidyverse)
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
dn <- read_csv("data/dennys.csv")
lq <- read_csv("data/laquinta.csv")

#How many Denny’s locations are there in Alaska?
dn_ak <- dn %>%
  filter(state %in% c("AK"))
#There are three locations in alaska.

#How many La Quinta locations are there in Alaska?
lq_ak <- lq %>%
  filter(state %in% c("AK"))
#there are two la Quinta's in Alaska.

ak_all <- full_join(dn_ak,lq_ak,by="state")

#How many observations in the full join and what are the variable names (you may use the
#glimpse function and paste your results as a comment in your script.

#Rows: 6
#Columns: 11
#$ address.x   <chr> "2900 Denali", "2900 Denali", "3850 Debarr Ro...
#$ city.x      <chr> "Anchorage", "Anchorage", "Anchorage", "Ancho...
#$ state       <chr> "AK", "AK", "AK", "AK", "AK", "AK"
#$ zip.x       <chr> "99503", "99503", "99508", "99508", "99701", ...
#$ longitude.x <dbl> -149.8767, -149.8767, -149.8090, -149.8090, -...
#$ latitude.x  <dbl> 61.1953, 61.1953, 61.2097, 61.2097, 64.8366, ...
#$ address.y   <chr> "3501 Minnesota Dr.", "4920 Dale Rd", "3501 M...
#$ city.y      <chr> "\nAnchorage", "\nFairbanks", "\nAnchorage", ...
#$ zip.y       <chr> "99503", "99709", "99503", "99709", "99503", ...
#$ longitude.y <dbl> -149.9119, -147.8660, -149.9119, -147.8660, -...
#$ latitude.y  <dbl> 61.18843, 64.82426, 61.18843, 64.82426, 61.18...



#What are the closest locations? Copy your output in your script and
#comment it out.
ak_all <- ak_all %>%
  mutate(distance = haversine(longitude.x,latitude.x,longitude.y,latitude.y))
ak_min <- ak_all %>%
  group_by(city.x,address.x) %>%
  summarise(.,min(distance))
#1 Anchorage 2900 Denali                 2.04
#2 Anchorage 3850 Debarr Road            6.00
#3 Fairbanks 1929 Airport Way            5.20

#What are the closest locations? Copy your output in your
#script and comment it out.
ds_states <- c("AL","SC","MS","GA","LA")
dn_ds <- dn %>%
  filter(state %in% ds_states)
lq_ds <- lq %>%
  filter(state %in% ds_states)

ds_all <- full_join(dn_ds,lq_ds,by="state")
ds_all <- ds_all %>%
  mutate(distance = haversine(longitude.x,latitude.x,longitude.y,latitude.y))
ds_min <- ds_all %>%
  group_by(state,city.x,address.x) %>%
  summarise(.,distance =min(distance)) %>%
  ungroup() %>%
  arrange(distance)

ds_min <- ds_min %>% 
  arrange(distance)
#1 LA    Metairie     5910 Veterans                 0.02 
#2 GA    Augusta      3026 Washington Rd            0.024
#3 GA    Savannah     6801 Abercorn St              0.07 
#4 AL    Huntsville   4874 University Drive         0.133
#5 GA    Columbus     3239 MacOn Rd                 0.224
#6 GA    Locust Grove 1020 Tanger Rd                0.224
#7 LA    Bossier City 1832 Old Minden Road          0.266
#8 AL    Cullman      5931 Alabama Highway, #157    0.325
#9 GA    Valdosta     1328 St Augustine Rd          0.434
#10 GA    Savannah     1 Gateway Blvd                0.588

# Copy your output in your script and comment it out.
ds_count <- ds_min %>%
  group_by(state) %>%
  count(state)
#1 AL 7
#2 GA 22
#3 LA 4
#4 MS 5
#5 SC 17
ds_avg <- ds_min %>%
  group_by(state) %>%
  summarise(avg_dist = mean(distance))

ds_avg %>%
  ggplot() +
  geom_col(mapping=aes(x=state, y=avg_dist, fill=state))

#On average, in which state are Denny’s and La Quinta the furthest apart? On
#average, in which state are they the closest?

#the distance between La Quinta and Denny's is lowest in LA and greatest in SC
