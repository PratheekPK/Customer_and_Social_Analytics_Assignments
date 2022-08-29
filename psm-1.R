
#install.packages("MatchIt")
library(MatchIt)

library(dplyr)



hnd <- read.csv("HighNote Data.csv")


hnd$male <- as.factor(hnd$male)
hnd$good_country <- as.factor(hnd$good_country)
hnd$adopter <- as.factor(hnd$adopter)

hnd$have_subscriber_friend <- ifelse(hnd$subscriber_friend_cnt>=1,1,0)

match2 <- matchit(adopter ~ posts + shouts + friend_cnt + songsListened + lovedTracks + playlists + male + tenure + good_country +  subscriber_friend_cnt + age , data = hnd, method = 'nearest')

match <- matchit(adopter ~ playlists + posts + shouts +friend_cnt +songsListened + lovedTracks + age + male + tenure +  have_subscriber_friend +  good_country,data = hnd, method = 'nearest')
summary(match)
plot(match)

plot(match,type = "hist")

dta_m <- match.data(match)
dta_m2 <- match.data(match2)


dim(dta_m)



mymodel2 <- glm(adopter ~ shouts +friend_cnt + have_subscriber_friend + songsListened + male + tenure + good_country+ lovedTracks + playlists + posts + age ,data = dta_m,family = 'binomial')
summary(mymodel2)

mymodel3 <- glm(adopter ~  posts + shouts +friend_cnt + subscriber_friend_cnt  +songsListened + tenure + good_country+ lovedTracks + playlists + age + male,data = dta_m2,family = 'binomial')
summary(mymodel3)




