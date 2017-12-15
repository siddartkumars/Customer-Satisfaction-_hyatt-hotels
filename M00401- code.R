##### Data Cleaning##########
library(readr)
feb <- read_csv("E:/project/IST687-data/IST687-data/out-201402.csv")
mar <- read_csv("E:/project/IST687-data/IST687-data/out-201403.csv")
apr <- read_csv("E:/project/IST687-data/IST687-data/out-201404.csv")
may <- read_csv("E:/project/IST687-data/IST687-data/out-201405.csv")
jun <- read_csv("E:/project/IST687-data/IST687-data/out-201406.csv")
jul <- read_csv("E:/project/IST687-data/IST687-data/out-201407.csv")

feb <- feb[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
feb<- feb[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
feb <- feb[!(is.na(feb$Likelihood_Recommend_H)),]

mar <- mar[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
mar <- mar[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
mar <- mar[!(is.na(mar$Likelihood_Recommend_H)),]

apr <- apr[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
apr <- apr[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
apr <- apr[!(is.na(apr$Likelihood_Recommend_H)),]

may <- may[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
may <- may[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
may <- may[!(is.na(may$Likelihood_Recommend_H)),]

jun <- jun[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
jun <- jun[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
jun <- jun[!(is.na(jun$Likelihood_Recommend_H)),]

jul <- jul[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
jul <- jul[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
jul <- jul[!(is.na(jul$Likelihood_Recommend_H)),]

halfyearly <- rbind(feb,mar,apr,may,jun,jul)

halfyearly_amenities <- halfyearly[,c(89:113,117)]
halfyearly_amenities <- halfyearly_amenities[halfyearly_amenities$NPS_Type=="Promoter",]
halfyearlypromoters <-nrow(halfyearly_amenities)




##################################################################

library(ggplot2)
library('rworldmap')
library('rworldxtra')
library('RColorBrewer')
library('maptools')
library('classInt')
library(ggmap)

################# Visualizing location in maps#######################################

map <- get_map(location = "United States of America", zoom = 4)
mappoi <-ggmap(map)
mapPoi <- ggmap(map) + geom_point(data = america ,aes( x = america$`Property Longitude_PL` , y = america$`Property Latitude_PL`))
mapPoi

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-139.3, -58.8) , ylim = c(13.5, 55.7), asp = 1)

points(america$`Property Longitude_PL`, america$`Property Latitude_PL`, col = "red", cex = .6)


############################ North america data##################

america <- clean[clean$Country_PL=="United States",]
america_sample <- america[1:100000,]
america_sample <- america[!(is.na(america$Likelihood_Recommend_H)),]
#View(america_sample)

l <- as.factor(america_sample$State_PL)
datal <- as.table(summary(l))
plot(datal)
which.max(datal)
#l <- as.data.frame(summary(l))
#View(l)View(x)
#cal <- america_sample[america_sample$State_PL=="California"]

cal_business <- america_sample[america_sample$POV_CODE_C=="BUSINESS"]
cal_leisure <- america_sample[america_sample$POV_CODE_C=="LEISURE"]
count <- c(nrow(cal_business),nrow(cal_leisure))
names(count) <- c("Business","LEISURE")

highchart()
library(highcharter)
highchart() %>% hc_title(text = "<b>Purpose of Visit</b>",
                         margin = 20, align = "center",
                         style = list(color = "red", useHTML = TRUE)) %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3,title = list(text = "Customer COUNT"),min=0),
    list(showLastLabel = FALSE, opposite = TRUE,title = list(text = "trend"))
  ) %>% 
  hc_xAxis(title=list(text ="Business   Leisure" ),categories = count) %>% 
  hc_add_series(name = "customers", data = count ,type = "column") %>%
  hc_plotOptions(series = list(stacking = FALSE)) %>%
  hc_chart(type = "column")

### luxury & upscale business hotels in USA
cal_business_luxury <- cal_business[cal_business$Class_PL=="Luxury Class"]
cal_business_upsacle <- cal_business[cal_business$Class_PL=="Upscale Class"]
count_lux <- nrow(cal_business_luxury)
count_ups <- nrow(cal_business_upsacle)
counter <- c(count_lux,count_ups)
names(counter) <- c("Luxury","upscale")
barplot(counter,col = "red",xlab = "Business hotels in USA")

cal_leisure_luxury <- cal_leisure[cal_leisure$Class_PL=="Luxury Class"]
cal_leisure_upscale <- cal_leisure[cal_leisure$Class_PL=="Upscale Class"]
count_lux1 <- nrow(cal_leisure_luxury)
count_ups1 <- nrow(cal_leisure_upscale)
counter1 <- c(count_lux1,count_ups1)
names(counter1) <- c("Luxury","upscale")
barplot(counter1,col = "red",xlab = "Leisure hotels in USA")






################################################################################################################################################################3
### Business hotels
lux_hotel <- as.factor(cal_business_luxury$`Hotel Name-Long_PL`)
customer1 <- data.frame(summary(lux_hotel))
customer1

ups_hotel <- as.factor(cal_business_upsacle$`Hotel Name-Long_PL`)
customer2 <- data.frame(summary(ups_hotel))
customer2

## classification of business luxury hotels
promoters_lux <- cal_business_luxury[cal_business_luxury$Likelihood_Recommend_H >8]
detractors_lux <- cal_business_luxury[cal_business_luxury$Likelihood_Recommend_H <=6]
passive <- cal_business_luxury[cal_business_luxury$Likelihood_Recommend_H<=8]
passive1 <-passive[passive$Likelihood_Recommend_H>=7]


#### classification of business upscale hotels
promoters_ups <- cal_business_upsacle[cal_business_upsacle$Likelihood_Recommend_H >8]
detractors_ups <- cal_business_upsacle[cal_business_upsacle$Likelihood_Recommend_H <=6]
passive_ups <- cal_business_upsacle[cal_business_upsacle$Likelihood_Recommend_H<=8]
passive2 <-passive_ups[passive_ups$Likelihood_Recommend_H>=7]


################################################################################################################################################################



######busines luxury hotel promoters 

hotel_promoter <- as.factor(promoters_lux$`Hotel Name-Long_PL`)
count_promoter <- data.frame(summary(hotel_promoter))
View(count_promoter)
summary(hotel_promoter)
which.max(summary(hotel_promoter))

####### business upscale hotel promoters
hotel_promoter1 <- as.factor(promoters_ups$`Hotel Name-Long_PL`)
count_promoter1 <- data.frame(summary(hotel_promoter1))
View(count_promoter1)
summary(hotel_promoter1)
which.max(summary(hotel_promoter1))


### Business Luxury Detractors
hotel_detractor <- as.factor(detractors_lux$`Hotel Name-Long_PL`)
summary(hotel_detractor)
bld <- data.frame(summary(hotel_detractor))

#### Business upscale Detractors
hotel_detractor2 <- as.factor(detractors_ups$`Hotel Name-Long_PL`)
summary(hotel_detractor2)
bud <- data.frame(summary(hotel_detractor2))

## business luxury Passive
hotel_passive <- as.factor(passive1$`Hotel Name-Long_PL`)
blp <- data.frame(summary(hotel_passive))
blp

### business upscale passive
hotel_passive2 <- as.factor(passive2$`Hotel Name-Long_PL`)
bup <- data.frame(summary(hotel_passive2))
bup

## percent of business luxury promoters

business_luxury <- data.frame(customer1,count_promoter,bld,blp)
names(business_luxury) <- c("customer","promoter","detractor","passive")
View(business_luxury)
write.csv(business_luxury,file="lux.csv")
business_upscale <- data.frame(customer2,count_promoter1,bud,bup)
names(business_upscale) <- c("customer","promoter","detractor","passive")
View(business_upscale)
write.csv(business_upscale,file="ups.csv")

business_luxury[,"promoter_percent"] <- (business_luxury[,"promoter"]/business_luxury[,"customer"])*100
business_upscale[,"promoter_percent"] <- (business_upscale[,"promoter"]/business_upscale[,"customer"])*100

## percent of business upscale promoters

## Percent of passive 
business_luxury[,"detractor_perc"] <- (business_luxury[,"detractor"]/business_luxury[,"customer"])*100
business_upscale[,"detractor_perc"] <- (business_upscale[,"detractor"]/business_upscale[,"customer"])*100

## percent of detractors
business_luxury[,"passive_perc"] <- (business_luxury[,"passive"]/business_luxury[,"customer"])*100
business_upscale[,"passive_perc"] <- (business_upscale[,"passive"]/business_upscale[,"customer"])*100

View(business_luxury)
View(business_upscale)
write.csv(business_upscale, file="mydat.csv" )
business_upscale <- read.csv("mydat.csv")

cust <- business_luxury$customer
prom <- business_luxury$promoter_percent
pass <- business_luxury$passive_perc
detrac <- business_luxury$detractor_perc
dag <- data.frame(cust,prom,pass,detrac)


library(ggplot2)

highchart() %>% hc_title(text = "<b>Business hotel analysis</b>",
                         margin = 20, align = "center",
                         style = list(color = "red", useHTML = TRUE)) %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3,title = list(text = "Customer COUNT"),min=0),
    list(showLastLabel = FALSE, opposite = TRUE,title = list(text = "trend"))
  ) %>% 
  hc_xAxis(title=list(text ="Business luxury Hotels" ),categories = cust) %>% 
  hc_add_series(name = "customers", data = cust ,type = "column") %>%
  hc_add_series(name = "promoters", data = prom  ,type = "spline") %>%
  hc_add_series(name = "passive", data = pass ,type = "spline") %>%
  hc_add_series(name = "detrac", data = detrac ,type = "spline") %>%
  hc_plotOptions(series = list(stacking = FALSE)) %>%
  hc_chart(type = "column")


cust1 <- business_upscale$customer
prom1 <- business_upscale$promoter_percent
pass1 <- business_upscale$passive_perc
detrac1 <- business_upscale$detractor_perc
barplot(cust1)

highchart() %>% hc_title(text = "<b>Business hotel analysis</b>",
                         margin = 20, align = "center",
                         style = list(color = "red", useHTML = TRUE)) %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3,title = list(text = "Customer COUNT"),min=0),
    list(showLastLabel = FALSE, opposite = TRUE,title = list(text = "trend"))
  ) %>% 
  hc_xAxis(title=list(text ="Business upscale Hotels" ),categories = cust1) %>% 
  hc_add_series(name = "customers", data = cust1 ,type = "column") %>%
  hc_add_series(name = "promoters", data = prom1  ,type = "spline") %>%
  hc_add_series(name = "passive", data = pass1 ,type = "spline") %>%
  hc_add_series(name = "detrac", data = detrac1 ,type = "spline") %>%
  hc_plotOptions(series = list(stacking = FALSE)) %>%
  hc_chart(type = "column")



################################################# leisure###################3

lux_hotel1 <- as.factor(cal_leisure_luxury$`Hotel Name-Long_PL`)
customer3 <- data.frame(summary(lux_hotel1))
customer3

write.csv(customer3,file="jkl.csv")
customer3 <- read.csv("jkl.csv")

ups_hotel1 <- as.factor(cal_leisure_upscale$`Hotel Name-Long_PL`)
customer4 <- data.frame(summary(ups_hotel1))
write.csv(customer4, file = "dac.csv")
customer4 <- read.csv("dac.csv")
customer4

c1 <- nrow(customer3)
c2 <- nrow(customer4)
cd <- c(c1,c2)
names(cd) <- c("Luxury","Upscale")
barplot(cd,col ="green",xlab = "Leisure Hotels in USA")

## classification of leisure luxury hotels
promoters_lux1 <- cal_leisure_luxury[cal_leisure_luxury$Likelihood_Recommend_H >8]
detractors_lux1 <- cal_leisure_luxury[cal_leisure_luxury$Likelihood_Recommend_H <=6]
passive3 <- cal_leisure_luxury[cal_leisure_luxury$Likelihood_Recommend_H<=8]
passive4 <-passive3[passive3$Likelihood_Recommend_H>=7]


#### classification of leisure upscale hotels
promoters_ups1 <- cal_leisure_upscale[cal_leisure_upscale$Likelihood_Recommend_H >8]
detractors_ups1 <- cal_leisure_upscale[cal_leisure_upscale$Likelihood_Recommend_H <=6]
passive_ups1 <- cal_leisure_upscale[cal_leisure_upscale$Likelihood_Recommend_H<=8]
passive5 <-passive_ups1[passive_ups1$Likelihood_Recommend_H>=7]


################################################################################################################################################################



######leisure luxury hotel promoters 

hotel_promoter_le_lux <- as.factor(promoters_lux1$`Hotel Name-Long_PL`)
count_promoter_le_lux <- data.frame(summary(hotel_promoter_le_lux))
View(count_promoter_le_lux)
summary(hotel_promoter)
which.max(summary(hotel_promoter))

####### leisure upscale hotel promoters
hotel_promoter_le_ups <- as.factor(promoters_ups1$`Hotel Name-Long_PL`)
count_promoter1_le_ups <- data.frame(summary(hotel_promoter_le_ups))
View(count_promoter1_le_ups)

write.csv(count_promoter1_le_ups, file="mydacv.csv")
count_promoter1_le_ups <- read.csv("mydacv.csv")
summary(hotel_promoter_le_ups)
which.max(summary(count_promoter1_le_ups))

### leisure Luxury Detractors
hotel_detractor_le_lux <- as.factor(detractors_lux1$`Hotel Name-Long_PL`)
summary(hotel_detractor_le_lux)
lld <- data.frame(summary(hotel_detractor_le_lux))
write.csv(lld,file = "gh.csv")
hotel_detractor_le_lux <- read.csv("gh.csv")

#### leisure upscale Detractors
hotel_detractor_le_ups <- as.factor(detractors_ups1$`Hotel Name-Long_PL`)
summary(hotel_detractor_le_ups)
lud <- data.frame(summary(hotel_detractor_le_ups))
lud
write.csv(lud,file = "lud.csv")
lud <- read.csv("lud.csv")

## leisure luxury Passive
hotel_passive_le_lux <- as.factor(passive4$`Hotel Name-Long_PL`)
llp <- data.frame(summary(hotel_passive_le_lux))
llp





## percent of leisure luxury promoters

leisure_luxury1 <- data.frame(customer3,count_promoter_le_lux,lld)
names(business_luxury1) <- c("customer","promoter","detractor","passive")
View(business_luxury1)

leisure_upscale1 <- data.frame(customer4,count_promoter1_le_ups,lud,lup)
names(leisure_upscale1) <- c("customer","promoter","detractor","passive")
View(leisure_upscale1)


leisure_luxury[,"promoter_percent"] <- (business_luxury[,"promoter"]/business_luxury[,"customer"])*100
business_upscale[,"promoter_percent"] <- (business_upscale[,"promoter"]/business_upscale[,"customer"])*100

## percent of lesire upscale promoters

## Percent of passive 
leisure_luxury[,"detractor_perc"] <- (leisure_luxury[,"detractor"]/leisure_luxury[,"customer"])*100
leisure_upscale[,"detractor_perc"] <- (leisure_upscale[,"detractor"]/leisure_upscale[,"customer"])*100

## percent of detractors
leisure_luxury[,"passive_perc"] <- (leisure_luxury[,"passive"]/leisure_luxury[,"customer"])*100
leisure_upscale[,"passive_perc"] <- (leisure_upscale[,"passive"]/leisure_upscale[,"customer"])*100

View(leisure_luxury)
View(leisure_upscale)
### PLOTSS

## length of stay vs no of detracotrs
## length of stay vs no of promoters

View(america_sample)
americaz <- america_sample[america_sample$NPS_Type=="Promoter",]
americaz$LENGTH_OF_STAY_C <- as.factor(americaz$LENGTH_OF_STAY_C)
americaz$NPS_Type <- as.factor(americaz$NPS_Type)
sum <- data.frame(summary(america_sample$NPS_Type),Customer)
x <- data.frame (summary(america_sample$NPS_Type))
x <- as.matrix(x)
x <- t(x)
x
x <- as.data.frame(x)

Customer <- as.data.frame(nrow(america_sample))
x <- data.frame(x,Customer)
names(x) <- c("Detra","passive",'promot',"customers")
View(x)

library(ggplot2)
plot1 <- ggplot(x, aes(x= customers))
plot1 <- plot1 + geom_col(aes(y = x$customers), fill = "gray12") 
plot1 <- plot1 + geom_col(aes(y = x$promot), fill = "royalblue") 
plot1 <- plot1 + geom_col(aes(y = x$Detra), fill = "red") 
plot1 <- plot1 + geom_col(aes(y=x$passive,fill="black"))
plot1 <- plot1 + ggtitle("Total Customers vs Promoter vs Detractor vs Passive count") 
plot1

#### Promoter percent in map
americaz <- america_sample[america_sample$NPS_Type=="Promoter",]
americaz$State_PL <- as.factor(americaz$State_PL)
americaz_detractor <- america_sample[america_sample$NPS_Type=="Detractor",]
americaz_detractor$State_PL <- as.factor(americaz_detractor$State_PL)

promotor <- as.data.frame(summary(americaz$State_PL))
detractor <- as.data.frame(summary(americaz_detractor$State_PL))
states <- data.frame(promotor,detractor,americaz$State_PL)


## maps
library(maps)
namevec <- map(database = "state", col = states$`summary(americaz$State_PL)`,fill=T, namesonly=TRUE)

us <- map_data("state") 
mp_det <- ggplot(states, aes(map_id = americaz$State_PL)) 
mp_det <- mp_det + geom_map(map = us, aes(fill = states$summary.americaz.State_PL., color = "black", na.rm = TRUE) )
mp_det <- mp_det + expand_limits(x = us$long, y = us$lat)
mp_det <- mp_det + geom_text(aes(label=americaz$State_PL, x = america$`Property Longitude_PL`, y = america$`Property Latitude_PL`), size = 3, color = "black") 
mp_det <- mp_det + scale_fill_gradient(low = "blue", high = "red", guide = "colorbar") 
mp_det <- mp_det + ggtitle("promoter Percentage in US") + labs (x = "Longitude", y = "Latitude") 
mp_det




####### each state how many nps counts
america_sample$State_PL <- as.factor(america_sample$State_PL)
count_states <- as.data.frame(summary(america_sample$State_PL))
count_states <- as.matrix(count_states)
count_states <- t(count_states)
View(count_states)
names(count_states) <- c("count")
count_states$state <- row.names(count_states)


gt <- ggplot(count_states,aes(x=state))
gt <- gt + geom_point(aes(y = count_states$count), size = 3, color = "red1")
gt <- gt + geom_col(aes(y = count_states$count), width = .1, color = "orange")
gt <- gt + theme (axis.text.x = element_text(angle = 90,hjust = 1))
gt <- gt + theme (panel.background = element_rect(fill = 'white', colour = 'black'))
gt

str(count_states$count)
str(count_zone$count1)
##################### which regions is doing well##################################
americaz$`US Region_PL` <- as.factor(americaz$`US Region_PL`)
americaz_detractor$`US Region_PL` <- as.factor(americaz_detractor$`US Region_PL`)
summary(americaz$`US Region_PL`)
summary(americaz$`US Region_PL`)

count_zone <- as.table(summary(americaz$`US Region_PL`))
count_det <- as.table(summary(americaz_detractor$`US Region_PL`))

names(count_zone) <- c("count")
names(count_det) <- c("detcount")

count_zone_det$zonal <- row.names(count_zone_det)

gh <- data.frame(count_zone,count_zone_det)

gg <- ggplot(count_zone,aes(x=zonal))
gg <- gg + geom_col(aes(y = count_zone$count), width = .1, color = "green")
gg <- gg + theme (axis.text.x = element_text(angle = 75,hjust = 1))
gg <- gg + theme (panel.background = element_rect(fill = 'white', colour = 'blue'))
gg


################# which variable is most influencing###############################

## ggplot

View(america_samplep)

model3 <- lm(formula = Likelihood_Recommend_H ~ Guest_Room_H ,data = america_samplep)
a <- summary(model3)
a <- a$r.squared

model4 <- lm(formula = Likelihood_Recommend_H ~ Tranquility_H ,data = america_samplep)
b <- summary(model4)
b <- b$ r.squared

model5 <- lm(formula = Likelihood_Recommend_H ~ Condition_Hotel_H ,data = america_samplep)
c <- summary(model5)$ r.squared

model6 <- lm(formula = Likelihood_Recommend_H ~ Customer_SVC_H ,data = america_samplep)
d <-summary(model6)$ r.squared

model7 <- lm(formula = Likelihood_Recommend_H ~ Staff_Cared_H ,data = america_samplep)
e <- summary(model7)$ r.squared

model8 <- lm(formula = Likelihood_Recommend_H ~ Internet_Sat_H ,data = america_samplep)
f <- summary(model8)$ r.squared

model9 <- lm(formula = Likelihood_Recommend_H ~ Check_In_H ,data = america_samplep)
g <- summary(model9)$ r.squared

model10 <- lm(formula = Likelihood_Recommend_H ~ F.B_FREQ_H ,data = america_samplep)
h <- summary(model10)$ r.squared

ratings <-data.frame(condition = c( "Guest_Room_H","Tranquility_H","Condition_Hotel_H", "Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H"),
                     count = c(a,b,c,d,e,f,g))
gr <- ggplot(ratings,aes(x=condition,y=count))+geom_col(aes(color="peach",fill="violet"))
gr <- gr + theme (axis.text.x = element_text(angle = 90,hjust = 1))
gr <- gr +theme (panel.background = element_rect(fill = 'white', colour = 'black'))
gr



#################################### Graphs and plots for six months#################
1.) number of cusomter, promoters nad passive for all months
library(readr)
feb <- read_csv("E:/project/IST687-data/IST687-data/out-201402.csv")
mar <- read_csv("E:/project/IST687-data/IST687-data/out-201403.csv")
apr <- read_csv("E:/project/IST687-data/IST687-data/out-201404.csv")
may <- read_csv("E:/project/IST687-data/IST687-data/out-201405.csv")
jul <- read_csv("E:/project/IST687-data/IST687-data/out-201407.csv")
aug <- read_csv("E:/project/IST687-data/IST687-data/out-201408.csv")


## FEB
feb <- feb[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
feb<- feb[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
feb <- feb[!(is.na(feb$Likelihood_Recommend_H)),]
hyatt_grand1 <- feb[feb$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust <- nrow(hyatt_grand1)

nprom <- nrow(hyatt_grand1[hyatt_grand1$Likelihood_Recommend_H >8,])
ndet <- nrow(hyatt_grand1[hyatt_grand1$Likelihood_Recommend_H <=6,])
passive <- hyatt_grand1[hyatt_grand1$Likelihood_Recommend_H<=8,]
npass <- nrow(passive[passive$Likelihood_Recommend_H>=7,])

#### MARCH##
mar <- mar[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
mar <- mar[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
mar <- mar[!(is.na(mar$Likelihood_Recommend_H)),]
hyatt_grand2 <- mar[mar$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust2 <- nrow(hyatt_grand2)

nprom2 <- nrow(hyatt_grand2[hyatt_grand2$Likelihood_Recommend_H >8,])
ndet2 <- nrow(hyatt_grand2[hyatt_grand2$Likelihood_Recommend_H <=6,])
passive2 <- hyatt_grand2[hyatt_grand2$Likelihood_Recommend_H<=8,]
npass2 <- nrow(passive2[passive2$Likelihood_Recommend_H>=7,])
################################ April###############################################################
apr <- apr[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
apr <- apr[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
apr <- apr[!(is.na(apr$Likelihood_Recommend_H)),]
hyatt_grand3 <- apr[apr$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust3 <- nrow(hyatt_grand3)

nprom3 <- nrow(hyatt_grand3[hyatt_grand3$Likelihood_Recommend_H >8,])
ndet3 <- nrow(hyatt_grand3[hyatt_grand3$Likelihood_Recommend_H <=6,])
passive3 <- hyatt_grand3[hyatt_grand3$Likelihood_Recommend_H<=8,]
npass3 <- nrow(passive3[passive3$Likelihood_Recommend_H>=7,])
#################################May###################################################################

may <- may[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
may <- may[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
may <- may[!(is.na(may$Likelihood_Recommend_H)),]
hyatt_grand4 <- may[may$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust4 <- nrow(hyatt_grand4)

nprom4 <- nrow(hyatt_grand4[hyatt_grand4$Likelihood_Recommend_H >8,])
ndet4 <- nrow(hyatt_grand4[hyatt_grand4$Likelihood_Recommend_H <=6,])
passive4 <- hyatt_grand4[hyatt_grand4$Likelihood_Recommend_H<=8,]
npass4 <- nrow(passive4[passive4$Likelihood_Recommend_H>=7,])

#####################################JUNE################################################################
jun <- jun[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
jun <- jun[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
jun <- jun[!(is.na(jun$Likelihood_Recommend_H)),]
hyatt_grand5 <- jun[jun$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust5 <- nrow(hyatt_grand5)

nprom5 <- nrow(hyatt_grand5[hyatt_grand5$Likelihood_Recommend_H >8,])
ndet5 <- nrow(hyatt_grand5[hyatt_grand5$Likelihood_Recommend_H <=6,])
passive5 <- hyatt_grand5[hyatt_grand5$Likelihood_Recommend_H<=8,]
npass5 <- nrow(passive5[passive5$Likelihood_Recommend_H>=7,])

####################################JULY#################################################################
jul <- jul[,-c(102:125,128:136,149:156,161,162,164,170,172,174,180,183,188,193,197,224,227,229:231,235:237)]
jul <- jul[,-c(1:9,13:16,25,27,29,31,33:37,39,45,49,50,55:58,60,63,68,69,74:82,84:87,89:96,99:101)]
jul <- jul[!(is.na(jul$Likelihood_Recommend_H)),]

hyatt_grand6 <- jul[jul$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
ncust6 <- nrow(hyatt_grand6)

nprom6 <- nrow(hyatt_grand6[hyatt_grand6$Likelihood_Recommend_H >8,])
ndet6 <- nrow(hyatt_grand6[hyatt_grand6$Likelihood_Recommend_H <=6,])
passive6 <- hyatt_grand6[hyatt_grand6$Likelihood_Recommend_H<=8,]
npass6 <- nrow(passive6[passive6$Likelihood_Recommend_H>=7,])

###########################################################################################################
##### pplotting

customers <- c(ncust,ncust2,ncust3,ncust4,ncust5,ncust6)
prom <- c(nprom,nprom2,nprom3,nprom4,nprom5,nprom6)
pass <- c(npass,npass2,npass3,npass4,npass5,npass6)
detrac <- c(ndet,ndet2,ndet3,ndet4,ndet5,ndet6)
### highcharts

library(highcharter)
highchart() %>% hc_title(text = "<b>NPS Trend of Grand Hyatt New York </b>",
                         margin = 20, align = "center",
                         style = list(color = "red", useHTML = TRUE)) %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3,title = list(text = "Customer COUNT"),min=0),
    list(showLastLabel = FALSE, opposite = TRUE,title = list(text = "trend"))
  ) %>% 
  hc_xAxis(title=list(text = "FEB to JUlY ANALYSIS"),categories = customers) %>% 
  hc_add_series(name = "population", data = customers ,type = "column") %>%
  hc_add_series(name = "promoter", data = prom ,type = "spline") %>% 
  hc_add_series(name = "passive", data = pass ,type = "spline") %>% 
  hc_add_series(name = "detractor", data = detrac ,type = "spline") %>% 
  
  hc_plotOptions(series = list(stacking = FALSE)) %>%
  hc_chart(type = "column")
###############################################################################################33



##################################################################################### Amenities used by promoters #######
###Business centre
halfyearly_amenities$`Business Center_PL` <- as.factor(halfyearly_amenities$`Business Center_PL`)
bc <- as.data.frame( summary(halfyearly_amenities$`Business Center_PL`))
bc <- c(231610,11581) 
lbc <- c("yes","no")
pie(bc,lbc, main= "Business Centre Used",col=rainbow(length(lbc))) 

### Conference Centres

halfyearly_amenities$Conference_PL <- as.factor(halfyearly_amenities$Conference_PL)
bc <- as.data.frame( summary(halfyearly_amenities$Conference_PL))
cc <- c(485,321748) 
lbcc <- c("yes","no")
pie(cc,lbcc, main= "Conference Centre Used",col=rainbow(length(lbdc))) 

## Dry-Cleaning
halfyearly_amenities$`Dry-Cleaning_PL` <- as.factor(halfyearly_amenities$`Dry-Cleaning_PL`)
dc <- as.data.frame( summary(halfyearly_amenities$`Dry-Cleaning_PL`))
lbdc <- c("yes","no")
pie(dc,lbdc, main= "Dry Cleaning Used",col=rainbow(length(lbdc)))

## Elevators
halfyearly_amenities$Elevators_PL <- as.factor(halfyearly_amenities$`Elevators_PL`)
e <- as.data.frame( summary(halfyearly_amenities$Elevators_PL))
lbe <- c("yes","no")
pie(e,lbe, main= "Elevators Used",col=rainbow(length(lbe)))

## Fitness centres
halfyearly_amenities$`Fitness Center_PL` <- as.factor(halfyearly_amenities$`Fitness Center_PL`)
fc <- as.data.frame( summary(halfyearly_amenities$`Fitness Center_PL`))
lbfc <- c("yes","no")
pie(fc,lbfc, main= "Fitness Centres Used",col=rainbow(length(lbfc)))

##golf
halfyearly_amenities$Golf_PL <- as.factor(halfyearly_amenities$Golf_PL)
summary(halfyearly_amenities$Golf_PL)
lbgf <- c("yes","no")
pie(glf,lbgf, main= "Gold Courses Used",col=rainbow(length(lbgf)))

##Laundry
halfyearly_amenities$Laundry_PL <- as.factor(halfyearly_amenities$Laundry_PL)
summary(halfyearly_amenities$Laundry_PL)
ldy <- c(187155,56036) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Laundry Used",col=rainbow(length(lbgf)))


## Mini bar
halfyearly_amenities$`Mini-Bar_PL` <- as.factor(halfyearly_amenities$`Mini-Bar_PL`)
summary(halfyearly_amenities$`Mini-Bar_PL`)
ldy <- c(65096,178095) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Mini Bar Used",col=rainbow(length(lbgf)))

## Pool indoor
halfyearly_amenities$`Pool-Indoor_PL` <- as.factor(halfyearly_amenities$`Pool-Indoor_PL`)
summary(halfyearly_amenities$`Pool-Indoor_PL`)
ldy <- c(83219,159972) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Indoor Pool Used",col=rainbow(length(lbgf)))

## pool outdoor
halfyearly_amenities$`Pool-Outdoor_PL` <- as.factor(halfyearly_amenities$`Pool-Outdoor_PL`)
summary(halfyearly_amenities$`Pool-Outdoor_PL`)
ldy <- c(147819,95372) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Outdoor Pool Used",col=rainbow(length(lbgf)))

## Restaurant
halfyearly_amenities$Restaurant_PL <- as.factor(halfyearly_amenities$Restaurant_PL)
summary(halfyearly_amenities$Restaurant_PL)
ldy <- c(216817,105416) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Restaurant Used",col=rainbow(length(lbgf)))

## Self Parking
halfyearly_amenities$`Self-Parking_PL` <- as.factor(halfyearly_amenities$`Self-Parking_PL`)
summary(halfyearly_amenities$`Self-Parking_PL`)
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Self Parking Used",col=rainbow(length(lbgf)))

## SPA
halfyearly_amenities$Spa_PL <- as.factor(halfyearly_amenities$Spa_PL)
summary(halfyearly_amenities$Spa_PL) 
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "SPA Used",col=rainbow(length(lbgf)))

## Indoor Corridors
halfyearly_amenities$`Indoor Corridors_PL` <- as.factor(halfyearly_amenities$`Indoor Corridors_PL`)
summary(halfyearly_amenities$`Indoor Corridors_PL`)
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Indoor Corridors Used",col=rainbow(length(lbgf)))

### Limo Service
halfyearly_amenities$`Limo Service_PL` <- as.factor(halfyearly_amenities$`Limo Service_PL`)
summary(halfyearly_amenities$`Limo Service_PL`)
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Limo Services Used",col=rainbow(length(lbgf)))

## Shuttle Services
halfyearly_amenities$`Shuttle Service_PL` <- as.factor(halfyearly_amenities$`Shuttle Service_PL`)
summary(halfyearly_amenities$`Shuttle Service_PL`)
lbdy <- c("yes","no")
pie(ldy,lbdy, main= "Shuttle Services Used",col=rainbow(length(lbgf)))
#############MODEL#################
############################################################################################################################
######################################## Decision Trees#####################################################################

sum(is.na(america$Likelihood_Recommend_H))
is.na(america_sample$Staff_Cared_H)
is.na(america_sample$Internet_Sat_H)


library(rpart)
library(rpart.plot)

fit <- rpart(Likelihood_Recommend_H~.,data=america_sample[,-c(1:5,12:18,21:24,28,33,38:40,42,43,47,58,62,64:69,72,75,78:81,116)],method = "class",control="cp= 5")
summary(fit)
rpart.plot(fit)
prp(fit, type=4, varlen=0, faclen=0, fallen.leaves=TRUE,cex=.85)
plotcp(fit)

character_vars <- lapply(america_sample, class) == "character"
america_sample[, character_vars] <- lapply(america_sample[, character_vars], as.factor)


try_tree <- rpart(Likelihood_Recommend_H, data=america_sample[,-c(1,4,5,16:18,58,116)], method = "class",cp=.02)
summary(try_tree)
rpart.plot(fit)
america_sample$output <-
  ifelse(america_sample$Likelihood_Recommend_H >=1 &america_sample$Likelihood_Recommend_H <=4, "one" ,ifelse(america_sample$Likelihood_Recommend_H >=5 &america_sample$Likelihood_Recommend_H <=8,"Two","Three"))

unique(america_sample$Likelihood_Recommend_H)
predictde <- predict(try_tree,america)
predictde

#####################################Linear Models #######################################################################
library(Metrics)
clean <- read.csv("halfyearly.csv")
america <- clean[clean$Country_PL=="United States",]
america_sample <- america[!(is.na(america$Likelihood_Recommend_H)),]
america_sampler <- america_sample[,c(47:57,118)]
america_sampler <- america_sampler[,-c(2,10,11,12)]
set.seed(123)
split  <- sample(seq_len(nrow(america_sampler)), size = floor(0.75 * nrow(america_sampler)))
train <- america_sampler[split, ]
test <- america_sampler[-split, ]

fit_5 <- lm(formula = Likelihood_Recommend_H~Guest_Room_H+Customer_SVC_H+Condition_Hotel_H+Staff_Cared_H+Internet_Sat_H ,data=(train))
summary(fit_5)

test$predictions <- predict.lm(fit_5,test)
test <- test[!(is.na(test$predictions)),]
test$predictions <- round(test$predictions)
accu <- accuracy(test$predictions,test$Likelihood_Recommend_H)
accu


#######################################################################################################################

hulu <- america_sample[,c(44:46,48:56,86:111)] 
fit3 <- lm(Likelihood_Recommend_H ~.,data= hulu[,c(3:20,21,22,24,25:37)])
summary(fit3)
plot(fit3)

###################################Seperating Detractors, promoter, passive#############################################
detractors_sample <- america_sample[america_sample$Likelihood_Recommend_H<=6,]
nopassive_sample <- america_sample[america_sample$Likelihood_Recommend_H<=8,]
passive_sample <- nopassive_sample[nopassive_sample$Likelihood_Recommend_H>=7,]
promoters_sample <- america_sample[america_sample$Likelihood_Recommend_H >8,]

#####################################Decision trees to understand 3 types of customers###########################################################################
detractors_fit <- rpart(Likelihood_Recommend_H~.,data=detractors_sample[,-c(1:5,12:18,21:24,28,33,38:40,42,43,47,58,62,64:69,72,75,78:81,116)],method = "class",control="cp= 4")
summary(detractors_fit)
prp(detractors_fit, type=4, varlen=0, faclen=0, fallen.leaves=TRUE,cex=.75)
rpart.plot(detractors_fit)

hulu_detractors <- detractors_sample[,c(44:46,48:56,86:111)] 
View(hulu_detractors)
detractors_fit_ovr <- rpart(Likelihood_Recommend_H+Overall_Sat_H~.,data=detractors_sample[,-c(1:5,12:18,21:24,28,33,38:40,42,43,46,58,62,64:69,72,75,78:81,116)],method = "class",control="cp= 4")
summary(detractors_fit_ovr)
rpart.plot(detractors_fit_ovr)
prp(detractors_fit_ovr, type=4, varlen=0, faclen=0, fallen.leaves=TRUE,cex=.75)

promoters_fit <- rpart(Likelihood_Recommend_H~.,data = promoters_sample[,-c(1:5,12:18,21:24,28,33,38:40,42,43,58,62,64:69,72,75,78:81,116)],method="class","cp=4")
summary(promoters_fit)
rpart.plot(promoters_fit)
############################################### A RULES ###################################################################
library(highcharter)
library(arules)
library(arulesViz)
library(readr)
amrca <- read_csv("halfyearly.csv")

america <- amrca[amrca$Country_PL=="United States",]
america_sample <- america[!(is.na(america$Likelihood_Recommend_H)),]
america_samplep <- america_sample[america_sample$`Hotel Name-Long_PL`=="Grand Hyatt New York",]
america_samplep <- america_samplep[america_samplep$NPS_Type=="Passive",]
america_samplep <- america_sample[,c(46:57,118)]

america_samplex <- america_samplep[,c(90:99,101:107,110:112,114,118)]

america_samplep$Guest_Room_H <- ifelse(america_samplep$Guest_Room_H>=8,"high",ifelse(america_samplep$Guest_Room_H<=6,"low","medium"))
america_samplep$Tranquility_H <- ifelse(america_samplep$Tranquility_H>=8,"high",ifelse(america_samplep$Tranquility_H<=6,"low","medium"))
america_samplep$Condition_Hotel_H <- ifelse(america_samplep$Condition_Hotel_H>=8,"high",ifelse(america_samplep$Condition_Hotel_H<=6,"low","medium"))
america_samplep$Customer_SVC_H <- ifelse(america_samplep$Customer_SVC_H>=8,"high",ifelse(america_samplep$Customer_SVC_H<=6,"low","medium"))
america_samplep$Staff_Cared_H <- ifelse(america_samplep$Staff_Cared_H>=8,"high",ifelse(america_samplep$Staff_Cared_H<=6,"low","medium"))
america_samplep$Internet_Sat_H <- ifelse(america_samplep$Internet_Sat_H>=8,"high",ifelse(america_samplep$Internet_Sat_H<=6,"low","medium"))
america_samplep$Check_In_H <- ifelse(america_samplep$Check_In_H>=8,"high",ifelse(america_samplep$Check_In_H<=6,"low","medium"))

america_samplex <- as.data.frame(unclass(america_samplex))



america_samplep <- america_samplep[,-c(1,3,11,12)]
america_samplep$Guest_Room_H <- as.factor(america_samplep$Guest_Room_H)
america_samplep$Tranquility_H <- as.factor(america_samplep$Tranquility_H)
america_samplep$Condition_Hotel_H <- as.factor(america_samplep$Condition_Hotel_H)
america_samplep$Customer_SVC_H <- as.factor(america_samplep$Customer_SVC_H)
america_samplep$Staff_Cared_H <- as.factor(america_samplep$Staff_Cared_H)
america_samplep$Internet_Sat_H <- as.factor(america_samplep$Internet_Sat_H)
america_samplep$Check_In_H <- as.factor(america_samplep$Check_In_H)
america_samplep$NPS_Type <- as.factor(america_samplep$NPS_Type)

library(arules)
rules <- apriori(america_samplep, parameter = list(minlen=3, supp=0.03, conf=0.5),
                 appearance = list(rhs="NPS_Type=Promoter",default="lhs"))
ruz <- sort(rules, decreasing=F,by=c("confidence","lift"))
inspectDT(ruz)
sort_asso <- head(rules)
library(arulesViz)
plot(sort_asso, method="graph", control=list(type="items"))
plot(ruz,col="purple")

#### amenities

View(america_samplex)
rules1 <- apriori(america_samplex[,-c(20,21,25,27,28,29,31,32)], parameter = list(minlen=3, supp=0.03, conf=0.5),
                  appearance = list(rhs="NPS_Type=Promoter",default="lhs"))
ruz1 <- sort(rules1, decreasing=TRUE,by=c("confidence","lift"))
inspectDT(ruz1)
sort_asso1 <- head(ruz1)
library(arulesViz)
plot(sort_asso1, method="graph", control=list(type="items"))
plot(ruz,col="purple")



##############SVM###################
america <- clean[clean$Country_PL=="United States",]
america_sample <- america[!(is.na(america$Likelihood_Recommend_H)),]
america_sampler <- america_sample[,c(46:55,117)]
america_sampler <- america_sampler[,-c(2,10)]


############# Code to convert parameters to low medium and high###############

america_sampler$Guest_Room_H <- ifelse(america_sampler$Guest_Room_H>=8,"high",ifelse(america_sampler$Guest_Room_H<=6,"low","medium"))
america_sampler$Tranquility_H <- ifelse(america_sampler$Tranquility_H>=8,"high",ifelse(america_sampler$Tranquility_H<=6,"low","medium"))
america_sampler$Condition_Hotel_H <- ifelse(america_sampler$Condition_Hotel_H>=8,"high",ifelse(america_sampler$Condition_Hotel_H<=6,"low","medium"))
america_sampler$Customer_SVC_H <- ifelse(america_sampler$Customer_SVC_H>=8,"high",ifelse(america_sampler$Customer_SVC_H<=6,"low","medium"))
############## Running Model###########################
set.seed(123)
split <- sample(seq_len(nrow(america_sampler)), size = floor(0.75 * nrow(america_sampler)))
train1 <- america_sampler[split, ]
test1 <- america_sampler[-split, ]

### BUILDING SVM
library(e1071)
svmModel1 <- svm(NPS_Type ~ Guest_Room_H + Tranquility_H + Condition_Hotel_H 
                 + Customer_SVC_H ,data=train1, kpar="automatic", C=5,cross=3, prob.model=TRUE)

summary(svmModel1)

############## Projecting it on Test Data########################
library(Metrics)
test1 <- na.omit(test1)
test1 <- test1[!(is.na(test1$NPS_Type)),]
test1$predictions <- predict(svmModel1,test1)
View(test1)
acc1 <- accuracy(test1$predictions,test1$NPS_Type)
acc1


plot2 = ggplot(test1, aes(NPS_Type, predictions)) +
  geom_point() +
  ggtitle("Plot of Likelihood_to Recommend V/s predictions with Errors of SVM") 
plot2

## Chart for the above obtained predictions
test$Likelihood_Recommend_H <- as.numeric(test$Likelihood_Recommend_H)
test$predictions1 <- as.numeric(test$predictions1)
library(ggplot2)
plot2 = ggplot(test1, aes(NPS_Type, predictions, size=error1)) +
  geom_point() +
  ggtitle("Plot of Likelihood_to Recommend V/s predictions with Errors of SVM") 
plot2
################# NAIVE BAYES model#####################################################
library(Metrics)
library(naivebayes)
train2$NPS_Type <- train2[!(is.na(train2$NPS_Type)),]
model1 <- naive_bayes( as.factor(Likelihood_Recommend_H)~Guest_Room_H + Tranquility_H + Condition_Hotel_H 
                       + Customer_SVC_H ,data=train2)

test2$predictions <- predict(model1,test2)
accu3 <- accuracy(test2$predictions,test2$Likelihood_Recommend_H)
accu3


######################################Conditional Regression forest#############################
install.packages('party')
library(party)
set.seed(123)
split3 <- sample(seq_len(nrow(america_sampler)), size = floor(0.75 * nrow(america_sampler)))
train3 <- america_sampler[split3, ]
test3 <- america_sampler[-split3, ]

fit <- cforest(as.factor(Likelihood_Recommend_H) ~ Guest_Room_H + Tranquility_H + Condition_Hotel_H 
               + Customer_SVC_H,
               data = train3)
####################XGBOOST###################
library(Matrix)
library(xgboost)
set.seed(1234)
ind <- sample(2,nrow(america_sampler),replace = T,prob = c(0.80,0.20))
train3 <- america_sampler[ind==1,]
test3 <- america_sampler[ind==2,]
### one hot encoding for factor variables
train3m <- sparse.model.matrix(Likelihood_Recommend_H ~  Condition_Hotel_H+Customer_SVC_H+ Guest_Room_H+NPS_Type,-1,data= train3)
head(train3m)

train_label <- train3m[,"NPS_TypePromoter"]
train_label
train_matrix <- xgb.DMatrix(data=as.matrix(train3m),label=train_label)

test3m <- sparse.model.matrix(Likelihood_Recommend_H ~  Condition_Hotel_H+Customer_SVC_H+ Guest_Room_H+NPS_Type,-1,data= train3)
test_label <- test3m[,"NPS_TypePromoter"]
test_matrix <- xgb.DMatrix(data = as.matrix(test3m),label=test_label)
nc <- length(unique(train_label))
xgb_params <- list("objective"="multi:softprob","eval_matric"="mlogloss","num_class"=nc)
watchlist <- list(train = train_matrix,test=test_matrix)
bst_model <- xgb.train(params = xgb_params,data=train_matrix,nrounds =10, watchlist = watchlist)
