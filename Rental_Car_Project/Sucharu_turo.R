## SUCHARU GUPTA, sucharu7115@gmail.com ########

####Load Dataset######
Vehicle_Data <- read.csv("C:/Users/Sucharu/Desktop/vehicles_(6).csv")
View(Vehicle_Data)
Reservation_Data <- read.csv("C:/Users/Sucharu/Desktop/reservations_(5).csv")
View(Reservation_Data)
##############################################

######## Checking types of variables###############
dim(Vehicle_Data)                          # dimension of Vehicle dataset
dim(Reservation_Data)                      # dimension of reservation dataset
str(Reservation_Data)                       # type of variables of Reservation dataset
Reservation_Data$vehicle_id <- sort(Reservation_Data$vehicle_id)  # sorting by vehicle_id varibale
View(Reservation_Data)                       # View reservation dataset  

##### Convert variables as factors ######## 
# we need to convert reservationtype, technology and street_parked as factor variables to analyse further##

Reservation_Data$reservation_type <- as.factor(Reservation_Data$reservation_type)
str(Reservation_Data)
Vehicle_Data$technology <- as.factor(Vehicle_Data$technology)
Vehicle_Data$street_parked <- as.factor(Vehicle_Data$street_parked)
str(Vehicle_Data)

####### Create & Calculate a new variable "Total no. of reservations" ###########
####### This is our response variable ################

tab <- table(Reservation_Data$vehicle_id)         # Gives frequency table according to vehicle_id
tot_rese <- data.frame(tab)                       # convert table to data frame
tot_rese[1:20,]                                   # View first 20 rows 
tab1 <- table(Reservation_Data$reservation_type)   # Frequency table according to reservation_type 
View(Vehicle_Data)                                # View Vehicle dataset
str(Vehicle_Data)                                 # View structure of Vehicle dataset
names(tot_rese)[1] <- "Var1"                      # change name of first variable of table to var1
names(tot_rese)[2] <- "Freq"                      # change name of 2nd variable to Freq


### Creating Reservation Frequency TABLE Including all three reservation types#####

data1 <- data.frame(tot_rese$Freq, row.names = tot_rese$Var1)

data2 <- data.frame( freq = rep(0,1000), row.names = seq(1, 1000) )
output <- data2
apply(matrix(rownames(data1),ncol=1),1,
  function(x){
    output[x,1] <<- data1[x,1];
    return(NULL)
  }
  )
output[1:20,]
###########MERGING WITH DATASET#######################

new <- data.frame(output$freq)
Vehicle_Data <- data.frame(Vehicle_Data)
data_freq <- cbind(Vehicle_Data, new$output.freq)
View(data_freq)
colnames(data_freq)[8] <- "Total_reservations";
str(data_freq)

######Analysis with Plots ##########

#### For plots used libraries and packages#############
install.packages("ggplot2")
install.packages("MASS")
install.packages("plotly")
library(ggplot2)
library(MASS)
library(plotly)

## Plot of correlation between actual price and reccomended price##########

ggplot(data_freq,aes(y=recommended_price,x=actual_price))+geom_point(color="violetred" )
cor(data_freq$actual_price,data_freq$recommended_price)      ###########value of correlation= 75.9% ########

####Plot of reservation type########

plot(Reservation_Data$reservation_type, col= "coral",xlab= "Reservation_type", ylab="Count")

#######Plot of technology and total reservations #####################

ggplot(data_freq, aes(x =Total_reservations,fill = technology)) + geom_histogram(col="aquamarine1",binwidth = 1)

######PLots of prices with technology#############

ggplot(data_freq,aes(y=recommended_price,x=technology))+geom_point(color="violetred" )
ggplot(data_freq,aes(y=actual_price,x=technology))+geom_point(color="violetred" )

######### Plot with num images, technoly and total reservations#########

p <- qplot(Total_reservations,num_images, data=data_freq, colour=technology)
ggplotly(p)
p <- qplot(Total_reservations,recommended_price, data=data_freq, colour=technology)
ggplotly(p)
p <- qplot(Total_reservations,actual_price, data=data_freq, colour=technology)
ggplotly(p)

 
#######   Analysis with Models ################

max(data_freq$Total_reservations)      # Max value of total reservations
min(data_freq$Total_reservations)      # min value of total reservation
mean(data_freq$Total_reservations)     # mean of Total reservations
var(data_freq$Total_reservations)      # variance of total reservations

########As variance is not equal to mean so it means this is overdispersed data So Using Negative Binomial Models########

##################Negative Binomial Models##############################################

M1 <- glm.nb(data_freq$Total_reservations ~ technology+recommended_price+actual_price+num_images+
                +street_parked+description, data = data_freq )
summary(M1)

M2 <- glm.nb(data_freq$Total_reservations ~ technology+actual_price+num_images+
               +street_parked+description, data = data_freq )
summary(M2)

M3 <- glm.nb(data_freq$Total_reservations ~ technology+recommended_price+num_images
             +street_parked+description , data = data_freq )
summary(M3)

M4 <- glm.nb(data_freq$Total_reservations ~ technology+num_images+street_parked+description
             , data = data_freq )
summary(M4)
#Here in model 1 we got recommended_price, technology and num_images comes as significant features.
# In modle1 Actual price, No. of images,Recommended price comes out significant###########
# In this case negative binomial model 3 (without actual price)is  giving good results ###########
# In model 4, if  we don't include price at all then technology and num_images comes significant. 

################ Analysis Based on Daily Reservation ##########################################
# Creating a new variable with only reservations according to hourly type ###############

tab1 <- table(Reservation_Data[Reservation_Data$reservation_type==1,1])  # Frequency table of only hourly reservations
tab1[1:20]                                                               # View first 20 rows of table
tot_rese1 <- data.frame(tab1)                                            # Convert to data frame
names(tot_rese1)[1] <- "Var2"                                            # giving names 
names(tot_rese1)[2] <- "Freq"
tot_rese1[1:20,]

##########Creating Reservation Frequency TABLE with reservation types = 1#############

data1 <- data.frame(tot_rese1$Freq, row.names = tot_rese1$Var2)

data2 <- data.frame( freq = rep(0,1000), row.names = seq(1, 1000) )
output1 <- data2
apply(matrix(rownames(data1),ncol=1),1,
      function(x){
        output1[x,1] <<- data1[x,1];
        return(NULL)
      }
)
output1[1:20,]          # gives numbers of reservations made hourly by each vehicle

############### Creating & MERGING data_freq1 with with freq of reservation hourly only ################

new1 <- data.frame(output1$freq)             
Vehicle_Data <- data.frame(Vehicle_Data)
data_freq1 <- cbind(Vehicle_Data, new1)          # Created new dataset datafreq1 with new varaiable tot_resv_hourly
View(data_freq1)
colnames(data_freq1)[8] <- "Tot_resv_hourly";
str(data_freq1)

############ Analysis with models#############

max(data_freq1$Tot_resv_hourly)
min(data_freq1$Tot_resv_hourly)
mean(data_freq1$Tot_resv_hourly)
var(data_freq1$Tot_resv_hourly)
table(data_freq1$Tot_resv_hourly)

## More zeros values in frequency table so considering zero inflated models###########

## Zero inflated data Model with neg binomial###################

install.packages("pscl")
library(pscl)
summary(model1 <- zeroinfl(Tot_resv_hourly ~ technology+recommended_price+actual_price+num_images+
                     +street_parked+ description, data = data_freq1 , dist = "negbin"))

summary(model2 <- zeroinfl(Tot_resv_hourly ~ technology+recommended_price+num_images+description
                     , data = data_freq1 , dist = "negbin"))

################### Without zero inflation Models  ################

summary(Model3 <- glm.nb(Tot_resv_hourly ~ technology+actual_price+num_images+
                        +street_parked+ description, data = data_freq1))

summary(Model4 <- glm.nb(Tot_resv_hourly ~ technology+actual_price+recommended_price+num_images+
                           +street_parked+ description, data = data_freq1))

summary(Model5 <- glm.nb(Tot_resv_hourly ~ technology+recommended_price+num_images
                           , data = data_freq1))

########## Both zero infl and without zero inflation giving same results so will go with without zeroinflated###
# In modle4 Actual price, No. of images,Recommended price comes significant###########
# In this case negative binomial model 5 (without actual price)is  doing good ###########
########Model5 technology and num_images along with recommended price comes out significant######### 


################Analysis based on Daily reservation ###########################
# Creating a new variable with only reservations according to Daily type ###############

tab2 <- table(Reservation_Data[Reservation_Data$reservation_type==2,1])  # Frequency table with daily reservation
tab2[1:20]
tot_rese2 <- data.frame(tab2)
names(tot_rese2)[1] <- "Var3"
names(tot_rese2)[2] <- "Freq"
tot_rese2[1:20,]

########Creating Reservation Frequency TABLE With reservation type = 2#############

data1 <- data.frame(tot_rese2$Freq, row.names = tot_rese2$Var3)

data2 <- data.frame( freq = rep(0,1000), row.names = seq(1, 1000) )
output2 <- data2
apply(matrix(rownames(data1),ncol=1),1,
      function(x){
        output2[x,1] <<- data1[x,1];
        return(NULL)
      }
)
output2[1:20,]

##### Creating & MERGING  datafreq2 with new variable tot_resv_daily################

new2 <- data.frame(output2$freq)
Vehicle_Data <- data.frame(Vehicle_Data)
data_freq2 <- cbind(Vehicle_Data, new2)
View(data_freq2)
colnames(data_freq2)[8] <- "Tot_resv_daily";    # created new variable with only hourly reservations
str(data_freq2)

#######################ANAlysis with Models ##############

max(data_freq2$Tot_resv_daily)
min(data_freq2$Tot_resv_daily)
mean(data_freq2$Tot_resv_daily)
var(data_freq2$Tot_resv_daily)
table(data_freq2$Tot_resv_daily)

## Zero inflated data Model with zeroinflated and neg binomial########

library(pscl)
summary(model1 <- zeroinfl(Tot_resv_daily ~ technology+recommended_price+num_images+actual_price+
                             +street_parked+ description, data = data_freq2 , dist = "negbin"))

summary(model2 <- zeroinfl(Tot_resv_daily ~ technology+recommended_price+num_images+
                             +street_parked+ description, data = data_freq2 , dist = "negbin"))

##### Without zero inflation data using negative binomial###########

summary(Model3 <- glm.nb(Tot_resv_daily ~ technology+actual_price+num_images++recommended_price+
                           +street_parked+ description, data = data_freq2))
summary(Model4 <- glm.nb(Tot_resv_daily ~ technology+recommended_price+num_images
                           , data = data_freq2))
summary(Model5 <- glm.nb(Tot_resv_daily ~ technology+actual_price+num_images
                         , data = data_freq2))

##########Both zero infl and without zero inflt giving same results so will go with without zeroinflated###
# In modle3 Actual price, No. of images,Recommended price comes significant###########
# In this case negative binomial model 4 (without actual price)is  doing good ###########
########In model4 technology and num_images and recommended_price comes out significant ######### 


#############Analysis based on Weekly reservation only#############

tab3 <- table(Reservation_Data[Reservation_Data$reservation_type==3,1])
tab3[1:20]
tot_rese3 <- data.frame(tab3)
names(tot_rese3)[1] <- "Var4"
names(tot_rese3)[2] <- "Freq"
tot_rese3[1:20,]

### Creating Reservation Frequency TABLE Including reservation types = 3##########

data1 <- data.frame(tot_rese3$Freq, row.names = tot_rese3$Var4)

data2 <- data.frame( freq = rep(0,1000), row.names = seq(1, 1000) )
output3 <- data2
apply(matrix(rownames(data1),ncol=1),1,
      function(x){
        output3[x,1] <<- data1[x,1];
        return(NULL)
      }
)
output3[1:20,]

### Creating & MERGING with new dataset datafreq3 with new varible tot_resv_Weekly###############

new3 <- data.frame(output3$freq)
Vehicle_Data <- data.frame(Vehicle_Data)
data_freq3 <- cbind(Vehicle_Data, new3)
View(data_freq3)
colnames(data_freq3)[8] <- "Tot_resv_Weekly";     # Created with daatset with only weekly reservations
str(data_freq3)

############ANalysis with models################

max(data_freq3$Tot_resv_Weekly)
min(data_freq3$Tot_resv_Weekly)
mean(data_freq3$Tot_resv_Weekly)
var(data_freq3$Tot_resv_Weekly)
table(data_freq3$Tot_resv_Weekly)


##################### zero inflated data Model with neg binomial########

library(pscl)
summary(model1 <- zeroinfl(Tot_resv_Weekly ~ technology+recommended_price+num_images+actual_price+
                             +street_parked+ description, data = data_freq3, dist = "negbin" ))

summary(model2 <- zeroinfl(Tot_resv_Weekly ~ technology+recommended_price+num_images+
                             +street_parked+ description, data = data_freq3 , dist = "negbin"))

######################## Without zero inflation modelling ###########

summary(Model3 <- glm.nb(Tot_resv_Weekly ~ technology+actual_price+num_images+recommended_price+
                        +street_parked+ description, data = data_freq3))

summary(Model4 <- glm.nb(Tot_resv_Weekly ~ technology+recommended_price+num_images
                          , data = data_freq3))

summary(Model5 <- glm.nb(Tot_resv_Weekly ~ technology+actual_price+num_images
                         , data = data_freq3))

##########Both zero infl and without zero inflt giving same results so will go with without zeroinflated###
# In modle3 Actual price, No. of images,Recommended price comes significant###########
# In this case negative binomial model 4 (without actual price)is  doing good ###########
########In model4 technology and num_images and recommended_price comes out significant ######### 

