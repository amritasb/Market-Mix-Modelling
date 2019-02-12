####################################################################################################

###
### Ecommerce Capstone Project
###
### Team            - Amrita Bhadrannavar
###                 - Shafeeq Rahiman
###
### Submission date - 21-10-2018
###
### ASSUMPTIONS 
### Data Set File   - Following data files are available in the working directory :
###                     - ConsumerElectronics.csv
###                     - Media data and other information.xlsx
###                     - adstock.csv - Adstock data created manually from Media file
###

####################################################################################################

### DISCLAIMER
### 
### Based on feedback received in mid submission results and inputs from mentor, changes have been 
### made in feature engineering, data preparation and EDA. Additional data has been derived and 
### variables name changes are also done.    

####################################################################################################

### STRUCTURE OF THE CODE
###
### Complete code is split into file based on stages of model building and must be run in same 
### order as provided below
### 01_DataPrep_And_EDA.R   - Data Understanding, Data Preparation, Feature Engineering and EDA
### 02_Modelling_And_Evaluation.R - Data preparation for each modelling types, common functions  
###                       used in all modelling types, 5 types modelling for each sub category  
###                       and tabulating all results for model selection

####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- ENVIRONMENT SETUP -----------------------------------------##
## -----------------------------------------------------------------------------------------------##

## Load required libraries
library(doParallel)
library(dplyr)
library(lubridate)
library(gdata)
library(zoo)
library(reshape2)
library(xlsx)
library(ggplot2)
library(DataCombine)
library(grid)
library(gridExtra)
library(car)
library(MASS)
library(DAAG)

# In case xlsx library does not work, please add this line before loading the library
# Sys.setenv(JAVA_HOME='path\to\java\)


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- DATA UNDERSTANDING-----------------------------------------##
## -----------------------------------------------------------------------------------------------##

## Dataset available for this project are as listed below:
## 1. "Media data and other information.xlsx"
## 2. "ConsumerElectronics.csv"
## 3. "Product Details.docx"

## "ConsumerElectronics.csv" has Order level data
## "Media data and other information.xlsx" includes :
##    - Monthly spends on various advertising channels
##    - Days when there was any special sale
##    - Monthly NPS score
## "Product Details.docx" has additional information on products

## For building MMM, we would need to use data in "ConsumerElectronics.csv" and "Media data and 
## other information.xlsx" and refer to "Product Details.docx" as needed.


## -----------------------------------------------------------------------------------------------##
## UNDERSTANDING DATA FILE - "ConsumerElectronics.csv"

## Load the dataset
CE_Data <- read.csv("ConsumerElectronics.csv",  stringsAsFactors = FALSE)

## Check the dimensions
dim(CE_Data)        
# 1648824 obs with 20 variables 

## View the dataset
View(CE_Data)          

## Check the structure of dataset
str(CE_Data)

## Check for first few rows of dataset
head(CE_Data)

## Check for data distribution
summary(CE_Data)


## -----------------------------------------------------------------------------------------------##
## UNDERSTANDING DATA FILE - "Media data and other information.xlsx"

## "Media data and other information.xlsx" consists of 4 sheets of data
## Sheet 1 -  Product List - Information of each product with its frequency and % of orders
## Sheet 2 -  Media Investment - Marketing investment done in different media. This information will
##            be used to create adstock of each of the 'commercial spends'. Also total investment
##            can be used for analysis
## Sheet 3 -  Special Sale Calendar - List of dates when there was a special sale. Data in this
##            is manually derived and used for analysis
## Sheet 4 -  Monthly NPS Score - NPS score provided per month basis. Data is imported and derived
##            as needed for analysis


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- DATA PREPARATION ------------------------------------------##
## -----------------------------------------------------------------------------------------------##


## -----------------------------------------------------------------------------------------------##
## DATA FILE - "ConsumerElectronics.csv"

## Clean up colnames as required
colnames(CE_Data)
colnames(CE_Data)[1]  <- "fsn_id"
colnames(CE_Data)[11] <- "order_payment_type"
colnames(CE_Data)[19] <- "mrp"

## Check for NA
sapply(CE_Data, function(x){sum(is.na(x))}) 
# gmv     - 4904 obs
# cust_id - 4904 obs
# pincode - 4904 obs
# ~0.4% obs have NA values in each column



## Column wise data analysis
## Analyse each column for data quality issues and manipulate as needed

## Column - order_date , Year, Month
# As per requirement, only obs between July-2015 to June-2016 are to be considered for analysis.
nrow(subset(CE_Data, (Month >=7 & Year==2015|Month <=6 & Year==2016)))
# 1648215 obs are in time range July-2015 to June 2016
# 906 obs out of time range and will be eliminated


## Column - gmv
nrow(filter(CE_Data, gmv < 0))
# No obs with negative gmv
nrow(filter(CE_Data, gmv == 0)) 
# 1349 obs with 0 gmv
# Negligible value like 1 can be assigned to obs with gmv=0

# GMV should always be less than mrp
# mrp per order is mrp*units
nrow(subset(CE_Data, (mrp*units)<gmv))
# 38569 obs have gmv > mrp
# These obs will have to be eliminated


## Column - units
nrow(filter(CE_Data, units < 0))  # 0 obs
nrow(filter(CE_Data, units == 0)) # 0 obs


## Column - deliverybdays
table(CE_Data$deliverybdays)  
# negative as well as very high values 
# \\N has 1312972 occurances ~79%
plot(as.factor(CE_Data$deliverybdays))
# \\N and negative values can be replaced with 0
qplot(as.factor(as.integer(CE_Data$deliverybdays)), stat= "count")


## Column - deliverycdays
table(CE_Data$deliverycdays)  
# negative as well as very high values
# \\N has 1312971 occurances ~79%
plot(as.factor(CE_Data$deliverycdays))
# \\N and negative values can be replaced with 0


## Column - order_payment_type
unique(CE_Data$order_payment_type)  # "COD", "Prepaid"
table(CE_Data$order_payment_type)   
plot(as.factor(CE_Data$order_payment_type))
# COD is more prefered by more than 50%


## Column - sla
table(CE_Data$sla)
# records with 0 sla - means same day delivery
# few records with 100+ and even 1000 days
plot(as.factor(CE_Data$sla))


## Column - pincode
length(unique(CE_Data$pincode)) # 7565 unique values 
# But we have 4904 NA values.
head(CE_Data$pincode)
summary(CE_Data$pincode)
# Negative pincode values are also seen


## Column - cust_id
length(unique(CE_Data$cust_id)) ## 1201090 distinct customers 
# we have 4904 NA values also
head(CE_Data$cust_id)
# Also negative values which doesnt make sense


## Column - product_analytic_super_category
unique(CE_Data$product_analytic_super_category) 
# Single value CE 


## Column - product_analytic_sub_category
unique(CE_Data$product_analytic_sub_category)  
## 14 distinct values 
# we need to consider only "CameraAccessory", "HomeAudio" & "GamingAccessory"
table(CE_Data$product_analytic_sub_category)
# "CameraAccessory" = 239508
# "HomeAudio"       = 124860
# "GamingAccessory" = 201899


## Column - product_analytic_category
unique(CE_Data$product_analytic_category)       
## 5 distinct values 


## Column - product_analytic_vertical
unique(CE_Data$product_analytic_vertical)       
## 74 distinct values 
str(CE_Data$product_analytic_vertical)
# Convert to factor type
CE_Data$product_analytic_vertical <- as.factor(CE_Data$product_analytic_vertical)


## Column - mrp
summary(CE_Data$mrp)
nrow(filter(CE_Data, mrp < 0)) 
# 0 obs with negative mrp
nrow(filter(CE_Data, mrp == 0)) 
# 5308 obs with 0 mrp. 
# These obs will have to be eliminated as these could correspond to freebies


## Column - product_procurement_sla
table(CE_Data$product_procurement_sla)
# Negative values and 0 values as well
# Update all negative values to 0



## Data manipulations required
# - Convert to weekly data
# - Remove NA values
# - Assign a negligible value to gmv where gmv is 0 & delete obs with gmv>mrp
# - Eliminate obs with 0 mrp
# - Replace \N and 0 in deliverybdays and deliverycdays with 0
# - Replace negative values with 0 in product_procurement_sla


## Convert to Weekly Data
CE_Data <- subset(CE_Data, (Month >=7 & Year==2015|Month <=6 & Year==2016))
# 1648215 records remaining

# Convert to weekly levels
CE_Data$order_date <- date(CE_Data$order_date)
# create new column week of year 
CE_Data$WeekNo <- week(CE_Data$order_date)

# Week from jan 2016 should have continuity from dec 2015
# So consider jan 2016 as week 54 and +
CE_Data$WeekNo<- ifelse(CE_Data$WeekNo<=26 & CE_Data$Year==2016,CE_Data$WeekNo+53,CE_Data$WeekNo)


## Remove NA values 
sum(is.na(CE_Data))
# removing rows with NA values
CE_Data <- na.omit(CE_Data)


## gmv values handling
# Assign a negligible value to gmv where gmv is 0.
CE_Data$gmv[which(CE_Data$gmv==0 & CE_Data$mrp!=0)] <- 1
# Eliminate obs with gmv>mrp
CE_Data <- subset(CE_Data, (mrp*units)>=gmv)
# 1604753 obs remaining


## Eliminate obs with 0 mrp
CE_Data <- subset(CE_Data, mrp!=0)
summary(CE_Data$mrp)


## Replace \N and 0 in deliverybdays and deliverycdays with 0
CE_Data$deliverybdays[CE_Data$deliverybdays < 0] = 0
CE_Data$deliverycdays[CE_Data$deliverycdays < 0] = 0
CE_Data$deliverycdays[CE_Data$deliverybdays == "\\N"] = 0
CE_Data$deliverycdays[CE_Data$deliverycdays == "\\N"] = 0
CE_Data$deliverybdays <- as.numeric(CE_Data$deliverybdays)
CE_Data$deliverycdays <- as.numeric(CE_Data$deliverycdays)


## Replace -ve values to 0 in product_procurement_sla
CE_Data$product_procurement_sla [CE_Data$product_procurement_sla < 0] <- 0



## -----------------------------------------------------------------------------------------------##
## DATA FILE - "Media data and other information.xlsx"

## Sheet 2 -  Media Investment
## Adstock data is built using excel and final data is saved in csv format and imported for model
## building
## Adstock data creation steps done :
## - Data for 9 channels of investment are provided (In Cr. INR)
## - Data is at month level
## - Two channels - Radio and Other have data only for 3 months - so these are eliminated
## - Remaining 7 channels of investment are used to create adstock data.
## - Number of weeks are calculated in each month and monthly investment is averaged across each 
##   week of the month.
## - Adstock rate is assumed to be 0.6
## - Final calculation is based on the weekly investment, prior week investment and adstock rates.
## - Final adstock data is available in file "adstock.csv"
Adstock <- read.csv("adstock.csv")
colnames(Adstock)[1] <- "WeekNo"


## Total investment data
# Import total investment data
TotalInvest_Data <- read.xlsx("Media data and other information.xlsx", sheetIndex=2, header=TRUE, 
                      colIndex=c(seq(2,4)))
View(TotalInvest_Data)

# Transform dataframe
colnames(TotalInvest_Data) <- c("Year","Month","total_investment")
TotalInvest_Data <- TotalInvest_Data[-1,]

View(TotalInvest_Data)



## Special Sale Data - Derive special sale data manually from Sheet 3
## Year 2015
##    - Eid & Rathayatra sale (18-19th July)
##    - Independence Sale (15-17th Aug)
##    - Rakshabandhan Sale (28-30th Aug)
##    - Daussera sale (17-15th Oct)
##    - Big Diwali Sale (7-14th Nov)
##    - Christmas & New Year Sale (25th Dec'15 - 3rd Jan'16)
## Year 2016
##    - Republic Day (20-22 Jan)
##    - BED (1-2 Feb)
##    - FHSD (20-21 Feb)
##    - Valentine's Day (14-15 Feb)
##    - BSD-5 (7-9 Mar)
##    - Pacman (25-27 May)
Holiday_Data <- as.Date(c("2015-07-18","2015-07-19",
                  "2015-08-15","2015-08-16","2015-08-17",
                  "2015-08-28","2015-08-29","2015-08-30",
                  "2015-10-15","2015-10-16","2015-10-17",
                  "2015-11-07","2015-11-08","2015-11-09","2015-11-10",
                  "2015-10-11","2015-10-12","2015-11-13","2015-11-14",
                  "2015-12-25","2015-12-26","2015-12-27","2015-12-28",
                  "2015-12-29","2015-12-30","2016-01-01","2016-01-02","2016-01-03",
                  "2016-01-20","2016-01-21","2016-01-22",
                  "2016-02-01","2016-02-02",
                  "2016-02-20","2016-02-21",
                  "2016-02-14","2016-02-15",
                  "2016-03-07","2016-03-08","2016-03-09",
                  "2016-05-25","2016-05-26","2016-05-27"))

# Retrieve week of each holiday 
Holiday_Data <- data.frame(week(Holiday_Data), year(Holiday_Data))
# Update weekno to reflect 2016 weeks to continue at 54+ week
colnames(Holiday_Data) <- c("WeekNo","Year")
Holiday_Data$WeekNo <- ifelse(Holiday_Data$WeekNo<=26 & Holiday_Data$Year==2016,
                              Holiday_Data$WeekNo+53,Holiday_Data$WeekNo)

# Calculate no of special days or holidays in each week
Holiday_Data$spl_day <- 1
Holiday_Data <- aggregate(spl_day ~ WeekNo,Holiday_Data,sum)

# Cleanup dataframe - delete year
Holiday_Data$Year <- NULL

View(Holiday_Data)



## Monthly NPS Score - Derive monthly NPS score from Sheet 4 and convert to required format
# Import Monthly NPS Score Data
NPS_Data <- read.xlsx("Media data and other information.xlsx", sheetIndex=4, header=FALSE, 
                       colIndex=c(seq(2,13)))
View(NPS_Data)
 
# Transpose dataframe
NPS_Data <- as.data.frame(t(NPS_Data), stringsAsFactors=FALSE)
# Update colname
colnames(NPS_Data)[1] <- "Month_Year"
colnames(NPS_Data)[2] <- "NPS"
# Update data types
str(NPS_Data)
NPS_Data$NPS <- as.numeric(NPS_Data$NPS)
NPS_Data$Month <- as.character(NPS_Data$Month)
 
# Derive data - Split Month_Year into two columns
NPS_Data$Year <- year(parse_date_time(NPS_Data$Month_Year, orders="my"))
NPS_Data$Month <- month(parse_date_time(NPS_Data$Month_Year, orders="my"))
 
# Cleanup dataframe - Delete Month_Year
NPS_Data$Month_Year <- NULL

View(NPS_Data)



## -----------------------------------------------------------------------------------------------##
## DATA CONSOLIDATION

## Merge Total Investment data with COnsumer Electroninc Order records
CE_Data <- merge(CE_Data, TotalInvest_Data, by=c("Month","Year"), all.x=TRUE)

## Merge NPS Data with Consumer Electronics Order records
CE_Data <- merge(CE_Data, NPS_Data, by=c("Month","Year"), all.x=TRUE)

## Filter data based on sub category
CE_CamAcc <- CE_Data[CE_Data$product_analytic_sub_category=="CameraAccessory",]
CE_GamAcc <- CE_Data[CE_Data$product_analytic_sub_category=="GamingAccessory" ,] 
CE_HomAud <- CE_Data[CE_Data$product_analytic_sub_category=="HomeAudio",]



####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- FEATURE ENGINEERING----------------------------------------##
## -----------------------------------------------------------------------------------------------##

## For each sub category - Gaming Accessory, Camera Accessory and Home Audio, additional features or 
## variables will be derived which will assist in analysis, model building and coming up with budget
## for marketing

## Following function will be used to perform feature engineering in each sub category

## Feature Engineering function
Prod_Feature_Engg <- function(prod_df, filename){
    
    ## Promotion or Discount factor
    #
    # Calculate list price of products
    prod_df$list_price <- prod_df$gmv/prod_df$units
    # Calculate discount percentage
    prod_df$discount_per <- (prod_df$mrp - prod_df$list_price) / prod_df$mrp

    
    ## Brand Perception
    #
    ## Based on the price of product, bucketing can be done as low-price (mass market), mid-price 
    ## (mid market) and high-price (premium market) products
    ## Product verticals will be considered in each sub category. Based on the units, mrp and list
    ## price, product verticals will be clustered into 3 groups. 
    ## If for any sub category, we have less than 3 verticals, then only mass market and mid market
    ## clustering will be done
    ## K-Means Clustering will be used for bucketing
    #
    # Create a df with product verticals and mean of units, mrp and list price for each vertical
    prod_clus <- aggregate(cbind(units, mrp, list_price) ~ product_analytic_vertical, prod_df, mean)

    # Based on no of verticals, cluster the products
    prod_vertical_no <- length(prod_clus$product_analytic_vertical)
    if(prod_vertical_no > 2) {
        # Scale numericals columns before clustering
        prod_clus$units      <- scale(prod_clus$units)
        prod_clus$mrp        <- scale(prod_clus$mrp)
        prod_clus$list_price <- scale(prod_clus$list_price)
        
        # Cluster using k-means clustering
        category_cluster <- kmeans(prod_clus[,-1], centers = 3, iter.max = 50, nstart = 50)
        prod_clus$cluster <- as.factor(category_cluster$cluster)

        # Categorise based on clusters formed
        prod_df <- merge(prod_df, prod_clus[,c("product_analytic_vertical","cluster")], 
                         by="product_analytic_vertical", all.x=TRUE)

        tmpdf <- as.data.frame(table(prod_df$cluster))
        tmpdf$market_type <- NA
        tmpdf$market_type[which(tmpdf$Freq == max(tmpdf$Freq))] <- "mass_market"
        tmpdf$market_type[which(tmpdf$Freq == min(tmpdf$Freq))] <- "premium_market"
        tmpdf$market_type[which(is.na(tmpdf$market_type))] <- "aspiring_market"
        colnames(tmpdf)[1]  <- "cluster"

        prod_df <- merge(prod_df, tmpdf[,c("cluster","market_type")], by="cluster")
        
    } else {
        # Based on the mean of mrp, group into mass and aspiring market
        prod_clus$market_type <- NA
        prod_clus$market_type[which(prod_clus$mrp < mean(prod_clus$mrp))] <- "mass_market"
        prod_clus$market_type[which(is.na(prod_clus$market_type))] <- "aspiring_market"
        
        # Merge with dataset
        prod_df <- merge(prod_df, prod_clus, by="product_analytic_vertical")
    }
    
    ## Save Market Type info for each product vertical
    tmpdf <- as.data.frame(unique(prod_df[,c("product_analytic_vertical","market_type")]))
    write.csv(tmpdf, filename, row.names = FALSE)
    
    
    ## Payment Type factor
    #
    ## Derive variable to identify prepaid orders
    prod_df$prepaid_order <- ifelse(prod_df$order_payment_type=="Prepaid",1,0)
    
 
       
    ## Percentage of Prepaid Order
    #
    ## Derive % of prepaid order to understand the effects on overall orders
    
    # Find total no of orders in each week
    weekly_total_order <- aggregate(order_id ~ WeekNo, prod_df, NROW)
    # Find total no of prepaid orders in each week
    weekly_prepaid_order <- aggregate(prepaid_order ~ WeekNo, prod_df, sum)
    
    # Merge the results and find prepaid order % per week
    weekly_orders <- merge(weekly_total_order, weekly_prepaid_order, by="WeekNo", all.x=TRUE)
    weekly_orders$prepaid_per <- weekly_orders$prepaid_order/weekly_orders$order_id
    
    # Merge results with main dataset
    prod_df <- merge(prod_df, weekly_orders[,c("WeekNo","prepaid_per")],by="WeekNo", all.x=TRUE)

    
    
    ## Delivery time
    #
    prod_df$delivery_time <- prod_df$sla - (prod_df$deliverybdays + prod_df$deliverycdays + 
                                            prod_df$product_procurement_sla)
    prod_df$delivery_status[prod_df$delivery_time < 0] <- "Delayed"
    prod_df$delivery_status[prod_df$delivery_time == 0] <- "On_time"
    prod_df$delivery_status[prod_df$delivery_time > 0] <- "Early"
    

    
    ## Consolidate dataframe on weekly basis
    #
    # count of products as per market_type
    weekly_prodno <- as.data.frame.matrix(t(table(prod_df$market_type,prod_df$WeekNo)))
    weekly_prodno$WeekNo <- row.names(weekly_prodno)

    # weekly sum of variables 
    weekly_sum <- aggregate(cbind(gmv,units,mrp,
                                  (ifelse (delivery_status =='Delayed',1,0)),
                                  (ifelse (delivery_status =='Early',1,0)),
                                  (ifelse (delivery_status =='On_time',1,0)))~WeekNo, prod_df, sum)
    colnames(weekly_sum)[-1] <- c("gmv","total_units","total_mrp",
                                      "delayed_delivery_count","early_delivery_count",
                                      "ontime_delivery_count")
 
    # weekly mean of  variables
    prod_df <- aggregate(cbind(units,sla,mrp,product_procurement_sla, total_investment,
                                   NPS, list_price, discount_per, prepaid_per)~WeekNo, 
                             prod_df, mean)
    colnames(prod_df)[c(2:4)] <- c("avg_units","sla","avg_mrp")
  
    # merge all datasets
    prod_df <- Reduce(function(x,y) merge(x,y, by="WeekNo", all.x=TRUE), 
                          list(prod_df,weekly_prodno, weekly_sum))
    
    
    
    ## Adstock
    #
    # Merge Adstock data to result dataframe
    prod_df <- merge(prod_df, Adstock, by="WeekNo")
    
    
    
    ## Holidays / Special Days
    #
    prod_df <- merge(prod_df, Holiday_Data, by="WeekNo", all.x=TRUE)
    prod_df$spl_day[is.na(prod_df$spl_day)] <-0
    
    
    
    ## Moving Averages
    #
    # Generate moving averages to analyse the list price and discount percentage inflation for upto
    # 3 weeks
    # Function to compute rolling means
    fun_rollmean_k1 = function(x) rollmean(x, k = 2, fill = NA, align = "right")
    fun_rollmean_k2 = function(x) rollmean(x, k = 3, fill = NA, align = "right")
    fun_rollmean_k3 = function(x) rollmean(x, k = 4, fill = NA, align = "right")

    # Calculate rolling means for list_price and discount_per
    tmpdf <- prod_df[,c("WeekNo","list_price","discount_per")]
    tmpdf1 <- mutate_each(tmpdf, funs(fun_rollmean_k1), list_price, discount_per)
    tmpdf2 <- mutate_each(tmpdf, funs(fun_rollmean_k2), list_price, discount_per)
    tmpdf3 <- mutate_each(tmpdf, funs(fun_rollmean_k3), list_price, discount_per)

    colnames(tmpdf1) <- c("WeekNo","MA_LP1","MA_DP1")
    colnames(tmpdf2) <- c("WeekNo","MA_LP2","MA_DP2")
    colnames(tmpdf3) <- c("WeekNo","MA_LP3","MA_DP3")

    tmpdf <- Reduce(function(x,y) merge(x,y, by="WeekNo"), list(tmpdf,tmpdf1,tmpdf2,tmpdf3))
    
    tmpdf$inc_MA_LP1<-(tmpdf$list_price - tmpdf$MA_LP1)/tmpdf$MA_LP1
    tmpdf$inc_MA_LP2<-(tmpdf$list_price - tmpdf$MA_LP2)/tmpdf$MA_LP2
    tmpdf$inc_MA_LP3<-(tmpdf$list_price - tmpdf$MA_LP3)/tmpdf$MA_LP3

    tmpdf$inc_MA_DP1<-(tmpdf$discount_per - tmpdf$MA_DP1)/tmpdf$MA_DP1
    tmpdf$inc_MA_DP2<-(tmpdf$discount_per - tmpdf$MA_DP2)/tmpdf$MA_DP2
    tmpdf$inc_MA_DP3<-(tmpdf$discount_per - tmpdf$MA_DP3)/tmpdf$MA_DP3

    prod_df <- merge(prod_df,tmpdf[,c(1,10:15)],by="WeekNo")
    rm(tmpdf)
    
    
    ## Lag of list_price and discounts
    #
    # Lag of List price by week 1 to week 3
    tmpdf <- slide(prod_df[,c("WeekNo","list_price","discount_per","spl_day")], 
                   Var="list_price", slideBy = -1)
    tmpdf <- slide(tmpdf, Var="list_price", slideBy = -2)
    tmpdf <- slide(tmpdf, Var="list_price", slideBy = -3)
    # Lag of discount variables by week 1 to week 3
    tmpdf <- slide(tmpdf, Var = "discount_per", slideBy = -1)
    tmpdf <- slide(tmpdf, Var = "discount_per", slideBy = -2)
    tmpdf <- slide(tmpdf, Var = "discount_per", slideBy = -3)
    # Lag of spl_day variables by week 1 to week 3
    tmpdf <- slide(tmpdf, Var = "spl_day", slideBy = -1)
    tmpdf <- slide(tmpdf, Var = "spl_day", slideBy = -2)
    tmpdf <- slide(tmpdf, Var = "spl_day", slideBy = -3)
    
    
    # Incremental Lag of List price & discounts by week 1 to week 3
    tmpdf$Lag_LP1_per <- (tmpdf$list_price - tmpdf$`list_price-1`) / tmpdf$`list_price-1`
    tmpdf$Lag_LP2_per <- (tmpdf$list_price - tmpdf$`list_price-2`) / tmpdf$`list_price-2`
    tmpdf$Lag_LP3_per <- (tmpdf$list_price - tmpdf$`list_price-3`) / tmpdf$`list_price-3`

    tmpdf$Lag_DP1_per <- (tmpdf$discount_per - tmpdf$`discount_per-1`) / tmpdf$`discount_per-1`
    tmpdf$Lag_DP2_per <- (tmpdf$discount_per - tmpdf$`discount_per-2`) / tmpdf$`discount_per-2`
    tmpdf$Lag_DP3_per <- (tmpdf$discount_per - tmpdf$`discount_per-3`) / tmpdf$`discount_per-3`
   
    prod_df <- na.omit(merge(prod_df,tmpdf[,c(1,11:19)],by="WeekNo"))

    colnames(prod_df)
    colnames(prod_df)[34:36] <- c("Lag_spl_day_1","Lag_spl_day_2","Lag_spl_day_3")
    
    return(prod_df) 
}

# Feature Engineering for each product sub category
CE_CamAcc_Final <- Prod_Feature_Engg(CE_CamAcc, "CamAcc_Market_Type.csv")
CE_GamAcc_Final <- Prod_Feature_Engg(CE_GamAcc, "GamAcc_Market_Type.csv")
CE_HomAud_Final <- Prod_Feature_Engg(CE_HomAud, "HomAud_Market_Type.csv")


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- EXPLORATORY DATA ANALYSIS----------------------------------##
## -----------------------------------------------------------------------------------------------##

## For each sub category - Gaming Accessory, Camera Accessory and Home Audio, EDA will be done on
## orginal variables and derived variables to understand the important factors affecting the sales
## based on marketing done.

## Following function will be used to perform EDA in each sub category
## Feature Engineering function
Prod_EDA <- function(prod_df,title) {
    grid.layout(ncol=5)

    ## Adstock Vs GMV
    # TV Adstock VS GMV
    plot1 <- ggplot(prod_df,aes(TV,gmv)) + geom_point() + geom_smooth() + 
             ggtitle(title) + labs(x ="TV AdStock", y="GMV")

    # Digital Adstock VS GMV
    plot2 <- ggplot(prod_df,aes(Digital,gmv)) + geom_point() + geom_smooth() +
             ggtitle(title) + labs(x="Digital AdStock", y="GMV")

    # Sponsorship AdStock VS GMV
    plot3 <-  ggplot(prod_df,aes(Sponsorship,gmv)) + geom_point() + geom_smooth() +
              ggtitle(title) + labs(x = "Sponsorship AdStock", y = "GMV")

    # Content Marketing AdStock VS GMV
    plot4 <-  ggplot(prod_df,aes(Content.Marketing,gmv)) + geom_point() +  geom_smooth() +
              ggtitle(title) + labs(x = "Content Marketing AdStock", y = "GMV")
  
    # Online Marketing AdStock VS GMV
    plot5 <-  ggplot(prod_df,aes(Online_marketing,gmv)) + geom_point() + geom_smooth() +
              ggtitle(title) + labs(x = "Online Marketing AdStock", y = "GMV")
  
    # Affiliates AdStock VS GMV
    plot6 <-  ggplot(prod_df,aes(Affiliates,gmv)) + geom_point() + geom_smooth() +
              ggtitle(title) + labs(x = "Affiliates AdStock", y = "GMV")
  
    # SEM AdStock VS GMV
    plot7 <-  ggplot(prod_df,aes(SEM,gmv)) + geom_point() + geom_smooth() +
              ggtitle(title) + labs(x = "SEM AdStock", y = "GMV")
  
    
    ## Special Day VS GMV
    plot8 <-  ggplot(prod_df,aes(WeekNo,gmv, fill = as.factor(ifelse(spl_day > 0,1,0)))) +
              geom_bar(stat="identity") + ggtitle(title) +
              labs(fill = "Special Days", x = "Week", y = "GMV")
  

    ## Avg Units VS GMV
    plot9 <-  ggplot(prod_df,aes(avg_units,gmv)) + geom_point() + geom_smooth() +
              ggtitle(title) + labs(x = "Units", y = "GMV")

    
    ## NPS VS GMV
    plot10 <-  ggplot(prod_df,aes(NPS,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "NPS", y = "GMV")

      
    ## Premium Market VS GMV
    plot11 <-  ggplot(prod_df,aes(premium_market,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "Premium Market", y = "GMV")

      
    ## aspiring_market VS GMV
    plot12 <-  ggplot(prod_df,aes(aspiring_market,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "Aspiring Market", y = "GMV")

    
    ## mass_market VS GMV
    plot13 <-  ggplot(prod_df,aes(mass_market,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "Mass Market", y = "GMV")

        
    ## avg_mrp VS GMV
    plot14 <-  ggplot(prod_df,aes(avg_mrp,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "Avg MRP", y = "GMV")
 
     
    ## discount_per VS GMV
    plot15 <-  ggplot(prod_df,aes(discount_per,gmv)) + geom_point() + geom_smooth() +
               ggtitle(title) + labs(x = "Discount %", y = "GMV")

      
    ## product_procurement_sla VS GMV
    plot16 <-  ggplot(prod_df,aes(product_procurement_sla,gmv)) + geom_point() + 
                geom_smooth() +
               ggtitle(title) + labs(x = "Product Procurement SLA", y = "GMV")
  
  
    ## Plot grid of all independent variables vs GMV
    grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8,
                 plot9, plot10, plot11, plot12, plot13, plot14, plot15, plot16)
  
}

## EDA for each sub category

# Turn Off warnings
older_warn_option <- getOption("warn")
options(warn = -1)

Prod_EDA(CE_CamAcc_Final,"Camera Accessory")
Prod_EDA(CE_GamAcc_Final,"Gaming Accessory")
Prod_EDA(CE_HomAud_Final,"Home Audio")

# Reset warnings options to as before
options(warn = older_warn_option)



## Comparative analysis
## GMV for each category across weeks.
plot_CamAcc_gmv_trend <- ggplot(CE_CamAcc_Final, aes(x = WeekNo, y = gmv)) + geom_line() + 
                          ggtitle("Camera Accessory Sales")
plot_GamAcc_gmv_trend <- ggplot(CE_GamAcc_Final, aes(x = WeekNo, y = gmv)) + geom_line() +
                          ggtitle("Gaming Accessory Sales")
plot_HomAud_gmv_trend <- ggplot(CE_HomAud_Final, aes(x = WeekNo, y = gmv)) + geom_line() +
                          ggtitle("Home Audio Sales")
plot_spldays          <- ggplot(Holiday_Data, aes(x = WeekNo, y = spl_day)) + geom_col() +
                          ggtitle("Holidays per week")

grid.arrange(plot_GamAcc_gmv_trend, plot_CamAcc_gmv_trend, plot_HomAud_gmv_trend, plot_spldays)


### EDA Conclusions
##
## - Home Audio is most expensive product bringing in maximum revenue amongst 3 sub categories.
## - Camera Accessory have maximum no of units sold followed by Gaming Accessory
## - A large portion of sales are with cash on delivery payment type. The Cash on Delivery mode 
##   should be made more convenient to enhance sales. But introspection on the Prepaid mode is also
##   needed to plan for better and smooth processes to increase sales as it is beneficial to the 
##   company from both operational and financial p.o.v.
## - The increase in GMV is observed for most investment channels, but only up to a certain limit.
##   After that, the increase in spend is not impacting GMV to a high degree.
## - We also notice that there could be a carry-over effect on certain marketing channels where the
##   increase in GMV is noted even when there is no substantial investment.
## - During holiday season, there are peaks in both sales and items sold across all product 
##   categories.
## - Heavy discount is seen during holiday season but discounts over 50% is not observed to be 
##   effective in increasing the sales.

####################################################################################################
