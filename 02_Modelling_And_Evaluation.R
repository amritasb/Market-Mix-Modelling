####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------------------- MODEL BUILDING --------------------------------------------##
## -----------------------------------------------------------------------------------------------##

####################################################################################################

## To build mix marketing model, following approach will be used
##
## 1. Co-relation analysis on dataset
##      Dataset derived after data understandingm preparation, EDA and feature engineering is
##      analysed for co-relation and variables which are highly co-related are eliminated in each
##      sub category dataset
##
## 2. 5 types of models will be built for each sub category :
##      A) Linear Model 
##          - all variables will be considered
##      B) Multiplicative Model 
##          - original variables along with derived variables will be considered
##      B) Koyck Model 
##          - original variables, derived variables and moving average variables are considered
##      D) Distributed Lag Model 
##          - all variables are considered, along with new lag variables created
##      E) Multiplicative + Distributed Lag Model 
##          - all variables are considered, along with new lag variables created
##
## 3. Cross validation with 10 fold will be performed to evaluate the models
## 
## 4. Elasticity of the models will be calculated
##
## 5. Results of modelling will be tabulated and saved to a csv file for 
##    analysis and best model selection
##
## Additional points
## - Functions have been created for common modelling steps as given in this file
## - Final variables selected in each modelling exercise is a result of iterative modelling exercise
##   with selection based on modelling results and how variables can affect the marketing budget

####################################################################################################

## -----------------------------------------------------------------------------------------------##
## Co-relation analysis
## -----------------------------------------------------------------------------------------------##

## Final dataset created for each sub category will have to be furthur analysed from co-relation
## p.o.v to eliminate variables which are highly corelated.
## In order to analyse the co-relation matrix, csv files were created and manually analysed to
## finalize the list of variables being removed from final dataset in each subcategory

## Function to analyse co-relation
Analyse_corr <- function(prod_df,filename){
    corr_matrix <- cor(prod_df[-1])
    #heatmap(corr_matrix,symm=TRUE)
    write.csv(as.data.frame(corr_matrix),filename)
}

# Analyse corelation matrix
Analyse_corr(CE_CamAcc_Final,"corr_matrix_camacc.csv")
Analyse_corr(CE_GamAcc_Final,"corr_matrix_gamacc.csv")
Analyse_corr(CE_HomAud_Final,"corr_matrix_homaud.csv")


## On analysis of "corr_matrix_camacc.csv", "corr_matrix_gamacc.csv", "corr_matrix_homdud.csv",
## following variables will be eliminated as in the function below
Eliminate_vars <- function(prod_df) {
    prod_df$mass_market <- NULL
    prod_df$total_units <- NULL
    prod_df$total_mrp <- NULL
    prod_df$delayed_delivery_count <- NULL
    prod_df$early_delivery_count <- NULL
    prod_df$ontime_delivery_count <- NULL
    prod_df$Content.Marketing <- NULL
    prod_df$Affiliates <- NULL
    prod_df$SEM <- NULL
    prod_df$inc_MA_LP2 <- NULL
    prod_df$inc_MA_LP3 <- NULL
    prod_df$inc_MA_DP2 <- NULL
    prod_df$inc_MA_DP3 <- NULL
    prod_df$Lag_LP1_per <- NULL
    prod_df$Lag_DP1_per <- NULL
    
    return(prod_df)
}
# Eliminate co-related variables
CE_CamAcc_Final <- Eliminate_vars(CE_CamAcc_Final)
CE_GamAcc_Final <- Eliminate_vars(CE_GamAcc_Final)
CE_HomAud_Final <- Eliminate_vars(CE_HomAud_Final)

# Analyse co-relation on final datasets
Analyse_corr(CE_CamAcc_Final,"corr_matrix_camacc_final.csv")
Analyse_corr(CE_GamAcc_Final,"corr_matrix_gamacc_final.csv")
Analyse_corr(CE_HomAud_Final,"corr_matrix_homaud_final.csv")

####################################################################################################

## -----------------------------------------------------------------------------------------------##
## Common model analysis functions
## -----------------------------------------------------------------------------------------------##

##
## Linear Model data prep
## Function for linear model data preparation
LM_Data_Prep <- function(prod_df) {
    LM_Data <- prod_df[,-1]
    colnames(LM_Data)
    
    # Scale the data
    LM_Data <- as.data.frame(scale(LM_Data))
    
    # Delete older models if any
    rm(list = ls(pattern = "^Model_[0-9]"))
    rm(LM_Model_Final, LM_MSE, LM_Elasticity)
    
    return(LM_Data)
}


##
## Multiplicative Model data prep
## Function for multiplicative model data preparation
MM_Data_Prep <- function(prod_df) {
    MM_Data <- prod_df[,c(2:18)]
    colnames(MM_Data)
    
    # Treatment of zero values
    summary(MM_Data)
    sapply(MM_Data, function(x){min(x)})
    
    MM_Data$prepaid_per[which(MM_Data$prepaid_per == 0)] <- 0.01
    MM_Data$aspiring_market[which(MM_Data$aspiring_market == 0)] <- 0.01
    MM_Data$premium_market[which(MM_Data$premium_market == 0)] <- 0.01
    MM_Data$spl_day[which(MM_Data$spl_day == 0)] <- 0.01
    
    # Log of variables
    MM_Data <- data.frame(sign(MM_Data)*log(abs(MM_Data)))
    
    # Delete older models if any
    rm(list = ls(pattern = "^Model_[0-9]"))
    rm(MM_Model_Final, MM_MSE, MM_Elasticity)
  
    return(MM_Data)
}


##
## Koyck Model data prep
## Function for koyck model data preparation
KM_Data_Prep <- function(prod_df) {
    KM_Data <- prod_df[,c(2:20)]
    colnames(KM_Data)
    
    # Lag dependent variable "gmv" by 1
    KM_Data <- slide(KM_Data, Var = "gmv", slideBy = -1)
    
    # Scale the data
    KM_Data <- as.data.frame(scale(na.omit(KM_Data)))
    
    # Delete older models if any
    rm(list = ls(pattern = "^Model_[0-9]"))
    rm(KM_Model_Final, KM_MSE, KM_Elasticity)
    
    return(KM_Data)
}


##
## Distributed Lag Model data prep
## Function for distributed lag model data preparation
DLM_Data_Prep <- function(prod_df) {
    DLM_Data <- prod_df[,-1]
    colnames(DLM_Data)
    
    # Lag dependent variable gmv by 1,2,3
    DLM_Data <- slide(DLM_Data, Var = "gmv", slideBy = -1)
    DLM_Data <- slide(DLM_Data, Var = "gmv", slideBy = -2)
    DLM_Data <- slide(DLM_Data, Var = "gmv", slideBy = -3)
    
    # Additional variable lagging - Lag NPS variable
    DLM_Data <- slide(DLM_Data, Var = "NPS", slideBy = -1)
    DLM_Data <- slide(DLM_Data, Var = "NPS", slideBy = -2)
    DLM_Data <- slide(DLM_Data, Var = "NPS", slideBy = -3)
  
    # Scale the data
    DLM_Data <- as.data.frame(scale(na.omit(DLM_Data)))
    
    # Delete older models if any
    rm(list = ls(pattern = "^Model_[0-9]"))
    rm(DLM_Model_Final, DLM_MSE, DLM_Elasticity)
    
    return(DLM_Data)
}


##
## Multiplicative + Distributed Lag Model data prep
## Function for multiplicative + distributed lag model data preparation
MDLM_Data_Prep <- function(prod_df) {
    MDLM_Data <- prod_df[,-1]
    colnames(MDLM_Data)
    
    # Lag dependent variable gmv by 1,2,3
    MDLM_Data <- slide(MDLM_Data, Var = "gmv", slideBy = -1)
    MDLM_Data <- slide(MDLM_Data, Var = "gmv", slideBy = -2)
    MDLM_Data <- slide(MDLM_Data, Var = "gmv", slideBy = -3)
    
    # Additional variable lagging - Lag NPS variable
    MDLM_Data <- slide(MDLM_Data, Var = "NPS", slideBy = -1)
    MDLM_Data <- slide(MDLM_Data, Var = "NPS", slideBy = -2)
    MDLM_Data <- slide(MDLM_Data, Var = "NPS", slideBy = -3)
    
    # Omit NA
    MDLM_Data <- na.omit(MDLM_Data)
    
    # Treatment of zero values
    summary(MDLM_Data)
    sapply(MDLM_Data, function(x){min(x)})
    
    MDLM_Data$prepaid_per[which(MDLM_Data$prepaid_per == 0)] <- 0.01
    MDLM_Data$aspiring_market[which(MDLM_Data$aspiring_market == 0)] <- 0.01
    MDLM_Data$premium_market[which(MDLM_Data$premium_market == 0)] <- 0.01
    MDLM_Data$spl_day[which(MDLM_Data$spl_day == 0)] <- 0.01
    MDLM_Data$Lag_spl_day_1[which(MDLM_Data$Lag_spl_day_1 == 0)] <- 0.01
    MDLM_Data$Lag_spl_day_2[which(MDLM_Data$Lag_spl_day_2 == 0)] <- 0.01
    MDLM_Data$Lag_spl_day_3[which(MDLM_Data$Lag_spl_day_3 == 0)] <- 0.01
    
  
    # Log of variables
    MDLM_Data <- data.frame(sign(MDLM_Data)*log(abs(MDLM_Data)))
  
    # Delete older models if any
    rm(list = ls(pattern = "^Model_[0-9]"))
    rm(MDLM_Model_Final, MDLM_MSE, MDLM_Elasticity)
    
    return(MDLM_Data)
}


##
## Cross validation with 10 fold
## Function to perform 10 fold cross validation
Model_CrossValidate <- function(Model_Data, cv_formula) {
    Model_CV <- cv.lm(data = Model_Data, m = 10, form.lm = formula(cv_formula))
    
    # Cross validation mean square error
    MSE <- attributes(Model_CV)$ms
    
    return(MSE)
}


##
## Elasticity Analysis
## Function to analyse elasticity
Elasticity_Analysis <- function(prod_df, model, title) {
    var_list   <- list()
    coff_len   <- length(model$coefficients)
    
    # Function to calculate elasticity
    Calc_Elasticity <- function(var) {
        x <- as.numeric(model$coefficients[var]*mean(prod_df[,var])/mean(prod_df$gmv))
        return(x)
    }
    
    # Calculate elasticity
    for(i in 2:coff_len) {
        var_list[i-1] <- Calc_Elasticity(names(model$coefficients)[i])
    }
    
    elas_outputs <- data.frame(names(model$coefficients[2:coff_len]))
    elas_outputs <- cbind(elas_outputs, do.call(rbind.data.frame, var_list))
    colnames(elas_outputs) <- c("Variable","Elasticity")
    
    elas_outputs$direction <- ifelse(elas_outputs$Elasticity > 0, "Positive", "Negative")
    
    # Plot elasticity for Camera Accesory - Linear Model
    elas_plot <- ggplot(elas_outputs, aes(x = reorder(Variable,Elasticity), y=Elasticity)) + 
      geom_bar(position="dodge",stat="identity") + coord_flip() + 
      ggtitle(title) + xlab("Variables")
    # Print the Plot
    print(elas_plot)
    
    # Return elasticity info
    return(elas_outputs)
}

####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ------------------------------- MODEL BUILDING - LINEAR MODEL----------------------------------##
## -----------------------------------------------------------------------------------------------##

## Delete old data if any
rm(list = ls(pattern = "*_Linear_Model"))

## -----------------------------------------------------------------------------------------------##
## LINEAR MODEL - CAMERA ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for linear model
LM_Data <- LM_Data_Prep(CE_CamAcc_Final)
View(LM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = LM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.8412,	Adjusted R-squared:  0.6686 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.8149,	Adjusted R-squared:  0.7462 


# Model 3 - remove spl_day
Model_03 <- lm(formula = gmv ~ sla + avg_mrp + total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1 + 
                 Lag_spl_day_1 + Lag_LP2_per + Lag_DP2_per + Lag_DP3_per, 
               data = LM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove Lag_DP3_per
Model_04 <- lm(formula = gmv ~ sla + avg_mrp + total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1 + 
                 Lag_spl_day_1 + Lag_LP2_per + Lag_DP2_per, 
               data = LM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove sla
Model_05 <- lm(formula = gmv ~ avg_mrp + total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1 + 
                 Lag_spl_day_1 + Lag_LP2_per + Lag_DP2_per, data = LM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove Lag_spl_day_1
Model_06 <- lm(formula = gmv ~  avg_mrp + total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1 + 
                 Lag_LP2_per + Lag_DP2_per, data = LM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove avg_mrp
Model_07 <- lm(formula = gmv ~ total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1 + 
                 Lag_LP2_per + Lag_DP2_per, data = LM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove Lag_DP2_per, Lag_LP2_per
# Two variables are removed after multiple reruns of Model 8 and final Model 8 is provided below
Model_08 <- lm(formula = gmv ~ total_investment + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1, data = LM_Data)
summary(Model_08)
vif(Model_08)
# Model 8 has all significant variables
# Multiple R-squared:  0.7102,	Adjusted R-squared:  0.6687 

# Model 8 was rerun by removing each significant variable and checking the effect on R-squared
# Remove total_investment - Multiple R-squared:  0.6114,	Adjusted R-squared:  0.5662 
# Remove discount_per     - Multiple R-squared:  0.5786,	Adjusted R-squared:  0.529 
# Remove aspiring_market  - Multiple R-squared:  0.6054,	Adjusted R-squared:  0.5595 
# Remove Sponsorship      - Multiple R-squared:  0.2515,	Adjusted R-squared:  0.1834 
# Remove inc_MA_LP1       - Multiple R-squared:  0.496,	  Adjusted R-squared:  0.4501 
# Remove inc_MA_DP1       - Multiple R-squared:  0.4101,	Adjusted R-squared:  0.3564 
# Based on the above results and summary of each model run above, Model 8 is accepted as final model



## Final model
LM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
LM_MSE <- Model_CrossValidate(LM_Data, as.character(LM_Model_Final$call)[2])

## Elasticity Analysis
LM_Elasticity <- Elasticity_Analysis(LM_Data,LM_Model_Final,"Camera Accessory - Linear Model")

## Results
CamAcc_Linear_Model <- LM_Model_Final
CamAcc_Linear_Model_MSE <- LM_MSE
CamAcc_Linear_Model_Elasticity <- LM_Elasticity


## -----------------------------------------------------------------------------------------------##
## LINEAR MODEL - GAMING ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for linear model
LM_Data <- LM_Data_Prep(CE_GamAcc_Final)
View(LM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = LM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.955,	Adjusted R-squared:  0.907 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.951,	Adjusted R-squared:  0.932 


# Model 3 - remove avg_units
Model_03 <- lm(formula = gmv ~ sla + avg_mrp + product_procurement_sla + 
                 total_investment + discount_per + prepaid_per + aspiring_market + 
                 premium_market + Sponsorship + inc_MA_DP1 + Lag_LP2_per + 
                 Lag_DP2_per + Lag_DP3_per, data = LM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove product_procurement_sla
Model_04 <- lm(formula = gmv ~ sla + avg_mrp +
                 total_investment + discount_per + prepaid_per + aspiring_market + 
                 premium_market + Sponsorship + inc_MA_DP1 + Lag_LP2_per + 
                 Lag_DP2_per + Lag_DP3_per, data = LM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove avg_mrp
Model_05 <- lm(formula = gmv ~ sla + total_investment + discount_per + 
                 prepaid_per + aspiring_market + premium_market + Sponsorship + 
                 inc_MA_DP1 + Lag_LP2_per + Lag_DP2_per + Lag_DP3_per, data = LM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove prepaid_per
Model_06 <- lm(formula = gmv ~ sla + total_investment + discount_per + 
                 aspiring_market + premium_market + Sponsorship + 
                 inc_MA_DP1 + Lag_LP2_per + Lag_DP2_per + Lag_DP3_per, data = LM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove Lag_DP3_per, Lag_DP2_per, Lag_LP2_per
Model_07 <- lm(formula = gmv ~ sla + total_investment + discount_per + 
                 aspiring_market + premium_market + Sponsorship + 
                 inc_MA_DP1, data = LM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove total_investment
Model_08 <- lm(formula = gmv ~ sla + discount_per + 
                 aspiring_market + premium_market + Sponsorship + 
                 inc_MA_DP1, data = LM_Data)
summary(Model_08)
vif(Model_08)


# Model 9 - remove Sponsorship
Model_09 <- lm(formula = gmv ~ sla + discount_per + aspiring_market + premium_market + 
                 inc_MA_DP1, data = LM_Data)
summary(Model_09)
vif(Model_09)


# Model 10 - remove sla
Model_10 <- lm(formula = gmv ~ discount_per + aspiring_market + premium_market + 
                 inc_MA_DP1, data = LM_Data)
summary(Model_10)
vif(Model_10)
# Model 10 has all significant variables
# Multiple R-squared:  0.854,	Adjusted R-squared:  0.84 


# Model 10 was rerun by removing each significant variable and checking the effect on R-squared
# Based on the rerun results & summary of each model run, Model 10 is accepted as final model


## Final model
LM_Model_Final <- Model_10

## Cross validation with 10 fold and MSE calculation
LM_MSE <- Model_CrossValidate(LM_Data, as.character(LM_Model_Final$call)[2])

## Elasticity Analysis
LM_Elasticity <- Elasticity_Analysis(LM_Data,LM_Model_Final,"Gaming Accessory - Linear Model")

## Results
GamAcc_Linear_Model <- LM_Model_Final
GamAcc_Linear_Model_MSE <- LM_MSE
GamAcc_Linear_Model_Elasticity <- LM_Elasticity



## -----------------------------------------------------------------------------------------------##
## LINEAR MODEL - HOME AUDIO
## -----------------------------------------------------------------------------------------------##

## Data Preparation for linear model
LM_Data <- LM_Data_Prep(CE_HomAud_Final)
View(LM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = LM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.825,	Adjusted R-squared:  0.627 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.778,	Adjusted R-squared:  0.702 


# Model 3 - remove Sponsorship
Model_03 <- lm(formula = gmv ~ avg_units + avg_mrp + product_procurement_sla + 
                 total_investment + NPS + list_price + discount_per + aspiring_market + 
                 premium_market + TV + Digital, data = LM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove product_procurement_sla
Model_04 <- lm(formula = gmv ~ avg_units + avg_mrp + 
                 total_investment + NPS + list_price + discount_per + aspiring_market + 
                 premium_market + TV + Digital, data = LM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove total_investment
Model_05 <- lm(formula = gmv ~ avg_units + avg_mrp + 
                 NPS + list_price + discount_per + aspiring_market + 
                 premium_market + TV + Digital, data = LM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove NPS, avg_units
Model_06 <- lm(formula = gmv ~ avg_mrp + list_price + discount_per + aspiring_market + 
                 premium_market + TV + Digital, data = LM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove premium_market, TV, discount_per
Model_07 <- lm(formula = gmv ~ avg_mrp + list_price + aspiring_market + Digital, data = LM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove list_price
Model_08 <- lm(formula = gmv ~ avg_mrp + aspiring_market + Digital, data = LM_Data)
summary(Model_08)
vif(Model_08)
# Model 8 has all significant variables
# Multiple R-squared:  0.589,	Adjusted R-squared:  0.561

# Model 8 was rerun by removing each significant variable and checking the effect on R-squared
# Based on the rerun results & summary of each model run, Model 8 is accepted as final model


## Final model
LM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
LM_MSE <- Model_CrossValidate(LM_Data, as.character(LM_Model_Final$call)[2])

## Elasticity Analysis
LM_Elasticity <- Elasticity_Analysis(LM_Data,LM_Model_Final,"Home Audio - Linear Model")

## Results
HomAud_Linear_Model <- LM_Model_Final
HomAud_Linear_Model_MSE <- LM_MSE
HomAud_Linear_Model_Elasticity <- LM_Elasticity


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ------------------------------- MODEL BUILDING - MULTIPLICATIVE MODEL--------------------------##
## -----------------------------------------------------------------------------------------------##

## Delete old data if any
rm(list = ls(pattern = "*_Multiplicative_Model"))

## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE MODEL - CAMERA ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for multiplicative model
MM_Data <- MM_Data_Prep(CE_CamAcc_Final)
View(MM_Data)


## Model building

# Model 01
Model_01 <- lm(gmv~. , data = MM_Data)
summary(Model_01)
# Multiple R-squared:  0.932,	Adjusted R-squared:  0.898 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.929,	Adjusted R-squared:  0.915 


# Model 3 - remove sla
Model_03 <- lm(formula = gmv ~ avg_units + avg_mrp + list_price + discount_per + 
                 prepaid_per + aspiring_market + Online_marketing, data = MM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove avg_mrp
Model_04 <- lm(formula = gmv ~ avg_units + list_price + discount_per + 
                 prepaid_per + aspiring_market + Online_marketing, data = MM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove discount_per
Model_05 <- lm(formula = gmv ~ avg_units + list_price +
                 prepaid_per + aspiring_market + Online_marketing, data = MM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove Online_marketing
Model_06 <- lm(formula = gmv ~ avg_units + list_price +
                 prepaid_per + aspiring_market, data = MM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove list_price
Model_07 <- lm(formula = gmv ~ avg_units + prepaid_per + aspiring_market, data = MM_Data)
summary(Model_07)
vif(Model_07)
# Multiple R-squared:  0.889,	Adjusted R-squared:  0.882 


## Final model
MM_Model_Final <- Model_07



## Cross validation with 10 fold and MSE calculation
MM_MSE <- Model_CrossValidate(MM_Data, as.character(MM_Model_Final$call)[2])


## Elasticity Analysis
MM_Elasticity <- Elasticity_Analysis(MM_Data,MM_Model_Final,
                                     "Camera Accessory - Multiplicative Model")


## Results
CamAcc_Multiplicative_Model <- MM_Model_Final
CamAcc_Multiplicative_Model_MSE <- MM_MSE
CamAcc_Multiplicative_Model_Elasticity <- MM_Elasticity


## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE MODEL - GAMING ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for multiplicative model
MM_Data <- MM_Data_Prep(CE_GamAcc_Final)
View(MM_Data)

## Model building

# Model 01
Model_01 <- lm(gmv~. , data = MM_Data)
summary(Model_01)
# Multiple R-squared:  0.993,	Adjusted R-squared:  0.989 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.992,	Adjusted R-squared:  0.99 


# Model 3 - remove product_procurement_sla
Model_03 <- lm(formula = gmv ~ sla + total_investment + 
                 NPS + list_price + discount_per + aspiring_market + premium_market + 
                 Digital + Sponsorship, data = MM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove NPS
Model_04 <- lm(formula = gmv ~ sla + total_investment + 
                 list_price + discount_per + aspiring_market + premium_market + 
                 Digital + Sponsorship, data = MM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove Digital
Model_05 <- lm(formula = gmv ~ sla + total_investment + 
                 list_price + discount_per + aspiring_market + premium_market + 
                 Sponsorship, data = MM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove total_investment
Model_06 <- lm(formula = gmv ~ sla + list_price + discount_per + aspiring_market + premium_market + 
                 Sponsorship, data = MM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove list_price
Model_07 <- lm(formula = gmv ~ sla + discount_per + aspiring_market + premium_market + 
                 Sponsorship, data = MM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove Sponsorship
Model_08 <- lm(formula = gmv ~ sla + discount_per + aspiring_market + premium_market, 
               data = MM_Data)
summary(Model_08)
vif(Model_08)
# Multiple R-squared:  0.989,	Adjusted R-squared:  0.988 



## Final model
MM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
MM_MSE <- Model_CrossValidate(MM_Data, as.character(MM_Model_Final$call)[2])


## Elasticity Analysis
MM_Elasticity <- Elasticity_Analysis(MM_Data,MM_Model_Final,"Gaming Accessory - Multiplicative Model")


## Results
GamAcc_Multiplicative_Model <- MM_Model_Final
GamAcc_Multiplicative_Model_MSE <- MM_MSE
GamAcc_Multiplicative_Model_Elasticity <- MM_Elasticity


## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE MODEL - HOME AUDIO
## -----------------------------------------------------------------------------------------------##

## Data Preparation for multiplicative model
MM_Data <- MM_Data_Prep(CE_HomAud_Final)
View(MM_Data)


## Model building

# Model 01
Model_01 <- lm(gmv~. , data = MM_Data)
summary(Model_01)
# Multiple R-squared:  0.692,	Adjusted R-squared:  0.534 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.675,	Adjusted R-squared:  0.608


# Model 3 - remove premium_market
Model_03 <- lm(formula = gmv ~ avg_units + sla + avg_mrp + total_investment + 
                 aspiring_market + TV + Digital, data = MM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove sla
Model_04 <- lm(formula = gmv ~ avg_units + avg_mrp + total_investment + 
                 aspiring_market + TV + Digital, data = MM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove avg_units
Model_05 <- lm(formula = gmv ~ avg_mrp + total_investment + 
                 aspiring_market + TV + Digital, data = MM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove total_investment
Model_06 <- lm(formula = gmv ~ avg_mrp + aspiring_market + TV + Digital, data = MM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove TV
Model_07 <- lm(formula = gmv ~ avg_mrp + aspiring_market + Digital, data = MM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove avg_mrp
Model_08 <- lm(formula = gmv ~  aspiring_market + Digital, data = MM_Data)
summary(Model_08)
vif(Model_08)
# Multiple R-squared:  0.406,	Adjusted R-squared:  0.379 



## Final model
MM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
MM_MSE <- Model_CrossValidate(MM_Data, as.character(MM_Model_Final$call)[2])


## Elasticity Analysis
MM_Elasticity <- Elasticity_Analysis(MM_Data,MM_Model_Final,"Home Audio - Multiplicative Model")


## Results
HomAud_Multiplicative_Model <- MM_Model_Final
HomAud_Multiplicative_Model_MSE <- MM_MSE
HomAud_Multiplicative_Model_Elasticity <- MM_Elasticity

####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ------------------------------- MODEL BUILDING - KOYCK MODEL-----------------------------------##
## -----------------------------------------------------------------------------------------------##

## Delete old data if any
rm(list = ls(pattern = "*_Koyck_Model"))

## -----------------------------------------------------------------------------------------------##
## KOYCK MODEL - CAMERA ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for koyck model
KM_Data <- KM_Data_Prep(CE_CamAcc_Final)
View(KM_Data)


## Model building

# Model 01
Model_01 <- lm(gmv~. , data = KM_Data)
summary(Model_01)
# Multiple R-squared:  0.814,	Adjusted R-squared:  0.688


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.793,	Adjusted R-squared:  0.737 


# Model 3 - remove spl_day
Model_03 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + TV + Digital + Sponsorship +
                 inc_MA_LP1 + inc_MA_DP1, data = KM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove Digital
Model_04 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + TV + Sponsorship +
                 inc_MA_LP1 + inc_MA_DP1, data = KM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove TV
Model_05 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1, data = KM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove sla
Model_06 <- lm(formula = gmv ~ product_procurement_sla + total_investment + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1, data = KM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove product_procurement_sla
Model_07 <- lm(formula = gmv ~  total_investment + 
                 aspiring_market + Sponsorship + inc_MA_LP1 + inc_MA_DP1, data = KM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove aspiring_market
Model_08 <- lm(formula = gmv ~  total_investment + Sponsorship + inc_MA_LP1 + inc_MA_DP1, 
               data = KM_Data)
summary(Model_08)
vif(Model_08)


# Model 9 - remove inc_MA_LP1
Model_09 <- lm(formula = gmv ~  total_investment + Sponsorship + inc_MA_DP1, data = KM_Data)
summary(Model_09)
vif(Model_09)


# Model 10 - remove inc_MA_DP1
Model_10 <- lm(formula = gmv ~  total_investment + Sponsorship, data = KM_Data)
summary(Model_10)
vif(Model_10)


# Model 11 - remove total_investment
Model_11 <- lm(formula = gmv ~ Sponsorship, data = KM_Data)
summary(Model_11)
vif(Model_11)
# Multiple R-squared:  0.253,	Adjusted R-squared:  0.237 


## Final model
KM_Model_Final <- Model_11


## Cross validation with 10 fold and MSE calculation
KM_MSE <- Model_CrossValidate(KM_Data, as.character(KM_Model_Final$call)[2])

## Elasticity Analysis
KM_Elasticity <- Elasticity_Analysis(KM_Data,KM_Model_Final,"Camera Accessory - Koyck Model")

## Results
CamAcc_Koyck_Model <- KM_Model_Final
CamAcc_Koyck_Model_MSE <- KM_MSE
CamAcc_Koyck_Model_Elasticity <- KM_Elasticity


## -----------------------------------------------------------------------------------------------##
## KOYCK MODEL - GAMING ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for koyck model
KM_Data <- KM_Data_Prep(CE_GamAcc_Final)
View(KM_Data)


## Model building

# Model 01
Model_01 <- lm(gmv~. , data = KM_Data)
summary(Model_01)
# Multiple R-squared:  0.939,	Adjusted R-squared:   0.9 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.931,	Adjusted R-squared:  0.917 


# Model 3 - remove premium_market
Model_03 <- lm(formula = gmv ~ sla + total_investment + list_price + discount_per + 
                 aspiring_market + Sponsorship + inc_MA_DP1, 
               data = KM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove inc_MA_DP1
Model_04 <- lm(formula = gmv ~ sla + total_investment + list_price + discount_per + 
                 aspiring_market + Sponsorship, data = KM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove discount_per
Model_05 <- lm(formula = gmv ~ sla + total_investment + list_price +
                 aspiring_market + Sponsorship, data = KM_Data)
summary(Model_05)
vif(Model_05)
# Multiple R-squared:  0.884,	Adjusted R-squared:  0.871


## Final model
KM_Model_Final <- Model_05


## Cross validation with 10 fold and MSE calculation
KM_MSE <- Model_CrossValidate(KM_Data, as.character(KM_Model_Final$call)[2])

## Elasticity Analysis
KM_Elasticity <- Elasticity_Analysis(KM_Data,KM_Model_Final,"Gaming Accessory - Koyck Model")

## Results
GamAcc_Koyck_Model <- KM_Model_Final
GamAcc_Koyck_Model_MSE <- KM_MSE
GamAcc_Koyck_Model_Elasticity <- KM_Elasticity


## -----------------------------------------------------------------------------------------------##
## KOYCK MODEL - HOME AUDIO
## -----------------------------------------------------------------------------------------------##

## Data Preparation for koyck model
KM_Data <- KM_Data_Prep(CE_HomAud_Final)
View(KM_Data)


## Model building

# Model 01
Model_01 <- lm(gmv~. , data = KM_Data)
summary(Model_01)
# Multiple R-squared:  0.813,	Adjusted R-squared:  0.682


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.809,	Adjusted R-squared:  0.725 


# Model 3 - remove premium_market
Model_03 <- lm(formula = gmv ~ avg_mrp + product_procurement_sla + total_investment + 
                 NPS + list_price + discount_per + prepaid_per + aspiring_market + 
                 TV + Digital + Sponsorship + inc_MA_DP1 + 
                 `gmv-1`, data = KM_Data)
summary(Model_03)
vif(Model_03)


# Model 4 - remove discount_per, `gmv-1`, inc_MA_DP1
# Variables removed and verified one at a time and below is consolidated model
Model_04 <- lm(formula = gmv ~ avg_mrp + product_procurement_sla + total_investment + 
                 NPS + list_price +  + prepaid_per + aspiring_market + 
                 TV + Digital + Sponsorship, data = KM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove product_procurement_sla, list_price, Sponsorship
Model_05 <- lm(formula = gmv ~ avg_mrp +  + total_investment + 
                 NPS + prepaid_per + aspiring_market + TV + Digital, data = KM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove NPS, total_investment, prepaid_per, TV
Model_06 <- lm(formula = gmv ~ avg_mrp + aspiring_market + Digital, data = KM_Data)
summary(Model_06)
vif(Model_06)
# Multiple R-squared:  0.589,	Adjusted R-squared:  0.56


## Final model
KM_Model_Final <- Model_06


## Cross validation with 10 fold and MSE calculation
KM_MSE <- Model_CrossValidate(KM_Data, as.character(KM_Model_Final$call)[2])

## Elasticity Analysis
KM_Elasticity <- Elasticity_Analysis(KM_Data,KM_Model_Final,"Home Audio - Koyck Model")

## Results
HomAud_Koyck_Model <- KM_Model_Final
HomAud_Koyck_Model_MSE <- KM_MSE
HomAud_Koyck_Model_Elasticity <- KM_Elasticity


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ------------------------------- MODEL BUILDING - DISTRIBUTED LAG MODEL-------------------------##
## -----------------------------------------------------------------------------------------------##

## Delete old data if any
rm(list = ls(pattern = "*_DistrbutedLag_Model"))

## -----------------------------------------------------------------------------------------------##
## DISTRIBUTED LAG MODEL - CAMERA ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
DLM_Data <- DLM_Data_Prep(CE_CamAcc_Final)
View(DLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = DLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.909,	Adjusted R-squared:  0.708 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.898,	Adjusted R-squared:  0.836 


# Model 3 - remove `NPS-1`
Model_03 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 prepaid_per + aspiring_market + premium_market + TV + Sponsorship + 
                 inc_MA_LP1 + inc_MA_DP1 + Lag_spl_day_1 + Lag_spl_day_3 + 
                 `gmv-1` + `gmv-2` + `gmv-3` + `NPS-3`, data = DLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove prepaid_per
Model_04 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + premium_market + TV + Sponsorship + 
                 inc_MA_LP1 + inc_MA_DP1 + Lag_spl_day_1 + Lag_spl_day_3 + 
                 `gmv-1` + `gmv-2` + `gmv-3` + `NPS-3`, data = DLM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove `gmv-1`
Model_05 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + premium_market + TV + Sponsorship + 
                 inc_MA_LP1 + inc_MA_DP1 + Lag_spl_day_1 + Lag_spl_day_3 + 
                 `gmv-2` + `gmv-3` + `NPS-3`, data = DLM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove Lag_spl_day_3, `gmv-3`, TV, `NPS-3`
Model_06 <- lm(formula = gmv ~ sla + product_procurement_sla + total_investment + 
                 aspiring_market + premium_market + Sponsorship + 
                 inc_MA_LP1 + inc_MA_DP1 + Lag_spl_day_1 + `gmv-2`, data = DLM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove sla, inc_MA_LP1, inc_MA_DP1
Model_07 <- lm(formula = gmv ~ product_procurement_sla + total_investment + 
                 aspiring_market + premium_market + Sponsorship + 
                 Lag_spl_day_1 + `gmv-2`, data = DLM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove premium_market, total_investment, Sponsorship, `gmv-2`
Model_08 <- lm(formula = gmv ~ product_procurement_sla +
                 aspiring_market + Lag_spl_day_1, data = DLM_Data)
summary(Model_08)
vif(Model_08)


# Model 9 - remove aspiring_market, Lag_spl_day_1
Model_09 <- lm(formula = gmv ~ product_procurement_sla, data = DLM_Data)
summary(Model_09)
vif(Model_09)
# Multiple R-squared:  0.241,	Adjusted R-squared:  0.223


## Final model
DLM_Model_Final <- Model_09


## Cross validation with 10 fold and MSE calculation
DLM_MSE <- Model_CrossValidate(DLM_Data, as.character(DLM_Model_Final$call)[2])

## Elasticity Analysis
DLM_Elasticity <- Elasticity_Analysis(DLM_Data,DLM_Model_Final,
                                      "Camera Accessory - Distributed Lag Model")

## Results
CamAcc_DistributedLag_Model <- DLM_Model_Final
CamAcc_DistributedLag_Model_MSE <- DLM_MSE
CamAcc_DistributedLag_Model_Elasticity <- DLM_Elasticity



## -----------------------------------------------------------------------------------------------##
## DISTRIBUTED LAG MODEL - GAMING ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
DLM_Data <- DLM_Data_Prep(CE_GamAcc_Final)
View(DLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = DLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.971,	Adjusted R-squared:  0.91


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.966,	Adjusted R-squared:  0.942


# Model 3 - remove avg_units
Model_03 <- lm(formula = gmv ~ sla + product_procurement_sla + 
                 total_investment + NPS + list_price + discount_per + aspiring_market + 
                 Sponsorship + inc_MA_LP1 + Lag_spl_day_2 + Lag_LP2_per + 
                 Lag_DP2_per + Lag_DP3_per + `gmv-1` + `gmv-2` + `gmv-3` + 
                 `NPS-1` + `NPS-2`, data = DLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove `gmv-2`, `gmv-3`
Model_04 <- lm(formula = gmv ~ sla + product_procurement_sla + 
                 total_investment + NPS + list_price + discount_per + aspiring_market + 
                 Sponsorship + inc_MA_LP1 + Lag_spl_day_2 + Lag_LP2_per + 
                 Lag_DP2_per + Lag_DP3_per + `gmv-1` +
                 `NPS-1` + `NPS-2`, data = DLM_Data)
summary(Model_04)
vif(Model_04)

# Model 5 - remove NPS, Lag_spl_day_2
Model_05 <- lm(formula = gmv ~ sla + product_procurement_sla + 
                 total_investment + list_price + discount_per + aspiring_market + 
                 Sponsorship + inc_MA_LP1 + Lag_LP2_per + 
                 Lag_DP2_per + Lag_DP3_per + `gmv-1` +
                 `NPS-1` + `NPS-2`, data = DLM_Data)
summary(Model_05)
vif(Model_05)

# Model 6 - remove product_procurement_sla, Lag_DP2_per, discount_per
Model_06 <- lm(formula = gmv ~ sla + total_investment + list_price + aspiring_market + 
                 Sponsorship + inc_MA_LP1 + Lag_LP2_per + Lag_DP3_per + `gmv-1` +
                 `NPS-1` + `NPS-2`, data = DLM_Data)
summary(Model_06)
vif(Model_06)

# Model 7 - remove `gmv-1`, `NPS-2`, `NPS-1`, Lag_LP2_per
Model_07 <- lm(formula = gmv ~ sla + total_investment + list_price + aspiring_market + 
                 Sponsorship + inc_MA_LP1 + Lag_DP3_per, data = DLM_Data)
summary(Model_07)
vif(Model_07)
# Multiple R-squared:  0.925,	Adjusted R-squared:  0.911 




## Final model
DLM_Model_Final <- Model_07


## Cross validation with 10 fold and MSE calculation
DLM_MSE <- Model_CrossValidate(DLM_Data, as.character(DLM_Model_Final$call)[2])

## Elasticity Analysis
DLM_Elasticity <- Elasticity_Analysis(DLM_Data,DLM_Model_Final,
                                      "Gaming Accessory - Distributed Lag Model")

## Results
GamAcc_DistributedLag_Model <- DLM_Model_Final
GamAcc_DistributedLag_Model_MSE <- DLM_MSE
GamAcc_DistributedLag_Model_Elasticity <- DLM_Elasticity



## -----------------------------------------------------------------------------------------------##
## DISTRIBUTED LAG MODEL - HOME AUDIO
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
DLM_Data <- DLM_Data_Prep(CE_HomAud_Final)
View(DLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = DLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.937,	Adjusted R-squared:  0.786


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.935,	Adjusted R-squared:  0.842 


# Model 3 - remove Lag_spl_day_3, product_procurement_sla, `NPS-2`
Model_03 <- lm(formula = gmv ~ avg_units + sla + avg_mrp +
                 total_investment + NPS + list_price + discount_per + prepaid_per + 
                 aspiring_market + premium_market + TV + Digital + Sponsorship + 
                 spl_day + inc_MA_LP1 + Lag_spl_day_1 + Lag_spl_day_2 +  
                 Lag_LP3_per + Lag_DP2_per + `gmv-1` + `gmv-2` + `gmv-3` + 
                 `NPS-1`, data = DLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove Sponsorship, spl_day, `gmv-3`, avg_units, sla
Model_04 <- lm(formula = gmv ~ avg_mrp + total_investment + NPS + list_price + discount_per + 
                 prepaid_per + aspiring_market + premium_market + TV + Digital +
                 inc_MA_LP1 + Lag_spl_day_1 + Lag_spl_day_2 +  
                 Lag_LP3_per + Lag_DP2_per + `gmv-1` + `gmv-2` +
                 `NPS-1`, data = DLM_Data)
summary(Model_04)
vif(Model_04)

# Model 5 - remove `NPS-1`, `gmv-2`, premium_market, `gmv-1`
Model_05 <- lm(formula = gmv ~ avg_mrp + total_investment + NPS + list_price + discount_per + 
                 prepaid_per + aspiring_market + TV + Digital +
                 inc_MA_LP1 + Lag_spl_day_1 + Lag_spl_day_2 +  
                 Lag_LP3_per + Lag_DP2_per, data = DLM_Data)
summary(Model_05)
vif(Model_05)

# Model 6 - remove inc_MA_LP1, Lag_DP2_per, Lag_spl_day_2, Lag_spl_day_1
Model_06 <- lm(formula = gmv ~ avg_mrp + total_investment + NPS + list_price + discount_per + 
                 prepaid_per + aspiring_market + TV + Digital + Lag_LP3_per, data = DLM_Data)
summary(Model_06)
vif(Model_06)

# Model 7 - remove Lag_LP3_per, total_investment, TV, prepaid_per
Model_07 <- lm(formula = gmv ~ avg_mrp + NPS + list_price + discount_per + 
                 aspiring_market + Digital, data = DLM_Data)
summary(Model_07)
vif(Model_07)

# Model 8 - remove NPS, discount_per, list_price
Model_08 <- lm(formula = gmv ~ avg_mrp + aspiring_market + Digital, data = DLM_Data)
summary(Model_08)
vif(Model_08)
# Multiple R-squared:  0.587,	Adjusted R-squared:  0.557 


## Final model
DLM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
DLM_MSE <- Model_CrossValidate(DLM_Data, as.character(DLM_Model_Final$call)[2])

## Elasticity Analysis
DLM_Elasticity <- Elasticity_Analysis(DLM_Data,DLM_Model_Final,
                                      "Home Audio - Distributed Lag Model")

## Results
HomAud_DistributedLag_Model <- DLM_Model_Final
HomAud_DistributedLag_Model_MSE <- DLM_MSE
HomAud_DistributedLag_Model_Elasticity <- DLM_Elasticity


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ----------------------MODEL BUILDING - MULTIPLICATIVE + DISTRIBUTED LAG MODEL------------------##
## -----------------------------------------------------------------------------------------------##

## Delete old data if any
rm(list = ls(pattern = "*_MultiDistrLag_Model"))

## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE + DISTRIBUTED LAG MODEL - CAMERA ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
MDLM_Data <- MDLM_Data_Prep(CE_CamAcc_Final)
View(MDLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = MDLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.9948,	Adjusted R-squared:  0.9832 


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.9942,	Adjusted R-squared:  0.991 


# Model 3 - remove NPS.2
Model_03 <- lm(formula = gmv ~ product_procurement_sla + total_investment + 
                 list_price + discount_per + aspiring_market + TV + Digital + 
                 spl_day + inc_MA_LP1 + inc_MA_DP1 + Lag_spl_day_2 + Lag_spl_day_3 + 
                 Lag_DP2_per + NPS.1 + NPS.3, data = MDLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove inc_MA_DP1, Lag_DP2_per, NPS.1
Model_04 <- lm(formula = gmv ~ product_procurement_sla + total_investment + 
                 list_price + discount_per + aspiring_market + TV + Digital + 
                 spl_day + inc_MA_LP1 + Lag_spl_day_2 + Lag_spl_day_3 + 
                 NPS.3, data = MDLM_Data)
summary(Model_04)
vif(Model_04)


# Model 5 - remove Digital
Model_05 <- lm(formula = gmv ~ product_procurement_sla + total_investment + 
                 list_price + discount_per + aspiring_market + TV +
                 spl_day + inc_MA_LP1 + Lag_spl_day_2 + Lag_spl_day_3 + 
                 NPS.3, data = MDLM_Data)
summary(Model_05)
vif(Model_05)


# Model 6 - remove inc_MA_LP1, total_investment
Model_06 <- lm(formula = gmv ~ product_procurement_sla +
                 list_price + discount_per + aspiring_market + TV +
                 spl_day + Lag_spl_day_2 + Lag_spl_day_3 + 
                 NPS.3, data = MDLM_Data)
summary(Model_06)
vif(Model_06)


# Model 7 - remove Lag_spl_day_2, list_price, Lag_spl_day_3
Model_07 <- lm(formula = gmv ~ product_procurement_sla + discount_per + 
                 aspiring_market + TV + spl_day + NPS.3, data = MDLM_Data)
summary(Model_07)
vif(Model_07)


# Model 8 - remove discount_per, spl_day, TV
Model_08 <- lm(formula = gmv ~ product_procurement_sla +
                 aspiring_market + NPS.3, data = MDLM_Data)
summary(Model_08)
vif(Model_08)
# Multiple R-squared:  0.9728,	Adjusted R-squared:  0.9709


## Final model
MDLM_Model_Final <- Model_08


## Cross validation with 10 fold and MSE calculation
MDLM_MSE <- Model_CrossValidate(MDLM_Data, as.character(MDLM_Model_Final$call)[2])

## Elasticity Analysis
MDLM_Elasticity <- Elasticity_Analysis(MDLM_Data,MDLM_Model_Final,
                                       "Camera Accessory - Multiplicative + Distributed Lag Model")

## Results
CamAcc_MultiDistrLag_Model <- MDLM_Model_Final
CamAcc_MultiDistrLag_Model_MSE <- MDLM_MSE
CamAcc_MultiDistrLag_Model_Elasticity <- MDLM_Elasticity



## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE + DISTRIBUTED LAG MODEL - GAMING ACCESSORY
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
MDLM_Data <- MDLM_Data_Prep(CE_GamAcc_Final)
View(MDLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = MDLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.998,	Adjusted R-squared:  0.993


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.998,	Adjusted R-squared:  0.996


# Model 3 - remove Lag_spl_day_1, Lag_spl_day_2, Lag_DP3_per
Model_03 <- lm(formula = gmv ~ sla + avg_mrp + product_procurement_sla + 
                 total_investment + NPS + list_price + discount_per + prepaid_per + 
                 aspiring_market + premium_market + TV + Sponsorship +
                 Lag_spl_day_3 + Lag_LP2_per + Lag_DP2_per + 
                 NPS.1 + NPS.3, data = MDLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove NPS, NPS.3, avg_mrp, total_investment
Model_04 <- lm(formula = gmv ~ sla + product_procurement_sla + 
                 list_price + discount_per + prepaid_per + 
                 aspiring_market + premium_market + TV + Sponsorship +
                 Lag_spl_day_3 + Lag_LP2_per + Lag_DP2_per + 
                 NPS.1, data = MDLM_Data)
summary(Model_04)
vif(Model_04)

# Model 5 - remove Lag_DP2_per, product_procurement_sla, Lag_LP2_per
Model_05 <- lm(formula = gmv ~ sla + list_price + discount_per + prepaid_per + 
                 aspiring_market + premium_market + TV + Sponsorship +
                 Lag_spl_day_3 + NPS.1, data = MDLM_Data)
summary(Model_05)
vif(Model_05)

# Model 6 - remove Lag_spl_day_3, Sponsorship, TV
Model_06 <- lm(formula = gmv ~ sla + list_price + discount_per + prepaid_per + 
                 aspiring_market + premium_market + NPS.1, data = MDLM_Data)
summary(Model_06)
vif(Model_06)

# Model 7 - remove prepaid_per, list_price, NPS.1
Model_07 <- lm(formula = gmv ~ sla + discount_per + aspiring_market + premium_market, 
               data = MDLM_Data)
summary(Model_07)
vif(Model_07)
# Multiple R-squared:  0.989,	Adjusted R-squared:  0.988 




## Final model
MDLM_Model_Final <- Model_07


## Cross validation with 10 fold and MSE calculation
MDLM_MSE <- Model_CrossValidate(MDLM_Data, as.character(MDLM_Model_Final$call)[2])

## Elasticity Analysis
MDLM_Elasticity <- Elasticity_Analysis(MDLM_Data,MDLM_Model_Final,
                                       "Gaming Accessory - Multiplicative + Distributed Lag Model")

## Results
GamAcc_MultiDistrLag_Model <- MDLM_Model_Final
GamAcc_MultiDistrLag_Model_MSE <- MDLM_MSE
GamAcc_MultiDistrLag_Model_Elasticity <- MDLM_Elasticity



## -----------------------------------------------------------------------------------------------##
## MULTIPLICATIVE + DISTRIBUTED LAG MODEL - HOME AUDIO
## -----------------------------------------------------------------------------------------------##

## Data Preparation for distributed lag model
MDLM_Data <- MDLM_Data_Prep(CE_HomAud_Final)
View(MDLM_Data)


# Model 01
Model_01 <- lm(gmv~. , data = MDLM_Data)
summary(Model_01)
vif(Model_01)
# Multiple R-squared:  0.88,	Adjusted R-squared:  0.593


# Model 02 - Run stepAIC
Model_02 <- stepAIC(Model_01,direction = "both")
summary(Model_02)
vif(Model_02)
# Multiple R-squared:  0.864,	Adjusted R-squared:  0.761 


# Model 3 - remove Lag_LP2_per, Lag_spl_day_1, list_price 
Model_03 <- lm(formula = gmv ~ avg_units + sla + product_procurement_sla + 
                 total_investment + NPS + discount_per + aspiring_market + 
                 premium_market + TV + Digital + Online_marketing +
                 Lag_spl_day_2 + Lag_spl_day_3 + gmv.1 + NPS.1 + NPS.2, data = MDLM_Data)
summary(Model_03)
vif(Model_03)

# Model 4 - remove sla, product_procurement_sla, avg_units
Model_04 <- lm(formula = gmv ~ total_investment + NPS + discount_per + aspiring_market + 
                 premium_market + TV + Digital + Online_marketing +
                 Lag_spl_day_2 + Lag_spl_day_3 + gmv.1 + NPS.1 + NPS.2, data = MDLM_Data)
summary(Model_04)
vif(Model_04)

# Model 5 - remove NPS, premium_market, Lag_spl_day_3
Model_05 <- lm(formula = gmv ~ total_investment + discount_per + aspiring_market + 
                 TV + Digital + Online_marketing +
                 Lag_spl_day_2 + gmv.1 + NPS.1 + NPS.2, data = MDLM_Data)
summary(Model_05)
vif(Model_05)

# Model 6 - remove NPS.2, TV, Online_marketing, NPS.1
Model_06 <- lm(formula = gmv ~ total_investment + discount_per + aspiring_market + 
                 Digital + Lag_spl_day_2 + gmv.1, data = MDLM_Data)
summary(Model_06)
vif(Model_06)

# Model 7 - remove total_investment, Lag_spl_day_2, gmv.1
Model_07 <- lm(formula = gmv ~ discount_per + aspiring_market + Digital, data = MDLM_Data)
summary(Model_07)
vif(Model_07)
# Multiple R-squared:  0.557,	Adjusted R-squared:  0.525


## Final model
MDLM_Model_Final <- Model_07


## Cross validation with 10 fold and MSE calculation
MDLM_MSE <- Model_CrossValidate(MDLM_Data, as.character(MDLM_Model_Final$call)[2])

## Elasticity Analysis
MDLM_Elasticity <- Elasticity_Analysis(MDLM_Data,MDLM_Model_Final,
                                       "Home Audio - Multiplicative + Distributed Lag Model")

## Results
HomAud_MultiDistrLag_Model <- MDLM_Model_Final
HomAud_MultiDistrLag_Model_MSE <- MDLM_MSE
HomAud_MultiDistrLag_Model_Elasticity <- MDLM_Elasticity


####################################################################################################
####################################################################################################

## -----------------------------------------------------------------------------------------------##
## ------------------------------- MODEL BUILDING - RESULTS --------------------------------------##
## -----------------------------------------------------------------------------------------------##

## Tabulate Modelling Results

# Get list of models created
Model <- ls(pattern = "_Model$")
Model
    
# Create a dataframe with all models and its results
Modelling_Results <- as.data.frame(Model)
Multiple_R2 <- numeric()
Adj_R2 <- numeric()
MSE <- numeric()
Variables <- character()
    
# Tabulate the results
for (i in 1:length(Model)) {
    # Collect Multiple R squared results
    m_r_sq_eq <- paste("Multiple_R2[",i,"] <- summary(",Modelling_Results$Model[i],")$r.squared", 
                       sep="")
    m_r_sq_eq
    eval(parse(text=m_r_sq_eq))
    
    # Collect Adjusted R squared results 
    r_sq_eq <- paste("Adj_R2[",i,"] <- summary(",Modelling_Results$Model[i],")$adj.r.squared", 
                     sep="")
    r_sq_eq
    eval(parse(text=r_sq_eq))
    
    # Collect MSE results
    mse_eq <- paste("MSE[",i,"] <- ",Modelling_Results$Model[i],"_MSE", sep="")
    mse_eq
    eval(parse(text=mse_eq))
    
    # Collect Variables list
    variables_eq <- paste("Variables[",i,"]<-as.character(",Modelling_Results$Model[i],"$call)[2]",
                          sep="")
    variables_eq
    eval(parse(text=variables_eq))
    # Extract the required variables
    maxlength <- max(nchar(Variables))
    Variables[i] <- substr(Variables[i],7,maxlength)
    Variables[i] <- gsub(' \\+', ',', Variables[i])
}

# Collect all results in one dataframe    
Modelling_Results <- cbind(Modelling_Results, Multiple_R2, Adj_R2, MSE, Variables)
View(Modelling_Results)
    
# Export results to csv file
write.csv(Modelling_Results, "Modelling_Results.csv", row.names = FALSE)


####################################################################################################

## -----------------------------------------------------------------------------------------------##
## -------------------------- MODELLING RESULTS ANALYSIS CONCLUSIONS -----------------------------##
## -----------------------------------------------------------------------------------------------##

## Analysis of results in file Modelling_Results.csv was done and following are conclusions:

## Camera Accessory
##
## Modelling Results
Modelling_Results[grep("CamAcc", Modelling_Results$Model),]
## - Koyck & Distributed Lag Model have low Adj-R2 & high MSE values and hence are rejected.
## - Multiplicative & Multiplicative + Distributed Lag Model have high R2 values which can be 
##   chosen as best model. But Multiplicative model has higher MSE value.
## - So Multiplicative + Distributed Lag Model with high Adj-R2 value and low MSE value would be 
##   best choice for Camera Accessory sub category.
##
## Elasticity of Multiplicative + Distributed Lag Model
CamAcc_MultiDistrLag_Model_Elasticity
ggplot(CamAcc_MultiDistrLag_Model_Elasticity, aes(x = reorder(Variable,Elasticity), y=Elasticity))+ 
  geom_bar(position="dodge",stat="identity") + coord_flip() + 
  ggtitle("Camera Accessory - Multiplicative + Distributed Lag Model") + xlab("Variables")


## Gaming Accessory
## 
## Modelling Results
Modelling_Results[grep("GamAcc", Modelling_Results$Model),]
## - Linear model is ruled out due to lowest Adj-R2 and high MSE values
## - Multiplicative & Multiplicative + Distributed Lag Model are both best options as Adj-R2 has
##   high value and MSE has low values. Also the significant variables are same both Models.
##   Multiplicative Model is chosen finally on the basis of selection of simpler model
## - Distributed Lag Model also is decent option to consider as it has more business variables
##   to work with.
##
## Elasticity of Multiplicative Model
GamAcc_Multiplicative_Model_Elasticity
ggplot(GamAcc_Multiplicative_Model_Elasticity, aes(x = reorder(Variable,Elasticity), y=Elasticity))+ 
  geom_bar(position="dodge",stat="identity") + coord_flip() + 
  ggtitle("Gaming Accessory - Multiplicative Model") + xlab("Variables")



## Home Audio
##
## Modelling Results
Modelling_Results[grep("HomAud", Modelling_Results$Model),]
## - Significant variables aspiring_market and Digital are common across all models. 
## - Multiplicative Model has least Adj-R2 and hence is ruled out
## - Linear and Koyck Model have good Adj-R2 but MSE values are high
## - Multiplicative + Distributed Lag Model has Adj-R2 in mid-range and MSE value is least which
##   makes this model best option for Home Audio Accessory
##
## Elasticity of Multiplicative + Distributed Lag Model
HomAud_MultiDistrLag_Model_Elasticity
ggplot(HomAud_MultiDistrLag_Model_Elasticity, aes(x = reorder(Variable,Elasticity), y=Elasticity))+ 
  geom_bar(position="dodge",stat="identity") + coord_flip() + 
  ggtitle("Home Audio - Multiplicative Model + Distributed Lag Model") + xlab("Variables")



####################################################################################################

## -----------------------------------------------------------------------------------------------##
## -------------------------- RECOMMENDATIONS FOR SALES INCREASE ---------------------------------##
## -----------------------------------------------------------------------------------------------##

## Based on the model selected for each sub category and the elasticity of the variables, following
## recommendations to marketing team to increase the sales

## Camera Accessory
##
## - Increase in promotion of aspiring market products like:
##      - Camera Film Rolls, Reflector Umbrella
## - Discount offers will also have an positive impact on sales
## - Allocate resources for channel of communication and feedback with customers to improve 
##   customer experience who become brand advocates leading to increase in sales

## Gaming Accessory
##
## - Increase in promotion of aspiring market products like:
##      - Cooling Pad, Gaming Headset, Gaming Adapter, Gaming Charging Station, Gaming Keyboard, 
##        Gaming Mouse Pad, Gaming Memory Card & Gaming Speakers
## - Increase in promotion of premium market products like:
##      - Motion Controller, Game Control Mount
## - Discount offers also play a good role in increasing sales and hence more resources can be 
##   channelized here.

## Home Audio
##
## - Increase in promotion of aspiring market products like:
##      - Sound Mixer, DJ Controller, Karaoke Player 
## - Increase expenditure in Digital Marketing to see positive sales results
## - Decrease in discount offers should also be adopted due to negative elasticity being observed.

####################################################################################################


