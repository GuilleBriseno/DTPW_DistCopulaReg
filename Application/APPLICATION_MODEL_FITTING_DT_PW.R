# Useful trycatch variation:
myTryCatch <- function(expr){
  
  warn <- err <- NULL
  
  value <- withCallingHandlers(
    
    tryCatch(expr, error=function(e) {
      
      err <<- e
      
      NULL
    }), warning=function(w) {
      
      warn <<- w
      
      invokeRestart("muffleWarning")
    })
  
  list(value=value, warning=warn, error=err)
}

library(survival)
library(MASS)
library(dplyr)
library(ggplot2)
library(readxl)
library(pammtools)
library(discSurv)




ECylclistsData <- read_excel("Revival_23/APPLICATIONS/BikesApplication/ECylclistsData.xlsx", skip = 2)


original_data <- ECylclistsData

dim(original_data)

head(original_data)


colnames(original_data) <- c("RedLight", "WaitingTime", "RiderType", "Gender", "ComingDirection", "WaitingPosition", "GroupSize", "VisualSearch", "ConformityBehaviour")



### Re-level the columns according to their meaning: 
#red-light running behavior	
#1=yes, 0=no	
unique(original_data$RedLight)
table(original_data$RedLight)

#	rider type
#	0=normal, 1=takeaway
unique(original_data$RiderType)
RiderType_FACTOR <- factor(original_data$RiderType, levels = c(0,1), labels = c("Normal", "TakeAway"))
table(RiderType_FACTOR)

#	gender
# 0=female, 1=male
unique(original_data$Gender)
Gender_FACTOR <- factor(original_data$Gender, levels = c(0,1), labels = c("Female", "Male"))
table(Gender_FACTOR)

# coming direction
#1=through, 2=left, 3=right
unique(original_data$ComingDirection)
ComingDirection_FACTOR <- factor(original_data$ComingDirection, levels = c(1,2,3), labels = c("Through", "LeftTurn", "RightTurn"))
table(ComingDirection_FACTOR)

#	waiting position		
#	0=behind stop line, 2=middle, 3=close to motorized lane	
#	0=behind stop line, 1=middle, 2=close to motorized lane	
unique(original_data$WaitingPosition)
WaitingPosition_FACTOR <- factor(original_data$WaitingPosition, levels = c(0,1,2), labels = c("BehindStopLine", "Middle", "CloseMotorLane"))
table(WaitingPosition_FACTOR)

# group size
# 1=0, 2=1-4, 3=5 and more	
unique(original_data$GroupSize)
GroupSize_FACTOR <- factor(original_data$GroupSize, levels = c(1,2,3), labels = c("0", "1To4", "5AndMore"))
table(GroupSize_FACTOR)

# visual search	
# 1=yes, 2=no	
unique(original_data$VisualSearch)
VisualSearch_RELEVEL <- ifelse(original_data$VisualSearch == 1, 0, 1)
table(VisualSearch_RELEVEL)

# conformity behaviour
# 1=0, 2=1 and more	
unique(original_data$ConformityBehaviour)
ConformityBehavour_RELEVEL <- ifelse(original_data$ConformityBehaviour == 1, 0, 1)
table(ConformityBehavour_RELEVEL)



OriginalData_Short <- data.frame(RiderType = RiderType_FACTOR, 
                                 Gender = Gender_FACTOR, 
                                 ComingDirection = ComingDirection_FACTOR, 
                                 WaitingPosition = WaitingPosition_FACTOR, 
                                 GroupSize = GroupSize_FACTOR, 
                                 VisualSearch = VisualSearch_RELEVEL, 
                                 ConformBehaviour = ConformityBehavour_RELEVEL, 
                                 WaitingTime = original_data$WaitingTime, 
                                 RedLight = original_data$RedLight
)


# Minor modifications and declaring of factors
OriginalData_Short <- OriginalData_Short %>% mutate(WaitingTime = ifelse(WaitingTime == 0, WaitingTime + 0.0001, WaitingTime))


sapply(1:ncol(OriginalData_Short), function(i) class(OriginalData_Short[,i]))

###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################

# Some preliminary data analysis for tables, etc:
colnames(OriginalData_Short)

### For waitingTime we use kaplan meier and stuff like that. 

## For factor variables we use table:
round( table(OriginalData_Short$RedLight) / nrow(OriginalData_Short), 3 ) *100
round( table(OriginalData_Short$VisualSearch) / nrow(OriginalData_Short), 3 ) *100


round( table(OriginalData_Short$Gender) / nrow(OriginalData_Short), 3 ) *100
round( table(OriginalData_Short$RiderType) / nrow(OriginalData_Short), 3 ) *100
round( table(OriginalData_Short$GroupSize) / nrow(OriginalData_Short), 3 ) *100
round( table(OriginalData_Short$ConformBehaviour) / nrow(OriginalData_Short), 3) *100
round( table(OriginalData_Short$WaitingPosition) / nrow(OriginalData_Short), 3 ) *100
round( table(OriginalData_Short$ComingDirection) / nrow(OriginalData_Short), 3 ) *100



round( table(OriginalData_Short$RedLight) / nrow(OriginalData_Short), 3 ) *100

OriginalData_Short$AllOne <- rep(1, nrow(OriginalData_Short))


range(OriginalData_Short$WaitingTime)


KM_fit <- survfit(Surv(WaitingTime, RedLight) ~ RiderType, data = OriginalData_Short)
KM_fit



KM_fit_basic <- survfit(Surv(WaitingTime, RedLight) ~ 1, data = OriginalData_Short)
summary(KM_fit_basic)
quantile(KM_fit_basic, 0.5)


survminer::ggsurvplot(KM_fit,
                      pval = TRUE, 
                      conf.int = TRUE,
                      risk.table = TRUE, # Add risk table
                      surv.median.line = "hv", # Specify median survival
                      ggtheme = theme_bw(), # Change ggplot2 theme
                      palette = c("#E7B800", "#2E9FDF"))




# Some plots
INTRO_PLOT <- ggplot(OriginalData_Short, aes(factor( VisualSearch ) , WaitingTime, fill = factor( RedLight ) )) +
  geom_boxplot() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  scale_y_continuous(breaks = c(seq(0, 140, by = 15))) +
  theme_light() +
  #ggtitle("Binary", subtitle = "(a)") +
  coord_flip() + 
  labs(x = "Visual search (no=0, yes=1)", y = "Waiting time (seconds)") +
  theme(legend.position = "",
        plot.title = element_text(hjust = 0.5, size = 25, vjust = -4),
        plot.subtitle = element_text(hjust = 0.5, size = 23),
        axis.title = element_text(size = 20),
        axis.title.x = element_text(vjust = -0.25),
        axis.title.y = element_text(vjust = +2),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18))

INTRO_PLOT


######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
#
#     Anaylsis using DT approach: 
#
###########################################################################################################
###########################################################################################################
###########################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
#library("GJRM", lib.loc = "/Users/briseno-mac/Downloads/Research_Stuff/Modded_R_Packages/MBS_Install/MBS_FIXEDHESS/MBS_GJRM/")
library("GJRM", lib.loc = "/CustonDirectoryForInstallation/Modified_GJRM/")


source("/prepare_data_Function.R")

######################################################################################################################################################################################################################
range(OriginalData_Short$WaitingTime)


### THREE MODEL SPECIFICATIONS REGARDING INTERVALS: 
the_used_cuts <- 0:131

the_used_cuts_2 <- seq(0, 131, length.out = 66)

the_used_cuts_3 <- seq(0, 131, length.out = 34)

the_used_cuts_4 <- seq(0, 131, length.out = 17)

the_used_cuts_4 <- seq(0, 131, length.out = 16)

the_used_cuts_4 <- seq(0, 131, length.out = 14)


###### 20 INTERVALS
the_used_cuts_4 <- seq(0, 131, length.out = 21)
######################################################################################################################################################################################################################


# ############################################################################################## STRATEGY WITH 1-SECOND LENGTH INTERVAL
# # 0 is always included in this function, so remove it from the vector of the_used_cuts!
# dat_disc <-  contToDisc(dataShort = as.data.frame(OriginalData_Short), 
#                         timeColumn = "WaitingTime", 
#                         intervalLimits = the_used_cuts[-1]
# )
# 
# 
# 
# dataset_long_margin2 <- discSurv:::dataLong(dat_disc, 
#                                             timeColumn = "timeDisc", 
#                                             eventColumn = "RedLight")
# 
# # create list of indices for margin 2 
# INDCS_list <- sapply(levels(as.factor(dataset_long_margin2$obj)), 
#                      function(i) which(dataset_long_margin2$obj == i), 
#                      simplify = FALSE)
# 
# 
# 
# OriginalData_Short$y_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
# OriginalData_Short$timeInt_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
# 
# # rename in long dataset to avoid any issues
# dataset_long_margin2$y_margin2 <- dataset_long_margin2$y
# dataset_long_margin2$timeInt_margin2 <- dataset_long_margin2$timeInt
# 
# ############################################################################################## STRATEGY WITH 65 INTERVALS
# # 0 is always included in this function, so remove it from the vector of the_used_cuts!
# dat_disc <-  contToDisc(dataShort = as.data.frame(OriginalData_Short), 
#                         timeColumn = "WaitingTime", 
#                         intervalLimits = the_used_cuts_2[-1]
# )
# 
# 
# 
# dataset_long_margin2 <- discSurv:::dataLong(dat_disc, 
#                                             timeColumn = "timeDisc", 
#                                             eventColumn = "RedLight")
# 
# # create list of indices for margin 2 
# INDCS_list <- sapply(levels(as.factor(dataset_long_margin2$obj)), 
#                      function(i) which(dataset_long_margin2$obj == i), 
#                      simplify = FALSE)
# 
# 
# 
# OriginalData_Short$y_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
# OriginalData_Short$timeInt_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
# 
# # rename in long dataset to avoid any issues
# dataset_long_margin2$y_margin2 <- dataset_long_margin2$y
# dataset_long_margin2$timeInt_margin2 <- dataset_long_margin2$timeInt
# 
# 
# 



############################################################################################## STRATEGY WITH 33 INTERVALS
# 0 is always included in this function, so remove it from the vector of the_used_cuts!
dat_disc <-  contToDisc(dataShort = as.data.frame(OriginalData_Short), 
                        timeColumn = "WaitingTime", 
                        intervalLimits = the_used_cuts_3[-1]
)



dataset_long_margin2 <- discSurv:::dataLong(dat_disc, 
                                            timeColumn = "timeDisc", 
                                            eventColumn = "RedLight")

# create list of indices for margin 2 
INDCS_list <- sapply(levels(as.factor(dataset_long_margin2$obj)), 
                     function(i) which(dataset_long_margin2$obj == i), 
                     simplify = FALSE)



OriginalData_Short$y_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
OriginalData_Short$timeInt_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)

# rename in long dataset to avoid any issues
dataset_long_margin2$y_margin2 <- dataset_long_margin2$y
dataset_long_margin2$timeInt_margin2 <- dataset_long_margin2$timeInt



############################################################################################## STRATEGY WITH 13 INTERVALS
test <- prepare_data(type = "DT", 
                     data = as.data.frame(OriginalData_Short), 
                     the_intervals = the_used_cuts_4, 
                     DT_timeCol = "WaitingTime", 
                     DT_eventCol = "RedLight")

# 0 is always included in this function, so remove it from the vector of the_used_cuts!
dat_disc <-  contToDisc(dataShort = as.data.frame(OriginalData_Short), 
                        timeColumn = "WaitingTime", 
                        intervalLimits = the_used_cuts_4[-1]
)



dataset_long_margin2 <- discSurv:::dataLong(dat_disc, 
                                            timeColumn = "timeDisc", 
                                            eventColumn = "RedLight")

# create list of indices for margin 2 
INDCS_list <- sapply(levels(as.factor(dataset_long_margin2$obj)), 
                     function(i) which(dataset_long_margin2$obj == i), 
                     simplify = FALSE)



OriginalData_Short$y_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)
OriginalData_Short$timeInt_margin2 <- sample(dataset_long_margin2$y, size = nrow(OriginalData_Short), replace = TRUE)

# rename in long dataset to avoid any issues
dataset_long_margin2$y_margin2 <- dataset_long_margin2$y
dataset_long_margin2$timeInt_margin2 <- dataset_long_margin2$timeInt


identical(test$DataShort, OriginalData_Short)
identical(test$ListOfIndices, INDCS_list)
identical(test$DataLong, dataset_long_margin2)

##################################################################################################################################### 
#####################################################################################################################################
#####################################################################################################################################
#####################################################################################################################################
##################################################################################################################################### 
# ###########################################################################################################
# ###########################################################################################################
# #         PREPARATIONS FOR THE MODEL, SUCH AS EQUATIONS ETC. 
# ###########################################################################################################
# ###########################################################################################################

### Formula for DT approach 
DT_formula <- as.formula(y_margin2 ~ s(timeInt_margin2, bs = "ps", m = 2, k = 10) + 
                           RiderType + Gender + 
                           WaitingPosition + GroupSize + ConformBehaviour
)


DT_formula <- as.formula(y_margin2 ~ s(timeInt_margin2, by = RiderType, bs = "ps", m = 2, k = 10) + 
                           RiderType + Gender + 
                           WaitingPosition + GroupSize + ConformBehaviour
)

# Non-time-to-event margin formula
NonSurv_Mar_formula <- as.formula(VisualSearch ~ RiderType + Gender + WaitingPosition )


copula_candidates <- c("N", "F", "C0", "G0", "C90", "G90")


########################################################################################################### COPULA MODEL: 


# Dependence parameter formula
Dependence_param_formula <- as.formula( ~ RiderType + ConformBehaviour )

MXS_formula <- list(NonSurv_Mar_formula,
                    DT_formula,
                    Dependence_param_formula)


MXS_margins <- c("cloglog", "PO")

candidate_DT_models_cloglog <- sapply(copula_candidates[3], function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # DT approach
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long dataset
    PAMM_dataset = dataset_long_margin2,
    gc.l = FALSE), 
  simplify = FALSE
)

sort(sapply(candidate_DT_models_cloglog, function(i) AIC(i)))
sort(sapply(candidate_DT_models_cloglog, function(i) BIC(i)))



summary(candidate_DT_models_cloglog$C0, nsim = 1000)
plot(candidate_DT_models_cloglog$C0, eq = 2)




##### C0 model with 130 intervals: 
C0_coeffs_130Intervals <- coef(candidate_DT_models_cloglog$C0)

##### C0 model with 65 intervals: 
C0_coeffs_65Intervals <- coef(candidate_DT_models_cloglog$C0)

##### C0 model with 33 intervals: 
C0_coeffs_33Intervals <- coef(candidate_DT_models_cloglog$C0)



length(C0_coeffs_130Intervals)
length(C0_coeffs_65Intervals)
length(C0_coeffs_33Intervals)


rbind(C0_coeffs_130Intervals, 
      C0_coeffs_65Intervals, 
      C0_coeffs_33Intervals) # Virtually same coefficient estimates!




###########################################################################################################
###########################################################################################################
########################################################################################################### RE FIT USING ALL MARGINS: 
###########################################################################################################

# logit for non-time-to-event margin
MXS_margins <- c("logit", "PO")

#### USING LOGIT LINK FUNCTION: 
candidate_DT_models_logit <- sapply(copula_candidates, function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # DT margin
    margins = MXS_margins,# Combination of margins for GJRM
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long dataset
    PAMM_dataset = dataset_long_margin2,
    gc.l = FALSE), 
  simplify = FALSE)


lapply(candidate_DT_models_logit, function(i) conv.check(i))
sort(sapply(candidate_DT_models_logit, function(i) AIC(i)))
sort(sapply(candidate_DT_models_logit, function(i) BIC(i)))




MXS_margins <- c("probit", "PO")

candidate_DT_models_probit <- sapply(copula_candidates, function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # DT margin
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long dataset
    PAMM_dataset = dataset_long_margin2,
    gc.l = FALSE), 
  simplify = FALSE
)

sort(sapply(candidate_DT_models_probit, function(i) AIC(i)))
sort(sapply(candidate_DT_models_probit, function(i) BIC(i)))


MXS_margins <- c("cloglog", "PO")

candidate_DT_models_cloglog <- sapply(copula_candidates, function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # DT margin
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long dataset
    PAMM_dataset = dataset_long_margin2,
    gc.l = FALSE), 
  simplify = FALSE
)

lapply(candidate_DT_models_cloglog, function(i) conv.check(i))
conv.check(candidate_DT_models_cloglog$F)
conv.check(candidate_DT_models_cloglog$C0)
sort(sapply(candidate_DT_models_cloglog, function(i) AIC(i)))
sort(sapply(candidate_DT_models_cloglog, function(i) BIC(i)))




####################################################################### Check best IC, then check the model: 

sort(sapply(candidate_DT_models_logit, function(i) AIC(i)))
sort(sapply(candidate_DT_models_probit, function(i) AIC(i)))
sort(sapply(candidate_DT_models_cloglog, function(i) AIC(i)))

lapply(candidate_DT_models_cloglog, function(i) conv.check(i))

## CLOGLOG according to AIC. CLAYTON_0 copula 

sort(sapply(candidate_DT_models_logit, function(i) BIC(i)))
sort(sapply(candidate_DT_models_probit, function(i) BIC(i)))
sort(sapply(candidate_DT_models_cloglog, function(i) BIC(i)))



(sapply(candidate_DT_models_cloglog, function(i) AIC(i)))
(sapply(candidate_DT_models_cloglog, function(i) BIC(i)))


cbind(c(sapply(candidate_DT_models_cloglog, function(i) AIC(i))),
      c(sapply(candidate_DT_models_cloglog, function(i) BIC(i))))

### CLOGLOG according to BIC, CLAYTON_0 copula too

summary(candidate_DT_models_cloglog$C0, nsim = 10000)






best_fitting_model <- candidate_DT_models_cloglog$C0


library(mvtnorm)
samples_from_beta <- rmvnorm(10000, mean = best_fitting_model$coefficients, sigma = best_fitting_model$Vb)

summary(best_fitting_model, nsim=10000)

### Confidence intervals for margin 1:
apply(samples_from_beta, 2, mean)[1:5]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,1:5]


### Confidence intervals for margin 2:
apply(samples_from_beta, 2, mean)[6:13]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,6:13]


### Confidence intervals for dependence:
apply(samples_from_beta, 2, mean)[32:34]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,32:34]

# save(best_fitting_model,
#      samples_from_beta,
#      file = "Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/BEST_MODEL_DT.RData")



###### Table of Kendall taus combination: 
### Use the samples obtained from the asymptotic distribution: 
### For the point estimator: 
Coppar_Intercept <- best_fitting_model$coefficients[32]
Coppar_RiderTakeaway <- best_fitting_model$coefficients[33]
Coppar_ConformYes <- best_fitting_model$coefficients[34]

SAMPLES_Coppar_Intercept <- samples_from_beta[,32]
SAMPLES_Coppar_RiderTakeaway <- samples_from_beta[,33]
SAMPLES_Coppar_ConformYes <- samples_from_beta[,34]

## This applies response function and transformation to Kendalls tau in one line
PointKendall_NormalNo     <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept ) )
PointKendall_NormalYes    <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_ConformYes ) )
PointKendall_TakeawayNo   <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_RiderTakeaway ) )
PointKendall_TakeawayYes  <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_ConformYes + Coppar_RiderTakeaway ) )

SAMPLES_Kendall_NormalNo      <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept ) )
SAMPLES_Kendall_NormalYes     <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_ConformYes ) )
SAMPLES_Kendall_TakeawayNo    <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_RiderTakeaway) )
SAMPLES_Kendall_TakeawayYes   <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_ConformYes + SAMPLES_Coppar_RiderTakeaway) )

round(PointKendall_NormalNo, 3)
round(quantile(SAMPLES_Kendall_NormalNo, probs = c(0.025, 0.975)), 3)

round(PointKendall_NormalYes, 3)
round(quantile(SAMPLES_Kendall_NormalYes, probs = c(0.025, 0.975)), 3)

round(PointKendall_TakeawayNo, 3)
round(quantile(SAMPLES_Kendall_TakeawayNo, probs = c(0.025, 0.975)), 3)

round(PointKendall_TakeawayYes, 3)
round(quantile(SAMPLES_Kendall_TakeawayYes, probs = c(0.025, 0.975)), 3)


########################################################################################################### INDEPENDENT MODELS:

# Binary margin: 
VisualSearch_IndepModel <- mgcv::gam(formula = NonSurv_Mar_formula, 
                                     family = binomial(link = "cloglog"), 
                                     data = OriginalData_Short)

summary(VisualSearch_IndepModel)

AIC(VisualSearch_IndepModel)
BIC(VisualSearch_IndepModel)


## Confidence intervals from asymptotic distribution:
library(mvtnorm)
samples_from_beta <- rmvnorm(10000, mean = VisualSearch_IndepModel$coefficients, sigma = VisualSearch_IndepModel$Vp)


### Confidence intervals for margin 1:
apply(samples_from_beta, 2, mean)[1:5]
round(VisualSearch_IndepModel$coefficients, digits = 3)
round(apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,1:5], 3)



# Time-to-event margin DT
WaitingTime_DT_IndepModel <- mgcv::gam(formula = DT_formula, 
                                       family = binomial(link = "cloglog"), 
                                       data = dataset_long_margin2)

summary(WaitingTime_DT_IndepModel)

AIC(WaitingTime_DT_IndepModel)
BIC(WaitingTime_DT_IndepModel)


### Confidence intervals for margin 2:
samples_from_beta <- rmvnorm(10000, mean = WaitingTime_DT_IndepModel$coefficients, sigma = WaitingTime_DT_IndepModel$Vp)


apply(samples_from_beta, 2, mean)[2:8]
round(WaitingTime_DT_IndepModel$coefficients[2:8], digits = 3)
round(apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,2:8], 3)


AIC(WaitingTime_DT_IndepModel) + AIC(VisualSearch_IndepModel)
BIC(WaitingTime_DT_IndepModel) + BIC(VisualSearch_IndepModel)





# ##### Plot of the "combinations" transformed to standard normal margins: 
Combination1 <- which(OriginalData_Short$RedLight == 0 & OriginalData_Short$VisualSearch == 0)
Combination2 <- which(OriginalData_Short$RedLight == 0 & OriginalData_Short$VisualSearch == 1)
Combination3 <- which(OriginalData_Short$RedLight == 1 & OriginalData_Short$VisualSearch == 0)
Combination4 <- which(OriginalData_Short$RedLight == 1 & OriginalData_Short$VisualSearch == 1)

sum(length(Combination1), length(Combination2), length(Combination3), length(Combination4))
dim(OriginalData_Short)

length(Combination1) 
length(Combination2)
length(Combination3)
length(Combination4)

eta_VisualSearch <- predict(best_fitting_model, eq = 1)
eta_DeltaWaitingTime <- predict(best_fitting_model, eq = 2)


# save(eta_VisualSearch, 
#      eta_DeltaWaitingTime, 
#      dataset_long_margin2, 
#      OriginalData_Short,
#      file = "Revival_23/APPLICATIONS/BikesApplication/Predicted_etaVisualSearch_etaDeltaWaitingTime_DTModel.RData")


## Apply response functions and obtain F of each margin: 
F_VisualSearch <- 1 - exp( - exp( eta_VisualSearch) )

param_DeltaWaitingTime <- exp( eta_DeltaWaitingTime ) 

## Apply proposed function F_delta.

f_0 <- function(theta_j){
  
  return(exp( - sum(theta_j)) )
  
}

f_1 <- function(theta_j){
  
  return( exp( - sum(theta_j[-length(theta_j)]) ) - exp( - sum(theta_j) )  )
  
}

Functions_f0 <- rep(NA, nrow(OriginalData_Short))

Functions_f1 <- rep(NA, nrow(OriginalData_Short))


for(i in 1:nrow(OriginalData_Short)){
  
  
  Functions_f0[i] <- f_0(param_DeltaWaitingTime[INDCS_list[[i]]])
  
  Functions_f1[i] <- f_1(param_DeltaWaitingTime[INDCS_list[[i]]])
  
}


TheFunctions_F <- ifelse(OriginalData_Short$RedLight == 0, Functions_f0, Functions_f0 + Functions_f1)
TheFunctions_F[which(TheFunctions_F == 1)] <- 0.99999

plot(TheFunctions_F)

##### Apply Gaussian CDF or use directly VineCopula
length(F_VisualSearch)
length(TheFunctions_F)

plot(F_VisualSearch, TheFunctions_F)

par(mfrow = c(2, 2))
kde_par <- 1.55

#length(Combination1)
#length(Combination2)
#length(Combination3)
#length(Combination4)



VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination1], u2 = TheFunctions_F[Combination1], margins = "norm", type = "contour", kde.pars = list(mult = kde_par), 
                     main = "RedLight = 0, VisualSearch = 0, n = 624")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination2], u2 = TheFunctions_F[Combination2], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 0, VisualSearch = 1, n = 414")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination3], u2 = TheFunctions_F[Combination3], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 1, VisualSearch = 0, n = 269")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination4], u2 = TheFunctions_F[Combination4], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 1, VisualSearch = 1, n = 866")
par(mfrow = c(1,1))

VineCopula::BiCopKDE(u1 = 1-F_VisualSearch, u2 = TheFunctions_F, margins = "norm", type = "contour", kde.pars = list(mult = kde_par))





###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
#         VISUALISATION OF RESULTS
###########################################################################################################
###########################################################################################################
###########################################################################################################
# GET READY TO PLOT: 

final_model <- best_fitting_model # candidate_DT_models_cloglog[[3]] 



# At unique intervals:
TimePoints <- sort(unique(dataset_long_margin2$timeInt_margin2))

PlotData <- dataset_long_margin2[1:length(TimePoints),]

###########################################################################################################
# Data has to have the correct levels:
levels(OriginalData_Short$RiderType)
levels(OriginalData_Short$GroupSize)
levels(OriginalData_Short$Gender)
levels(OriginalData_Short$ComingDirection)
levels(OriginalData_Short$WaitingPosition)
unique(OriginalData_Short$ConformBehaviour)


PlotData$timeInt_margin2 <- TimePoints
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))


PlotData$Gender <- factor(rep("Female",length(TimePoints)))
PlotData$ComingDirection <- factor(rep("Through",length(TimePoints)))
PlotData$WaitingPosition <- factor(rep("BehindStopLine",length(TimePoints)))
PlotData$ConformBehaviour <- (rep(0,length(TimePoints)))

PlotData_Takeaway <- PlotData

# ################################################################### The grid is BY RIDER TYPE:  FEMALE drivers ############### USING COPULA MODEL: 
# ################################
# PlotData$Gender <- factor(rep("Female",length(TimePoints)))
# PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
# PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# PlotData_Takeaway$Gender <- factor(rep("Female", length(TimePoints)))
# 
# 
# #### Group Size 0: 
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_NormalRider_Group0 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #### Group Size 1 to 4: 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_NormalRider_Group1To4 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #### Group Size 5 and more: 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_NormalRider_Group5AndMore <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# 
# 
# ########################################################################################## For takeaway rider: 
# #### Group Size 0: 
# PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group0 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))
# 
# #### Group Size 1 to 4: 
# PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group1To4 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))
# 
# #### Group Size 5 and more: 
# PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group5AndMore <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))
# 
# #######################################################################################################################################
# ############################# MALE drivers
# PlotData$Gender <- factor(rep("Male",length(TimePoints)))
# PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
# PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# PlotData_Takeaway$Gender <- factor(rep("Male", length(TimePoints)))
# 
# 
# #### Group Size 0: 
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_NormalRider_Group0_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #### Group Size 1 to 4: 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_NormalRider_Group1To4_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #### Group Size 5 and more: 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_NormalRider_Group5AndMore_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# 
# 
# ########################################################################################## For takeaway rider: 
# #### Group Size 0: 
# PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group0_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))
# 
# #### Group Size 1 to 4: 
# PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group1To4_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))
# 
# #### Group Size 5 and more: 
# PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group5AndMore_MALE <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData_Takeaway) ))


################################################################################################################################ USING INDEPENDENT MODEL:
################################################################### The grid is BY RIDER TYPE:  FEMALE drivers
################################
final_model <- WaitingTime_DT_IndepModel

PlotData$Gender <- factor(rep("Female",length(TimePoints)))
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
PlotData_Takeaway$Gender <- factor(rep("Female", length(TimePoints)))


#### Group Size 0: 
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_NormalRider_Group0 <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))

#### Group Size 1 to 4: 
PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_NormalRider_Group1To4 <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))

#### Group Size 5 and more: 
PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_NormalRider_Group5AndMore <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))



########################################################################################## For takeaway rider: 
#### Group Size 0: 
PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group0 <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))

#### Group Size 1 to 4: 
PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group1To4 <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))

#### Group Size 5 and more: 
PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group5AndMore <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))

#######################################################################################################################################
############################# MALE drivers
PlotData$Gender <- factor(rep("Male",length(TimePoints)))
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
PlotData_Takeaway$Gender <- factor(rep("Male", length(TimePoints)))


#### Group Size 0: 
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_NormalRider_Group0_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))

#### Group Size 1 to 4: 
PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_NormalRider_Group1To4_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))

#### Group Size 5 and more: 
PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_NormalRider_Group5AndMore_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData, type = "link") ))



########################################################################################## For takeaway rider: 
#### Group Size 0: 
PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group0_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))

#### Group Size 1 to 4: 
PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group1To4_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))

#### Group Size 5 and more: 
PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group5AndMore_MALE <- 1 - exp(-exp( predict(final_model, newdata = PlotData_Takeaway, type = "link") ))



#######################################################################################################################################
# COMPUTE SURVIVAL PROBABILITIES: #######################################################################################################################################
#######################################################################################################################################
PlotData$Survival_NormalRyder_Group0 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group0)

PlotData$Survival_NormalRyder_Group1To4 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group1To4)

PlotData$Survival_NormalRyder_Group5AndMore <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group5AndMore)

PlotData$Survival_NormalRyder_Group0_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group0_MALE)

PlotData$Survival_NormalRyder_Group1To4_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group1To4_MALE)

PlotData$Survival_NormalRyder_Group5AndMore_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRider_Group5AndMore_MALE)



PlotData$Survival_TakeawayRyder_Group0 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group0)

PlotData$Survival_TakeawayRyder_Group1To4 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group1To4)

PlotData$Survival_TakeawayRyder_Group5AndMore <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group5AndMore)


PlotData$Survival_TakeawayRyder_Group0_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group0_MALE)

PlotData$Survival_TakeawayRyder_Group1To4_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group1To4_MALE)

PlotData$Survival_TakeawayRyder_Group5AndMore_MALE <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeAwayRider_Group5AndMore_MALE)



# Stack these datasets:
CS1PLOTDATA_FEMALE <- data.frame(Time = rep(TimePoints, 6),
                                 values = c(PlotData$hazard_NormalRider_Group0, 
                                            PlotData$hazard_NormalRider_Group1To4, 
                                            PlotData$hazard_NormalRider_Group5AndMore,
                                            PlotData$hazard_TakeAwayRider_Group0, 
                                            PlotData$hazard_TakeAwayRider_Group1To4, 
                                            PlotData$hazard_TakeAwayRider_Group5AndMore),
                                 GroupSize = factor(rep(c(rep("0", length(TimePoints)),
                                                          rep("1 to 4", length(TimePoints)),
                                                          rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                 RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)


CS1PLOTDATA_MALE <- data.frame(Time = rep(TimePoints, 6),
                               values = c(PlotData$hazard_NormalRider_Group0_MALE, 
                                          PlotData$hazard_NormalRider_Group1To4_MALE, 
                                          PlotData$hazard_NormalRider_Group5AndMore_MALE,
                                          PlotData$hazard_TakeAwayRider_Group0_MALE, 
                                          PlotData$hazard_TakeAwayRider_Group1To4_MALE,
                                          PlotData$hazard_TakeAwayRider_Group5AndMore_MALE),
                               GroupSize = factor(rep(c(rep("0", length(TimePoints)),
                                                        rep("1 to 4", length(TimePoints)),
                                                        rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                               RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)

CS1PLOTDATA <- rbind(CS1PLOTDATA_FEMALE, CS1PLOTDATA_MALE)

CS1PLOTDATA$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE)),
                               rep("Male", nrow(CS1PLOTDATA_FEMALE))))




#######################
#######################
#######################
CS1PLOTDATA_FEMALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
                                      values = c(PlotData$Survival_NormalRyder_Group0, 
                                                 PlotData$Survival_NormalRyder_Group1To4, 
                                                 PlotData$Survival_NormalRyder_Group5AndMore,
                                                 PlotData$Survival_TakeawayRyder_Group0, 
                                                 PlotData$Survival_TakeawayRyder_Group1To4, 
                                                 PlotData$Survival_TakeawayRyder_Group5AndMore),
                                      GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
                                                               rep("1 to 4", length(TimePoints)), 
                                                               rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                      RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)


CS1PLOTDATA_MALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
                                    values = c(PlotData$Survival_NormalRyder_Group0_MALE, 
                                               PlotData$Survival_NormalRyder_Group1To4_MALE, 
                                               PlotData$Survival_NormalRyder_Group5AndMore_MALE,
                                               PlotData$Survival_TakeawayRyder_Group0_MALE, 
                                               PlotData$Survival_TakeawayRyder_Group1To4_MALE, 
                                               PlotData$Survival_TakeawayRyder_Group5AndMore_MALE),
                                    GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
                                                             rep("1 to 4", length(TimePoints)), 
                                                             rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                    RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)

CS1PLOTDATA_SURV <- rbind(CS1PLOTDATA_FEMALE_SURV, CS1PLOTDATA_MALE_SURV)

CS1PLOTDATA_SURV$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE_SURV)), 
                                    rep("Male", nrow(CS1PLOTDATA_FEMALE_SURV))))


#### TimePoints Real:
TimePoints_Real <- seq(0, 131, length.out = 21)[-1]

CS1PLOTDATA$TimeReal <- rep(TimePoints_Real, 12)
CS1PLOTDATA_SURV$TimeReal <- rep(TimePoints_Real, 12)










# 
# ##############################################################################################################################
# PlotData$hazard_NormalRyder <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# 
# PlotData$hazard_NormalRyderGroup2 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# 
# PlotData$hazard_NormalRyderGroup3 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# 
# PlotData_Takeaway <- PlotData
# 
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyder <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #######################################################################################################################################
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyderGroup2 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyderGroup3 <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #######################################################################################################################################
# #
# #
# #.   CHANGE IN GENDER
# #######################################################################################################################################
# PlotData$Gender <- factor(rep("Male",length(TimePoints)))
# 
# PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# 
# 
# PlotData$ComingDirection <- factor(rep("Through",length(TimePoints)))
# PlotData$WaitingPosition <- factor(rep("BehindStopLine",length(TimePoints)))
# 
# 
# ##############################################################################################################################
# PlotData$hazard_NormalRyderMale <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# 
# PlotData$hazard_NormalRyderGroup2Male <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# 
# PlotData$hazard_NormalRyderGroup3Male <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# 
# 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyderMale <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# #######################################################################################################################################
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyderGroup2Male <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# 
# PlotData$hazard_TakeawayRyderGroup3Male <- 1 - exp(-exp( predict(final_model, 2, newdata = PlotData) ))
# 
# 
# 
# #######################################################################################################################################
# # COMPUTE SURVIVAL PROBABILITIES: #######################################################################################################################################
# #######################################################################################################################################
# PlotData$Survival_NormalRyder <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyder)
# 
# PlotData$Survival_NormalRyderG2 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyderGroup2)
# 
# PlotData$Survival_NormalRyderG3 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyderGroup3)
# 
# PlotData$Survival_TakeawayRyder <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyder)
# 
# PlotData$Survival_TakeawayRyderG2 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyderGroup2)
# 
# PlotData$Survival_TakeawayRyderG3 <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyderGroup3)
# 
# # #### FOR MALE
# PlotData$Survival_NormalRyderMale <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyderMale)
# 
# PlotData$Survival_NormalRyderG2Male <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyderGroup2Male)
# 
# PlotData$Survival_NormalRyderG3Male <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_NormalRyderGroup3Male)
# 
# PlotData$Survival_TakeawayRyderMale <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyderMale)
# 
# PlotData$Survival_TakeawayRyderG2Male <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyderGroup2Male)
# 
# PlotData$Survival_TakeawayRyderG3Male <- cumprod(rep(1, nrow(PlotData)) - PlotData$hazard_TakeawayRyderGroup3Male)
# 
# 
# 
# ggplot(PlotData_Takeaway, aes(timeInt_margin2, hazard_NormalRyder)) +
#   geom_line() +
#   geom_line(data = PlotData, aes(timeInt_margin2, hazard_TakeawayRyder), col = "blue") +
#   geom_line(data = PlotData, aes(timeInt_margin2, hazard_TakeawayRyder), col = "blue") +
#   #
#   #
#   #
#   geom_line(data = PlotData, aes(timeInt_margin2, hazard_TakeawayRyder), col = "blue") +
#   geom_line(data = PlotData, aes(timeInt_margin2, hazard_TakeawayRyderGroup2), col = "red") +
#   geom_line(data = PlotData, aes(timeInt_margin2, hazard_TakeawayRyderGroup3), col = "orange")






# # Stack these datasets:
# CS1PLOTDATA_FEMALE <- data.frame(Time = rep(TimePoints, 6),
#                                  values = c(PlotData$hazard_NormalRyder, PlotData$hazard_NormalRyderGroup2, PlotData$hazard_NormalRyderGroup3,
#                                             PlotData$hazard_TakeawayRyder, PlotData$hazard_TakeawayRyderGroup2, PlotData$hazard_TakeawayRyderGroup3),
#                                  GroupSize = factor(rep(c(rep("0", length(TimePoints)),
#                                                           rep("1 to 4", length(TimePoints)),
#                                                           rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
#                                  RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
# )
# 
# 
# CS1PLOTDATA_MALE <- data.frame(Time = rep(TimePoints, 6),
#                                values = c(PlotData$hazard_NormalRyderMale, PlotData$hazard_NormalRyderGroup2Male, PlotData$hazard_NormalRyderGroup3Male,
#                                           PlotData$hazard_TakeawayRyderMale, PlotData$hazard_TakeawayRyderGroup2Male, PlotData$hazard_TakeawayRyderGroup3Male),
#                                GroupSize = factor(rep(c(rep("0", length(TimePoints)),
#                                                         rep("1 to 4", length(TimePoints)),
#                                                         rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
#                                RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
# )
# 
# CS1PLOTDATA <- rbind(CS1PLOTDATA_FEMALE, CS1PLOTDATA_MALE)
# 
# CS1PLOTDATA$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE)),
#                                rep("Male", nrow(CS1PLOTDATA_FEMALE))))
# 
# 
# 
# 
# #######################
# #######################
# #######################
# CS1PLOTDATA_FEMALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
#                                       values = c(PlotData$Survival_NormalRyder, PlotData$Survival_NormalRyderG2, PlotData$Survival_NormalRyderG3,
#                                                  PlotData$Survival_TakeawayRyder, PlotData$Survival_TakeawayRyderG2, PlotData$Survival_TakeawayRyderG3),
#                                       GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
#                                                                rep("1 to 4", length(TimePoints)), 
#                                                                rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
#                                       RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
# )
# 
# 
# CS1PLOTDATA_MALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
#                                     values = c(PlotData$Survival_NormalRyderMale, PlotData$Survival_NormalRyderG2Male, PlotData$Survival_NormalRyderG3Male,
#                                                PlotData$Survival_TakeawayRyderMale, PlotData$Survival_TakeawayRyderG2Male, PlotData$Survival_TakeawayRyderG3Male),
#                                     GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
#                                                              rep("1 to 4", length(TimePoints)), 
#                                                              rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
#                                     RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
# )
# 
# CS1PLOTDATA_SURV <- rbind(CS1PLOTDATA_FEMALE_SURV, CS1PLOTDATA_MALE_SURV)
# 
# CS1PLOTDATA_SURV$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE_SURV)), 
#                                     rep("Male", nrow(CS1PLOTDATA_FEMALE_SURV))))





hazard_rates_DT <- ggplot(CS1PLOTDATA, aes(TimeReal, values, linetype = Gender, col = GroupSize, fill = GroupSize)) +
  geom_line(linewidth = 0.85) +
  #geom_step(linewidth = 0.85) +
  facet_grid(~ RiderType ) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "darkgreen")) +
  ylab(expression(hat(lambda)(WaitingTime))) +
  xlab(paste(expression(WaitingTime), "(seconds)") ) +
  labs(title = "(a)") + 
  #scale_x_continuous(breaks = seq(0, 130, by = 20), labels = seq(0, 130, by =  20)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c(0, 20, 40, 60, 80, 100, 120)) +
  #scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075), labels = c(0, 0.025, 0.05, 0.075), limits = c(0,0.075)) +
  #lims(y = c(0, 0.1)) + 
  theme_light() +
  theme(legend.position = "",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 27, hjust = 0.5),
        strip.text = element_text(size = 22))




survival_functions_DT <- ggplot(CS1PLOTDATA_SURV, aes(TimeReal, values, linetype = Gender, col = GroupSize, fill = GroupSize)) +
  geom_line(linewidth = 0.85) +
  #geom_step(linewidth = 0.85) +
  facet_grid(~ RiderType ) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "darkgreen")) +
  ylab(expression(hat(S)(WaitingTime))) + xlab(paste(expression(WaitingTime), "(seconds)") ) + labs(fill = "Group Size") +
  geom_hline(yintercept = 0.5, col = "red", linetype = "solid", alpha = 0.5) +
  geom_hline(yintercept = 0, col = "black", linetype = "solid", alpha = 0) +
  #scale_x_continuous(breaks = seq(0,130,  length.out = 20), labels = seq(0,130, length.out =  20)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c(0, 20, 40, 60, 80, 100, 120)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0.0, 0.25, 0.50, 0.75, 1)) + 
  theme_light() +
  #guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) + 
  guides(color = guide_legend(override.aes = list(size = 5)),
         linetype = guide_legend(override.aes = list(size = 2))) + 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 22),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 25, hjust = 0.5),
        legend.box="vertical", legend.margin=margin())

hazard_rates_DT
survival_functions_DT


# save(hazard_rates_DT, survival_functions_DT, 
#      file ="Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/DT_HazardSurvivalPlots.RData"
# )

gridExtra::grid.arrange(hazard_rates_DT, survival_functions_DT, nrow = 2)



####### ALL GJRM EFFECTS: 
summary(final_model)

par(mfrow = c(1, 1))
plot(final_model,2)



# DIAGNOSTIC

# function for residuals and diagnostics!
compute_deviance_resid <- function(original_status, other_residual){
  
  devres <- sign(other_residual) * sqrt( -2 * (other_residual + original_status * log(original_status - other_residual ) ) )
  
  return(devres)
  
}


compute_relevant_residuals <- function(model, data_short, data_long, type, indices, equation_survmodel){
  
  if(type == "DT"){
    
    pred_haz <- 1 - exp( -exp( predict(model, equation_survmodel) ) )
    
    # cox snell residuals
    cxs_resids <- sapply(indices, function(i)  sum( pred_haz[i]  ) ) 
    
    cxs_resids_logoneminus <- sapply(indices, function(i)  sum( - log( 1 - pred_haz[i]) ) ) 
    
    # martingale
    mart_resids <- data_short$status - cxs_resids
    
    mart_resids_logoneminus <- data_short$status - cxs_resids_logoneminus
    
    # deviance residuals
    dv_resids <- compute_deviance_resid(data_short$status, mart_resids)
    
    dv_resids_logoneminus <- compute_deviance_resid(data_short$status, mart_resids_logoneminus)
    
    
    relevant_residuals <- data.frame(cox_snell_residuals = cxs_resids, 
                                     deviance_residuals = dv_resids,
                                     martingale_residuals = mart_resids,
                                     #
                                     #
                                     logoneminus_cox_snell = cxs_resids_logoneminus,
                                     logoneminus_deviance = dv_resids_logoneminus,
                                     logoneminus_martingale = mart_resids_logoneminus
    ) 
    
    # temporal object
    relevant_residuals_temp <- relevant_residuals
    
    relevant_residuals_temp$status <- data_short$status
    
    # Attach the Nelson Aalen estimator of the cox-snell logoneminus residuals
    relevant_residuals$NelsonAalen_coxsnell_logoneminus <- mice::nelsonaalen(relevant_residuals_temp, logoneminus_cox_snell, status)
    
  }
  
  
  if(type == "PW"){
    
    # hazard rate
    pred_haz <- exp( predict(res$value, 1) )
    
    # cox snell residuals
    cxs_resids <- sapply(1:length(indices), function(i) sum(  pred_haz[ indices[[i]] ] * exp( data_long$offset[ indices[[i]] ] ) ) )
    
    
    # martingale residuals
    mart_resids <- data_short$status - cxs_resids
    
    # deviance residuals 
    dv_resids <- compute_deviance_resid(data_short$status, mart_resids)
    
    
    relevant_residuals <- data.frame(cox_snell_residuals = cxs_resids, 
                                     deviance_residuals = dv_resids,
                                     martingale_residuals = mart_resids) 
    
    # temporal object
    relevant_residuals_temp <- relevant_residuals
    
    relevant_residuals_temp$status <- data_short$status
    
  }
  
  
  # Attach the Nelson Aalen estimator of the cox-snell residuals
  relevant_residuals$NelsonAalen_coxsnell <- mice::nelsonaalen(relevant_residuals_temp, cox_snell_residuals, status)
  
  
  return(relevant_residuals)
  
}

OriginalData_Short$status <- OriginalData_Short$RedLight

the_residuals <- compute_relevant_residuals(final_model, 
                                            data_short = OriginalData_Short, 
                                            data_long = dataset_long_margin2, 
                                            type = "DT", 
                                            indices = INDCS_list, 
                                            equation_survmodel = 2)




ggplot(the_residuals, aes(logoneminus_cox_snell, NelsonAalen_coxsnell_logoneminus)) +
  geom_point(size = 2) +
  labs(x = expression(hat(r)^{CS}), y = expression(hat(Lambda)(hat(r)^{CS})), title = "(a)") +
  theme_light() +
  geom_abline(slope = 1, col = "red") +
  theme(legend.position = "right",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10), 
        plot.title = element_text(hjust = 0.5, size = 30)
  )






###########################################################################################################
###########################################################################################################
###########################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
###########################################################################################################
###########################################################################################################
#
#     Analysis using PW approach: 
#
###########################################################################################################
###########################################################################################################
###########################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
######################################################################################################################################################################################################################
###########################################################################################################
###########################################################################################################
#          PREPARATIONS FOR THE DATASET
###########################################################################################################
###########################################################################################################
library("GJRM", lib.loc = "/Users/briseno-mac/Downloads/Research_Stuff/Modded_R_Packages/MBS_Install/MBS_FIXEDHESS/MBS_GJRM/")



library(pammtools)

maximum_time_cut <- max(OriginalData_Short$WaitingTime)

#number_of_ints <- 13
number_of_ints <- 20


test <- prepare_data(type = "PW", 
                     data = as.data.frame(OriginalData_Short), 
                     the_intervals = the_used_cuts_4, 
                     PW_formula = Surv(WaitingTime, RedLight) ~ ., 
                     PW_maximum_time_cut = maximum_time_cut)



### Create long-format data:
dat_ped <- as_ped(Surv(WaitingTime, RedLight) ~ ., 
                  max_time = maximum_time_cut,
                  cut = seq(from = 0,
                            to = maximum_time_cut,
                            length.out = number_of_ints),
                  data = OriginalData_Short)


# declare IDs as factor:
dat_ped$id <- factor(dat_ped$id)


# Create list of indices: (ABSOLUTELY NECESSARY FOR MBS)
INDCS_list <- sapply(levels(dat_ped$id),
                     function(i) which(dat_ped$id == i),
                     simplify = FALSE)


# attach ped_status in the model:
OriginalData_Short$ped_status <- unlist(lapply(INDCS_list, function(i) tail(i, 1)))
OriginalData_Short$tend <- dat_ped$tend[OriginalData_Short$ped_status]

dataset_long_margin2 <- dat_ped


dim(dat_ped)
dim(test$DataLong)

identical(dat_ped, test$DataLong)
identical(INDCS_list, test$ListOfIndices)



########################################## Model configuration
### Formula for PW approach 
PW_formula <- as.formula(ped_status ~ s(tend, bs = "ps", m = 2, k = 10) + 
                           RiderType + Gender + WaitingPosition + GroupSize + ConformBehaviour
)

PW_formula <- as.formula(ped_status ~ s(tend, by = RiderType, bs = "ps", m = 2, k = 10) + 
                           RiderType + Gender + WaitingPosition + GroupSize + ConformBehaviour
)

# Non-time-to-event margin formula
NonSurv_Mar_formula <- as.formula(VisualSearch ~ RiderType + Gender + WaitingPosition)


########################################################################################################### Independence model: 
# Time-to-event margin PW
WaitingTime_PW_IndepModel <- mgcv::gam(formula = PW_formula, 
                                       family = poisson(), offset = offset,
                                       data = dataset_long_margin2)

summary(WaitingTime_PW_IndepModel)

AIC(WaitingTime_PW_IndepModel)
BIC(WaitingTime_PW_IndepModel)


### Confidence intervals for margin 2:
samples_from_beta <- rmvnorm(10000, mean = WaitingTime_PW_IndepModel$coefficients, sigma = WaitingTime_PW_IndepModel$Vp)


apply(samples_from_beta, 2, mean)[2:8]
round(WaitingTime_PW_IndepModel$coefficients[2:8], digits = 3)
round(apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,2:8], 3)


AIC(VisualSearch_IndepModel) + AIC(WaitingTime_PW_IndepModel)
AIC(VisualSearch_IndepModel) + BIC(WaitingTime_PW_IndepModel)

###########################################################################################################



# Dependence parameter formula
Dependence_param_formula <- as.formula( ~ RiderType + ConformBehaviour )


MXS_formula <- list(NonSurv_Mar_formula,
                    PW_formula,
                    Dependence_param_formula)



###########################################################################################################
#               FIT THE MODEL  USING PW  APPROACH
###########################################################################################################
###########################################################################################################

copula_candidates <- c("N", "F", "C0", "G0", "C90", "G90")


MXS_margins <- c("logit", "PO")

candidate_PW_models_logit <- sapply(copula_candidates, function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # either DT or PW approach
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long data
    PAMM_dataset = dataset_long_margin2,
    # Offset for PW approach
    PAMM_offset = dataset_long_margin2$offset,
    # further arguments
    gc.l = TRUE), 
  #
  simplify = FALSE
)



lapply(candidate_DT_models, function(i) conv.check(i))
sort(sapply(candidate_DT_models, function(i) AIC(i)))



MXS_margins <- c("probit", "PO")

candidate_DT_models_probit <- sapply(copula_candidates, function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # either DT or PW approach
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long data
    PAMM_dataset = dataset_long_margin2,
    # Offset for PW approach
    PAMM_offset = dataset_long_margin2$offset,
    # further arguments
    gc.l = TRUE), 
  #
  simplify = FALSE
)


sort(sapply(candidate_DT_models_probit, function(i) AIC(i)))



MXS_margins <- c("cloglog", "PO")


### change the index here to 1, 2, 3, 4, 5, or 6. 
candidate_DT_models_cloglog <- sapply(copula_candidates[3], function(i) 
  
  gjrm(
    MXS_formula, 
    data = OriginalData_Short, 
    BivD = i,      
    # either DT or PW approach
    margins = MXS_margins,# Combination of margins
    Model = "B",
    # List of subject ids
    ListOfIDs = INDCS_list,
    # long data
    PAMM_dataset = dataset_long_margin2,
    # Offset for PW approach
    PAMM_offset = dataset_long_margin2$offset,
    # further arguments
    gc.l = TRUE), 
  #
  simplify = FALSE
)

PW_CLOGLOG_AICs <- sort(sapply(candidate_DT_models_cloglog, function(i) AIC(i)))
PW_CLOGLOG_BICs <- sort(sapply(candidate_DT_models_cloglog, function(i) BIC(i)))

PW_CLOGLOG_AICs
PW_CLOGLOG_BICs
# 
# save(PW_CLOGLOG_AICs,
#      PW_CLOGLOG_BICs,
#      file = "Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/PW_GUMBEL90COP_STRATA_20INTS_AICBIC.RData")



### CLOGLOG according to BIC, CLAYTON_0 copula too

summary(candidate_DT_models_cloglog$C0, nsim = 10000)
plot(candidate_DT_models_cloglog$C0, eq = 2)
### looks like, at 5% significance 

best_fitting_model_PW <- candidate_DT_models_cloglog$C0


#save(best_fitting_model_PW, file = "Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/PW_ClaytonCopulaModel.RData")

summary(best_fitting_model_PW, nsim = 10000)

library(mvtnorm)
samples_from_beta <- rmvnorm(10000, mean = best_fitting_model_PW$coefficients, sigma = best_fitting_model_PW$Vb)

### Confidence intervals for margin 1:
apply(samples_from_beta, 2, mean)[1:5]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,1:5]


### Confidence intervals for margin 2:
apply(samples_from_beta, 2, mean)[6:13]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,6:13]


### Confidence intervals for dependence:
apply(samples_from_beta, 2, mean)[32:34]
apply(samples_from_beta, 2, quantile, probs = c(0.025, 0.975))[,32:34]




###### Table of Kendall taus combination: 
### Use the samples obtained from the asymptotic distribution: 
### For the point estimator: 
Coppar_Intercept <- best_fitting_model_PW$coefficients[32]
Coppar_RiderTakeaway <- best_fitting_model_PW$coefficients[33]
Coppar_ConformYes <- best_fitting_model_PW$coefficients[34]

SAMPLES_Coppar_Intercept <- samples_from_beta[,32]
SAMPLES_Coppar_RiderTakeaway <- samples_from_beta[,33]
SAMPLES_Coppar_ConformYes <- samples_from_beta[,34]

## This applies response function and transformation to Kendalls tau in one line
PointKendall_NormalNo     <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept ) )
PointKendall_NormalYes    <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_ConformYes ) )
PointKendall_TakeawayNo   <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_RiderTakeaway ) )
PointKendall_TakeawayYes  <- VineCopula::BiCopPar2Tau(family = 3, par = exp( Coppar_Intercept + Coppar_ConformYes + Coppar_RiderTakeaway ) )

SAMPLES_Kendall_NormalNo      <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept ) )
SAMPLES_Kendall_NormalYes     <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_ConformYes ) )
SAMPLES_Kendall_TakeawayNo    <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_RiderTakeaway) )
SAMPLES_Kendall_TakeawayYes   <- VineCopula::BiCopPar2Tau(family = 3, par = exp( SAMPLES_Coppar_Intercept + SAMPLES_Coppar_ConformYes + SAMPLES_Coppar_RiderTakeaway) )

round(PointKendall_NormalNo, 3)
round(quantile(SAMPLES_Kendall_NormalNo, probs = c(0.025, 0.975)), 3)

round(PointKendall_NormalYes, 3)
round(quantile(SAMPLES_Kendall_NormalYes, probs = c(0.025, 0.975)), 3)

round(PointKendall_TakeawayNo, 3)
round(quantile(SAMPLES_Kendall_TakeawayNo, probs = c(0.025, 0.975)), 3)

round(PointKendall_TakeawayYes, 3)
round(quantile(SAMPLES_Kendall_TakeawayYes, probs = c(0.025, 0.975)), 3)


# ##### Plot of the "combinations" transformed to standard normal margins: 
# Combination1 <- which(OriginalData_Short$RedLight == 0 & OriginalData_Short$VisualSearch == 0)
# Combination2 <- which(OriginalData_Short$RedLight == 0 & OriginalData_Short$VisualSearch == 1)
# Combination3 <- which(OriginalData_Short$RedLight == 1 & OriginalData_Short$VisualSearch == 0)
# Combination4 <- which(OriginalData_Short$RedLight == 1 & OriginalData_Short$VisualSearch == 1)

sum(length(Combination1), length(Combination2), length(Combination3), length(Combination4))
dim(OriginalData_Short)


eta_VisualSearch <- predict(best_fitting_model_PW, eq = 1)
eta_DeltaWaitingTime <- predict(best_fitting_model_PW, eq = 2)


# save(eta_VisualSearch, 
#      eta_DeltaWaitingTime, 
#      dataset_long_margin2, 
#      OriginalData_Short,
#      file = "Revival_23/APPLICATIONS/BikesApplication/Predicted_etaVisualSearch_etaDeltaWaitingTime_PWModel.RData")

## Apply response functions and obtain F of each margin: 
F_VisualSearch <- 1 - exp( - exp( eta_VisualSearch) )

param_DeltaWaitingTime <- exp( eta_DeltaWaitingTime ) * exp(dataset_long_margin2$offset)

## Apply proposed function F_delta.

f_0 <- function(theta_j){
  
  return(exp( - sum(theta_j)) )
  
}

f_1 <- function(theta_j){
  
  return( exp( - sum(theta_j) ) * tail(theta_j, 1)  )
  
}

Functions_f0 <- rep(NA, nrow(OriginalData_Short))

Functions_f1 <- rep(NA, nrow(OriginalData_Short))


for(i in 1:nrow(OriginalData_Short)){
  
  
  Functions_f0[i] <- f_0(param_DeltaWaitingTime[INDCS_list[[i]]])
  
  Functions_f1[i] <- f_1(param_DeltaWaitingTime[INDCS_list[[i]]])
  
}


TheFunctions_F <- ifelse(OriginalData_Short$RedLight == 0, Functions_f0, Functions_f0 + Functions_f1)
TheFunctions_F[which(TheFunctions_F == 1)] <- 0.99999

plot(TheFunctions_F)

##### Apply Gaussian CDF or use directly VineCopula
length(F_VisualSearch)
length(TheFunctions_F)

plot(F_VisualSearch, TheFunctions_F)

par(mfrow = c(2, 2))
kde_par <- 1.55

#length(Combination1)
#length(Combination2)
#length(Combination3)
#length(Combination4)

VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination1], u2 = TheFunctions_F[Combination1], margins = "norm", type = "contour", kde.pars = list(mult = kde_par), 
                     main = "RedLight = 0, VisualSearch = 0, n = 624")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination2], u2 = TheFunctions_F[Combination2], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 0, VisualSearch = 1, n = 414")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination3], u2 = TheFunctions_F[Combination3], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 1, VisualSearch = 0, n = 269")
VineCopula::BiCopKDE(u1 = 1-F_VisualSearch[Combination4], u2 = TheFunctions_F[Combination4], margins = "norm", type = "contour", kde.pars = list(mult = kde_par),
                     main = "RedLight = 1, VisualSearch = 1, n = 866")
par(mfrow = c(1,1))


VineCopula::BiCopKDE(u1 = 1-F_VisualSearch, u2 = TheFunctions_F, margins = "norm", type = "contour", kde.pars = list(mult = kde_par))



###########################################################################################################
###########################################################################################################

6.8421052631579 * 19


TimePoints <- sort(unique(dataset_long_margin2$tend))

The_Offsets <- log(6.8421052631579)

PlotData <- dataset_long_margin2[1:length(TimePoints),]


PlotData$offset <- rep(The_Offsets, nrow(PlotData))


# Data has to have the correct levels:
levels(OriginalData_Short$RiderType)
levels(OriginalData_Short$GroupSize)
levels(OriginalData_Short$Gender)
levels(OriginalData_Short$ComingDirection)
levels(OriginalData_Short$WaitingPosition)
unique(OriginalData_Short$ConformBehaviour)


PlotData$tend <- TimePoints
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))


PlotData$Gender <- factor(rep("Female",length(TimePoints)))
PlotData$ComingDirection <- factor(rep("Through",length(TimePoints)))
PlotData$WaitingPosition <- factor(rep("BehindStopLine",length(TimePoints)))
PlotData$ConformBehaviour <- (rep(0,length(TimePoints)))

PlotData_Takeaway <- PlotData

# ################################################################### The grid is BY RIDER TYPE:  FEMALE drivers
# ################################
# PlotData$Gender <- factor(rep("Female",length(TimePoints)))
# PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
# PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# PlotData_Takeaway$Gender <- factor(rep("Female", length(TimePoints)))
# 
# 
# #### Group Size 0: 
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_NormalRider_Group0 <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# #### Group Size 1 to 4: 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_NormalRider_Group1To4 <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# #### Group Size 5 and more: 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_NormalRider_Group5AndMore <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# 
# 
# ########################################################################################## For takeaway rider: 
# #### Group Size 0: 
# PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group0 <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# #### Group Size 1 to 4: 
# PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group1To4 <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# #### Group Size 5 and more: 
# PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group5AndMore <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# #######################################################################################################################################
# ############################# MALE drivers
# PlotData$Gender <- factor(rep("Male",length(TimePoints)))
# PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
# PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
# PlotData_Takeaway$Gender <- factor(rep("Male", length(TimePoints)))
# 
# 
# #### Group Size 0: 
# PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_NormalRider_Group0_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# #### Group Size 1 to 4: 
# PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_NormalRider_Group1To4_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# #### Group Size 5 and more: 
# PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_NormalRider_Group5AndMore_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData) )
# 
# 
# 
# ########################################################################################## For takeaway rider: 
# #### Group Size 0: 
# PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group0_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# #### Group Size 1 to 4: 
# PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group1To4_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# #### Group Size 5 and more: 
# PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
# PlotData$hazard_TakeAwayRider_Group5AndMore_MALE <- exp( predict(best_fitting_model_PW, 2, newdata = PlotData_Takeaway) )
# 
# 



############################################################################################################################## USING INDEPENDENCE MODELS: 
################################################################### The grid is BY RIDER TYPE:  FEMALE drivers
################################
best_fitting_model_PW <- WaitingTime_PW_IndepModel

PlotData$Gender <- factor(rep("Female",length(TimePoints)))
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
PlotData_Takeaway$Gender <- factor(rep("Female", length(TimePoints)))


#### Group Size 0: 
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_NormalRider_Group0 <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )

#### Group Size 1 to 4: 
PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_NormalRider_Group1To4 <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )

#### Group Size 5 and more: 
PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_NormalRider_Group5AndMore <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )



########################################################################################## For takeaway rider: 
#### Group Size 0: 
PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group0 <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )

#### Group Size 1 to 4: 
PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group1To4 <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )

#### Group Size 5 and more: 
PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group5AndMore <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )

#######################################################################################################################################
############################# MALE drivers
PlotData$Gender <- factor(rep("Male",length(TimePoints)))
PlotData$RiderType <- factor(rep("Normal", length(TimePoints)))
PlotData_Takeaway$RiderType <- factor(rep("TakeAway", length(TimePoints)))
PlotData_Takeaway$Gender <- factor(rep("Male", length(TimePoints)))


#### Group Size 0: 
PlotData$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_NormalRider_Group0_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )

#### Group Size 1 to 4: 
PlotData$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_NormalRider_Group1To4_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )

#### Group Size 5 and more: 
PlotData$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_NormalRider_Group5AndMore_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData, type = "link") )



########################################################################################## For takeaway rider: 
#### Group Size 0: 
PlotData_Takeaway$GroupSize <- factor(rep("0", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group0_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )

#### Group Size 1 to 4: 
PlotData_Takeaway$GroupSize <- factor(rep("1To4", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group1To4_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )

#### Group Size 5 and more: 
PlotData_Takeaway$GroupSize <- factor(rep("5AndMore", length(TimePoints)))
PlotData$hazard_TakeAwayRider_Group5AndMore_MALE <- exp( predict(best_fitting_model_PW, newdata = PlotData_Takeaway, type = "link") )




#######################################################################################################################################
# COMPUTE SURVIVAL PROBABILITIES: #######################################################################################################################################
#######################################################################################################################################
PlotData$Survival_NormalRyder_Group0 <- exp( - cumsum( PlotData$hazard_NormalRider_Group0 * exp(PlotData$offset) ) )

PlotData$Survival_NormalRyder_Group1To4 <- exp( - cumsum( PlotData$hazard_NormalRider_Group1To4 * exp(PlotData$offset) ) )

PlotData$Survival_NormalRyder_Group5AndMore <- exp( - cumsum( PlotData$hazard_NormalRider_Group5AndMore * exp(PlotData$offset) ) ) 

PlotData$Survival_NormalRyder_Group0_MALE <- exp( - cumsum( PlotData$hazard_NormalRider_Group0_MALE * exp(PlotData$offset) ) )

PlotData$Survival_NormalRyder_Group1To4_MALE <- exp( - cumsum( PlotData$hazard_NormalRider_Group1To4_MALE * exp(PlotData$offset) ) )

PlotData$Survival_NormalRyder_Group5AndMore_MALE <- exp( - cumsum( PlotData$hazard_NormalRider_Group5AndMore_MALE * exp(PlotData$offset) ) )



PlotData$Survival_TakeawayRyder_Group0 <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group0 * exp(PlotData$offset) ) )

PlotData$Survival_TakeawayRyder_Group1To4 <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group1To4 * exp(PlotData$offset) ) )

PlotData$Survival_TakeawayRyder_Group5AndMore <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group5AndMore * exp(PlotData$offset) ) )


PlotData$Survival_TakeawayRyder_Group0_MALE <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group0_MALE* exp(PlotData$offset) ) )

PlotData$Survival_TakeawayRyder_Group1To4_MALE <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group1To4_MALE * exp(PlotData$offset) ) )

PlotData$Survival_TakeawayRyder_Group5AndMore_MALE <- exp( - cumsum( PlotData$hazard_TakeAwayRider_Group5AndMore_MALE * exp(PlotData$offset) ) )



# Stack these datasets:
CS1PLOTDATA_FEMALE <- data.frame(Time = rep(TimePoints, 6),
                                 values = c(PlotData$hazard_NormalRider_Group0, 
                                            PlotData$hazard_NormalRider_Group1To4, 
                                            PlotData$hazard_NormalRider_Group5AndMore,
                                            PlotData$hazard_TakeAwayRider_Group0, 
                                            PlotData$hazard_TakeAwayRider_Group1To4, 
                                            PlotData$hazard_TakeAwayRider_Group5AndMore),
                                 GroupSize = factor(rep(c(rep("0", length(TimePoints)),
                                                          rep("1 to 4", length(TimePoints)),
                                                          rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                 RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)


CS1PLOTDATA_MALE <- data.frame(Time = rep(TimePoints, 6),
                               values = c(PlotData$hazard_NormalRider_Group0_MALE, 
                                          PlotData$hazard_NormalRider_Group1To4_MALE, 
                                          PlotData$hazard_NormalRider_Group5AndMore_MALE,
                                          PlotData$hazard_TakeAwayRider_Group0_MALE, 
                                          PlotData$hazard_TakeAwayRider_Group1To4_MALE,
                                          PlotData$hazard_TakeAwayRider_Group5AndMore_MALE),
                               GroupSize = factor(rep(c(rep("0", length(TimePoints)),
                                                        rep("1 to 4", length(TimePoints)),
                                                        rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                               RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)

CS1PLOTDATA <- rbind(CS1PLOTDATA_FEMALE, CS1PLOTDATA_MALE)

CS1PLOTDATA$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE)),
                               rep("Male", nrow(CS1PLOTDATA_FEMALE))))




#######################
#######################
#######################
CS1PLOTDATA_FEMALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
                                      values = c(PlotData$Survival_NormalRyder_Group0, 
                                                 PlotData$Survival_NormalRyder_Group1To4, 
                                                 PlotData$Survival_NormalRyder_Group5AndMore,
                                                 PlotData$Survival_TakeawayRyder_Group0, 
                                                 PlotData$Survival_TakeawayRyder_Group1To4, 
                                                 PlotData$Survival_TakeawayRyder_Group5AndMore),
                                      GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
                                                               rep("1 to 4", length(TimePoints)), 
                                                               rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                      RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)


CS1PLOTDATA_MALE_SURV <- data.frame(Time = rep(TimePoints, 6), 
                                    values = c(PlotData$Survival_NormalRyder_Group0_MALE, 
                                               PlotData$Survival_NormalRyder_Group1To4_MALE, 
                                               PlotData$Survival_NormalRyder_Group5AndMore_MALE,
                                               PlotData$Survival_TakeawayRyder_Group0_MALE, 
                                               PlotData$Survival_TakeawayRyder_Group1To4_MALE, 
                                               PlotData$Survival_TakeawayRyder_Group5AndMore_MALE),
                                    GroupSize = factor(rep(c(rep("0", length(TimePoints)), 
                                                             rep("1 to 4", length(TimePoints)), 
                                                             rep("5 and more", length(TimePoints))), 2), levels = c("0", "1 to 4", "5 and more"), ordered = TRUE),
                                    RiderType = factor(c(rep("Normal", length(TimePoints)*3), rep("Takeaway", length(TimePoints)*3)))
)

CS1PLOTDATA_SURV <- rbind(CS1PLOTDATA_FEMALE_SURV, CS1PLOTDATA_MALE_SURV)

CS1PLOTDATA_SURV$Gender <- factor(c(rep("Female", nrow(CS1PLOTDATA_FEMALE_SURV)), 
                                    rep("Male", nrow(CS1PLOTDATA_FEMALE_SURV))))


#### TimePoints Real:
TimePoints_Real <- unique(dataset_long_margin2$tend)

CS1PLOTDATA$TimeReal <- rep(TimePoints_Real, 12)
CS1PLOTDATA_SURV$TimeReal <- rep(TimePoints_Real, 12)





hazard_rates_PW <- ggplot(CS1PLOTDATA, aes(TimeReal, values, linetype = Gender, col = GroupSize, fill = GroupSize)) +
  geom_line(linewidth = 0.85) +
  #geom_step(linewidth = 0.85) +
  facet_grid(~ RiderType ) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "darkgreen")) +
  ylab(expression(hat(lambda)(WaitingTime))) +
  xlab(paste(expression(WaitingTime), "(seconds)") ) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c(0, 20, 40, 60, 80, 100, 120)) +
  labs(title = "(b)") + 
  theme_light() +
  theme(legend.position = "",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.title = element_text(size = 27, hjust = 0.5),
        strip.text = element_text(size = 22))




survival_functions_PW <- ggplot(CS1PLOTDATA_SURV, aes(TimeReal, values, linetype = Gender, col = GroupSize, fill = GroupSize)) +
  geom_line(linewidth = 0.85) +
  facet_grid(~ RiderType ) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "darkgreen")) +
  ylab(expression(hat(S)(WaitingTime))) + xlab(paste(expression(WaitingTime), "(seconds)") ) + labs(fill = "Group Size") +
  geom_hline(yintercept = 0.5, col = "red", linetype = "solid", alpha = 0.5) +
  geom_hline(yintercept = 0, col = "black", linetype = "solid", alpha = 0) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120), labels = c(0, 20, 40, 60, 80, 100, 120)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), labels = c(0.0, 0.25, 0.50, 0.75, 1)) + 
  theme_light() +
  guides(color = guide_legend(override.aes = list(size = 5)),
         linetype = guide_legend(override.aes = list(size = 2))) + 
  theme(legend.position = "bottom",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 22),
        legend.key.size = unit(1, "cm"),
        legend.box="vertical", legend.margin=margin())

hazard_rates_PW
survival_functions_PW


# save(hazard_rates_PW, survival_functions_PW, 
#      file ="Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/PW_HazardSurvivalPlots.RData"
# )




load("/Users/briseno-mac/Downloads/Research_Stuff/MXS_Project/BBPAMM/Revival_23/APPLICATIONS/SMJ_Revision_BIKESAPPLICATION_DT/PW_HazardSurvivalPlots.RData")

### Minor modifications: 
hazard_rates_PW <- hazard_rates_PW +
  geom_hline(yintercept = 0, col = "black", linetype = "solid", alpha = 0) +
  scale_y_continuous(breaks = c(0, 0.025, 0.05, 0.075), labels = c(0, 0.025, 0.05, 0.075)) +#, limits = c(0,0.075)) +
  theme(legend.position = "",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 9),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 22))

survival_functions_PW <- survival_functions_PW + 
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 9),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 22),
        legend.key.size = unit(1, "cm"),
        legend.box="vertical", legend.margin=margin())

survival_functions_DT <- survival_functions_DT + theme(axis.text.x = element_text(size = 9), 
                                                       axis.title.y = element_text(size = 18), 
                                                       axis.title.x = element_text(size = 18))
hazard_rates_DT <- hazard_rates_DT + theme(axis.text.x = element_text(size = 9), 
                                           axis.title.x = element_blank(),
                                           axis.title.y = element_text(size = 18))

survival_functions_DT_NOLEGEND <- survival_functions_DT + theme(legend.position = "none")
survival_functions_PW_NOLEGEND <- survival_functions_PW + theme(legend.position = "none")


grid.arrange(hazard_rates_DT, hazard_rates_PW, 
             survival_functions_DT, survival_functions_PW, ncol = 2, nrow = 2, ) 


grid.arrange(hazard_rates_DT, hazard_rates_PW, 
             survival_functions_DT_NOLEGEND, survival_functions_PW_NOLEGEND, ncol = 2, nrow = 2) 



combined_plot <- grid.arrange(hazard_rates_DT, hazard_rates_PW, 
                              survival_functions_DT_NOLEGEND, survival_functions_PW_NOLEGEND, ncol = 2, nrow = 2) 

library("gridExtra") 

get_only_legend <- function(plot) { 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend) 
} 

# extract legend from plot1 using above function 
legend <- get_only_legend(survival_functions_DT)    


# final combined plot with shared legend 
grid.arrange(combined_plot, legend, nrow = 2, heights = c(5, 0.5))




ggpubr::ggarrange(hazard_rates_DT, hazard_rates_PW, survival_functions_DT, survival_functions_PW, byrow = T, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


###########################################################################################################
############################################################################################################ DIAGNOSTIC

# function for residuals and diagnostics!
compute_deviance_resid <- function(original_status, other_residual){
  
  devres <- sign(other_residual) * sqrt( -2 * (other_residual + original_status * log(original_status - other_residual ) ) )
  
  return(devres)
  
}


compute_relevant_residuals <- function(model, data_short, data_long, type, indices, equation_survmodel){
  
  if(type == "DT"){
    
    pred_haz <- 1 - exp( -exp( predict(model, equation_survmodel) ) )
    
    # cox snell residuals
    cxs_resids <- sapply(indices, function(i)  sum( pred_haz[i]  ) ) 
    
    cxs_resids_logoneminus <- sapply(indices, function(i)  sum( - log( 1 - pred_haz[i]) ) ) 
    
    # martingale
    mart_resids <- data_short$status - cxs_resids
    
    mart_resids_logoneminus <- data_short$status - cxs_resids_logoneminus
    
    # deviance residuals
    dv_resids <- compute_deviance_resid(data_short$status, mart_resids)
    
    dv_resids_logoneminus <- compute_deviance_resid(data_short$status, mart_resids_logoneminus)
    
    
    relevant_residuals <- data.frame(cox_snell_residuals = cxs_resids, 
                                     deviance_residuals = dv_resids,
                                     martingale_residuals = mart_resids,
                                     #
                                     #
                                     logoneminus_cox_snell = cxs_resids_logoneminus,
                                     logoneminus_deviance = dv_resids_logoneminus,
                                     logoneminus_martingale = mart_resids_logoneminus
    ) 
    
    # temporal object
    relevant_residuals_temp <- relevant_residuals
    
    relevant_residuals_temp$status <- data_short$status
    
    # Attach the Nelson Aalen estimator of the cox-snell logoneminus residuals
    relevant_residuals$NelsonAalen_coxsnell_logoneminus <- mice::nelsonaalen(relevant_residuals_temp, logoneminus_cox_snell, status)
    
  }
  
  
  if(type == "PW"){
    
    res <- model
    
    # hazard rate
    pred_haz <- exp( predict(res, equation_survmodel) )
    
    # cox snell residuals
    cxs_resids <- sapply(1:length(indices), function(i) sum(  pred_haz[ indices[[i]] ] * exp( data_long$offset[ indices[[i]] ] ) ) )
    
    
    # martingale residuals
    mart_resids <- data_short$status - cxs_resids
    
    # deviance residuals 
    dv_resids <- compute_deviance_resid(data_short$status, mart_resids)
    
    
    relevant_residuals <- data.frame(cox_snell_residuals = cxs_resids, 
                                     deviance_residuals = dv_resids,
                                     martingale_residuals = mart_resids) 
    
    # temporal object
    relevant_residuals_temp <- relevant_residuals
    
    relevant_residuals_temp$status <- data_short$status
    
  }
  
  
  # Attach the Nelson Aalen estimator of the cox-snell residuals
  relevant_residuals$NelsonAalen_coxsnell <- mice::nelsonaalen(relevant_residuals_temp, cox_snell_residuals, status)
  
  
  return(relevant_residuals)
  
}

OriginalData_Short$status <- OriginalData_Short$RedLight

the_residuals <- compute_relevant_residuals(best_fitting_model_PW, 
                                            data_short = OriginalData_Short, 
                                            data_long = dataset_long_margin2, 
                                            type = "PW", 
                                            indices = INDCS_list, 
                                            equation_survmodel = 2)


SomeDataFrame <- data.frame(Residuals = the_residuals$cox_snell_residuals, 
                            Status =  OriginalData_Short$status)



ggplot(the_residuals, aes(cox_snell_residuals, NelsonAalen_coxsnell)) +
  geom_point(size = 2) +
  labs(x = expression(hat(r)^{CS}), y = expression(hat(Lambda)(hat(r)^{CS})), title = "(b)") +
  theme_light() +
  geom_abline(slope = 1, col = "red") +
  theme(legend.position = "right",
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10), 
        plot.title = element_text(hjust = 0.5, size = 30)
  )


