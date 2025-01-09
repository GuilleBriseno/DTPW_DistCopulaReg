

library("GJRM", lib.loc = "/Users/briseno-mac/Downloads/Research_Stuff/Modded_R_Packages/MBS_Install/MBS_FIXEDHESS/MBS_GJRM/")

library(parallel)
library(pbapply)



# create data:
source("SIMULATIONS/Data_generation/SIM_BINSURV_VARDEPENDENCE.R")
source("SIMULATIONS/Fits/SIM_FIT.R")
source("SIMULATIONS/Fits/SIM_FIT_PW.R")



# fit on that data:
# This will fit always using the same model specification and sample sizes
# n1 =  750, and n2 =  1500

n1 <- 750

n2 <- 1500


# monte carlo replications:
R <- 100

# number of intervals:
number_of_ints_n1 <- 20


n.sims <- 1000



# Pre-specify the copula classes here:
# 1 = GAUSS = N, 
# 5 = FRANK = F
# 4 = GUMBEL = G0, (24 rotated G90),
# 3 = CLAYTON = C0, (23 rotated C90),
copula_class <- "G0"

copula_type <- 4



## MILD CENSORING
set.seed(1)
data_objects_n1 <- replicate(n = R,
                             create_data_binsurv(n = n1, binary_margin = "logit", copulatype = copula_type, dependence_predictor = "not fixed"),
                             simplify = FALSE)



maximum_time_cut <- 4.1

continuous_margin <- "N"


# Fit on these R datasets a copula mixed responses model for binary and time-to-event margins:
# Using discrete time approach:
DT_results_n1 <- mclapply(data_objects_n1,
                          function(i)
                            fit_MXS(i, 
                                    NonSurv_Margin = "logit",
                                    copula_class = copula_class,
                                    baseline_smooth = "not log", 
                                    maximum_time_cut = maximum_time_cut,
                                    varying_dependence = TRUE,
                                    n.sim = n.sims,
                                    number_of_ints = number_of_ints_n1), 
                          mc.set.seed = TRUE, 
                          mc.cores = getOption("mc.cores", 1L) 
)



# Using piecewise exponential approach:
PW_results_n1 <- mclapply(data_objects_n1,
                          function(i)
                            fit_MXS_PWE(i, 
                                        NonSurv_Margin = "logit",
                                        copula_class = copula_class,
                                        baseline_smooth = "not log", 
                                        maximum_time_cut = maximum_time_cut,
                                        varying_dependence = TRUE,
                                        n.sim = n.sims,
                                        number_of_ints = number_of_ints_n1), 
                          mc.set.seed = TRUE, 
                          mc.cores = getOption("mc.cores", 1L) 
)











### HEAVY CENSORING
set.seed(1)
data_objects_n1 <- replicate(n = R,
                             create_data_binsurv_heavycensoring(n = n1, binary_margin = "logit", copulatype = copula_type, dependence_predictor = "not fixed"),
                             simplify = FALSE)



maximum_time_cut <- 7


# Fit on these R datasets a copula mixed responses model for binary and time-to-event margins:
# Using discrete time approach:
DT_results_n1 <- mclapply(data_objects_n1,
                          function(i)
                            fit_MXS(i, 
                                    NonSurv_Margin = "logit",
                                    copula_class = copula_class,
                                    baseline_smooth = "not log", 
                                    maximum_time_cut = maximum_time_cut,
                                    varying_dependence = TRUE,
                                    n.sim = n.sims,
                                    number_of_ints = number_of_ints_n1), 
                          mc.set.seed = TRUE, 
                          mc.cores = getOption("mc.cores", 1L) 
)


# Using piecewise exponential approach:
PW_results_n1 <- mclapply(data_objects_n1,
                          function(i)
                            fit_MXS_PWE(i, 
                                        NonSurv_Margin = "logit",
                                        copula_class = copula_class,
                                        baseline_smooth = "not log", 
                                        maximum_time_cut = maximum_time_cut,
                                        varying_dependence = TRUE,
                                        n.sim = n.sims,
                                        number_of_ints = number_of_ints_n1), 
                          mc.set.seed = TRUE, 
                          mc.cores = getOption("mc.cores", 1L) 
)




