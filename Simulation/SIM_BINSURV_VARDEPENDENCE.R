library(tidyverse)
library(copula)
library(VineCopula)
library(pammtools)
library(discSurv)

# This function is to extract a piece-wise exponential 
# hazard rate used in the sampling process (no longer used)
sim_PAM_rate <- function(formula, data, cut, copulatype){
  
  dat <- data
  
  dat <- dat %>% dplyr:::mutate(id = row_number(), time = max(cut), 
                                status = 1)
  
  
  Form <- Formula:::Formula(formula)
  
  f1 <- formula(Form, rhs = 1)
  
  if (length(Form)[2] > 1) {
    f2 <- formula(Form, rhs = 2)
  }else {
    f2 <- NULL
  }
  
  # Create the dataset with covariates and 
  # HAZARD RATE as well as log_hazard rate
  ped <- pammtools:::split_data(formula = Surv(time, status) ~ ., 
                                data = select_if(dat, is_atomic), 
                                cut = cut, 
                                id = "id") %>% 
    dplyr:::rename(t = "tstart") %>% 
    mutate(rate = exp(lazyeval:::f_eval(f1, .)),
           log_rate = lazyeval:::f_eval(f1, .))
  
  
  # Create final version of the dataset (NOT USED HERE!)
  # sim_df <- ped %>%
  #   group_by(id) %>%
  #   summarize(time = msm:::rpexp(rate = .data$rate, 
  #                                t = .data$t)#,
  #             #rate = .data$rate,
  #             #log_rate = .data$log_rate
  #             ) %>%
  #   mutate(status = 1L * (.data$time <= max(cut)),
  #          time = pmin(.data$time, max(cut)))
  # 
  # suppressMessages(sim_df <- sim_df %>%
  #                    left_join(select(data,
  #                                     -.data$time,
  #                                     -.data$status)))
  
  return(as.data.frame(ped))
  
}


# This function returns a dataset with covariates (DGP is fixed for now)
# and the marginal responses: continuous (y1 in R ) x survival(t, status)

# This version of the function generates data based on the SURVIVAL FUNCTION:
create_data_binsurv <- function(n, 
                                dependence = "mild", 
                                dependence_direction = "positive", 
                                binary_margin = "logit",
                                copulatype = 1, 
                                censoring_type = "random",
                                censoring_rate = "mild",
                                censoring_cut = 4,
                                dependence_predictor = "fixed"
){
  
  # sample size
  n <- n
  
  # Generate covariates, independent:
  x1 <- runif(n, -1, +1)
  x2 <- runif(n, -1, +1)
  x3 <- runif(n, -1, +1)
  x4 <- runif(n, -1, +1)
  x5 <- runif(n, -1, +1)
  
  
  ############################################################
  # STEP 1: Obtain PARAMETER THETA 1 FOR MARGIN 1
  beta_1 <- c(0, -0.75, 1, 1.5)
  
  # smooth function on additive predictor OF MARGIN 1
  f1 <- function(x){
    
    
    eff <- 1*sin(pi* x)
    
    return(eff)
    
  }
  
  eta1 <- beta_1[1] + beta_1[2]*x1 + beta_1[3]*x2 
  
  # Distribution parameter of non-survival margin:
  if(binary_margin == "logit"){ binary_responsefunction <- plogis }
  if(binary_margin == "probit"){ binary_responsefunction <- pnorm  }
  if(binary_margin == "cloglog"){ binary_responsefunction <- function(eta){ 1 - exp(- exp( eta) ) } }
  
  theta1 <- binary_responsefunction(eta1)
  
  
  # true spline in binary margin:
  xsynth <- seq(-1, +1, length.out = 100)
  
  
  true_spline_margin1 <- f1(xsynth)
  true_spline_margin1_centered <- f1(xsynth) - mean(f1(xsynth))
  
  
  ############################################################
  # GENERATE UNIFORM RANDOM NUMBERS U1, U2, FROM COPULA:
  
  # check how the additive predictor would look like:
  if(!(dependence_predictor == "fixed") ){
    
    if(copulatype == 1){
    
    eta3 <-  1.75*x5
    
    theta3 <- tanh(eta3)
    
    original_tau <- BiCopPar2Tau(family = copulatype, theta3)
    
    original_tau_range <- range(original_tau)
    
    beta_copula_param <- 1.75
    
    }
    
    # GUMBEL
    if(copulatype == 4){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3) + 1
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    # GUMBEL NEGATIVE
    if( (copulatype == 4) && (dependence_direction == "negative") ){
      
      eta3 <-  + 1.75*x5 
      
      theta3 <- - ( exp(eta3) + 1 )
      
      original_tau <- BiCopPar2Tau(family = 24, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    # CLAYTON
    if(copulatype == 3){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    
    # CLAYTON NEGATIVE
    if( (copulatype == 3) && (dependence_direction == "negative") ){
      
      eta3 <-  1.75*x5
      
      theta3 <- -exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = 23, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    
    # FRANK
    if( copulatype == 5 ){
      
      eta3 <-  12*x5
      
      theta3 <- eta3
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 12
      
    }
    
    
  }else{
  
  
  # CONSTRUCT COPULA PARAMETER
  if(dependence_direction == "positive"){
    
    if(dependence == "independence"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0)
      
      original_tau <- 0
    }
    
    if(dependence == "weak"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.15)
      
      original_tau <- 0.15
    }
    
    if(dependence == "mild"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.25)
      
      original_tau <- 0.25
    }
    
    if(dependence == "strong"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.6)
      
      original_tau <- 0.6
    }
    
  }else{
    
    if(!(copulatype == 3 || copulatype == 4 || copulatype == 6) ){
      
      if(dependence == "independence"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0)
        
        original_tau <- 0
      }
      
      if(dependence == "weak"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.15)
        
        original_tau <- -0.15
      }
      
      if(dependence == "mild"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.25)
        
        original_tau <- -0.25
      }
      
      if(dependence == "strong"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.6)
        
        original_tau <- -0.6
      }
      
    }else{
      
      if(copulatype == 3){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(copulatype, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(23, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(23, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(23, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      if(copulatype == 4){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(24, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(24, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(24, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(24, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      if(copulatype == 6){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(26, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(26, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(26, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(26, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      
      
    }  
    
  }
  
  }
    
  
  # determine the copula based on the argument "copulatype"
  if(copulatype == 1){
   
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 3 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 3 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 23, par = theta3)
    
  }
  
  if(copulatype == 4 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 4 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 24, par = theta3)
    
  }
  
  if(copulatype == 5){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 6 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 6 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 26, par = theta3)
    
  }
  
  
  
  ###############################################################
  # Obtain the marginal responses from arbitrary distributinos
  # survival function of survival margin:
  f1 <- function(t1, beta_0, beta_1, u, z1, z2){ 
    
    S_0 <- 0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1)
    
    exp(-exp(log(-log(S_0)) #))-u 
             + beta_0*z1 + beta_1*z2)) - u
    
  }
  
  
  # Retrieve the true SURVIVAL FUNCTION, HAZARD RATE, ETC
  f0 <- function(t1){
    
    0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1)
    
  }
  
  # artificial time values
  time_synth <- seq(0, 8, length.out = 100)
  
  # true function
  true_survival_function <-  f0(time_synth)
  
  # true cumulative hazard function: 
  true_cumuhazard_function <- -log(f0(time_synth))
  
  # true baseline hazard function:
  
  Lambda_0 <- function(t1){
    
    -log( 0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1) )  
    
  }
  
  
  times <- seq(0.0001, 8, length.out = n)
  
  hazards <- rep(NA,n)
  
  
  for(i in 1:n){
    
    hazards[i] <- numDeriv::grad(Lambda_0, x = times[i])
    
  }
  
  true_baselinehazard <- hazards
  true_logbaselinehazard <- log(hazards)
  true_logbaseline_centered <- true_logbaselinehazard - mean(true_logbaselinehazard)
  
  
  
  # coefficients for survival margin:
 
  beta_01 <- -1.5
  beta_11 <- -1
  
  ###############################################################
  # SAMPLE THE MARGINAL RESPONSES
  
  # y1 ~ Bernoulli
  y1 <- qbinom(copula_samples[,1], prob = theta1, size = 1)
  
  ## initialize times:
  t1 <- rep(NA, n)
  
  
  # SAMPLE TRUE SURVIVAL TIMES (CENSORING HAPPENS BELOW)
  for (i in 1:n){
    
    t1[i] <- uniroot(f1, c(0, 8), 
                     tol = .Machine$double.eps^0.5, 
                     beta_0 = beta_01, 
                     beta_1 = beta_11, 
                     u = copula_samples[i,2], 
                     z1 = x2[i],
                     z2 = x4[i], 
                     extendInt = "yes" )$root
  }
  
  
  survival_times <- t1
  
  
  if(censoring_type == "none"){
    
    status <- rep(1, length(survival_times))
    
    observed_time <- survival_times
    
    
  }
  
  if(censoring_type == "fixed"){
    
    # RIGHT CENSORING AND CREATION OF CENSORING STATUS:
    # censoring status
    status <- 1L * (survival_times <= censoring_cut )
    
    # survival response!  min( true_survival_time, Censoring_time)
    observed_time <- pmin(survival_times, censoring_cut )
    
  }
  
  if(censoring_type == "random"){
    
    
    if(censoring_rate == "heavy"){
      
      random_censtimes <- runif(n, 0.5, 1.25)  
      
    }else{
      
      
      random_censtimes <- runif(n, 1.6, 4)  
      
    }
    
    
    
    # censoring indicators:
    status  <- ifelse(survival_times < random_censtimes, 1, 0)
    
    
    # # RIGHT CENSORING AND CREATION OF CENSORING STATUS:
    # # censoring status
    # status <- 1L * (survival_times <= random_censtimes )
    # survival response!  min( true_survival_time, Censoring_time)
    observed_time <- pmin(survival_times, random_censtimes)
    
  }
  
  ###############################################################
  # STEP 5:
  # PUT EVERYTHING TOGETHER:
  dat <- data.frame(y1 = y1,
                    time = observed_time,
                    status = status,
                    x1 = x1,
                    x2 = x2,
                    x3 = x3,
                    x4 = x4,
                    x5 = x5)
  
  
  # true coefficients: 
  true_coefficients <- list(non_surv = c(beta_1[1], beta_1[2], beta_1[3]),
                            surv = c(beta_01, beta_11),
                            copula_param = c(0, beta_copula_param)
                            )
  
  
  
  # return a bit more than the dataset (!)
  output <- list(dataset = dat,
                 mean_y1 = mean(dat$y1),
                 original_tau = original_tau,
                 true_copula_param = theta3,
                 type_copula = copulatype, 
                 #
                 true_coefficients = true_coefficients,
                 #
                 censoring_rate = table(status),
                 censoring_rate_percent = table(status)/n,
                 true_survival_function = true_survival_function,
                 true_cumuhazard_function = true_cumuhazard_function, 
                 true_baselinehazard = true_baselinehazard,
                 true_log_baselinehazard = true_logbaselinehazard,
                 true_log_baselinehazard_centered = true_logbaseline_centered,
                 true_spline_margin1 = true_spline_margin1,
                 true_spline_margin1_centered = true_spline_margin1_centered
  )
  
  
  
  
  
  return(output)
  
}





#testobject <- create_data_binsurv(n = 1000, dependence_predictor = "variable", copulatype = 3, dependence_direction = "negative")

create_data_binsurv_heavycensoring <- function(n, 
                                               dependence = "mild", 
                                               dependence_direction = "positive", 
                                               binary_margin = "logit",
                                               copulatype = 1, 
                                               censoring_type = "random",
                                               censoring_cut = 4,
                                               dependence_predictor = "fixed"
){
  
  # sample size
  n <- n
  
  # Generate covariates, independent:
  x1 <- runif(n, -1, +1)
  x2 <- runif(n, -1, +1)
  x3 <- runif(n, -1, +1)
  x4 <- runif(n, -1, +1)
  x5 <- runif(n, -1, +1)
  
  
  ############################################################
  # STEP 1: Obtain PARAMETER THETA 1 FOR MARGIN 1
  beta_1 <- c(0, -0.75, 1, 1.5)
  
  # smooth function on additive predictor OF MARGIN 1
  f1 <- function(x){
    
    
    eff <- 1*sin(pi* x)
    
    return(eff)
    
  }
  
  eta1 <- beta_1[1] + beta_1[2]*x1 + beta_1[3]*x2 
  
  # Distribution parameter of non-survival margin:
  if(binary_margin == "logit"){ binary_responsefunction <- plogis }
  if(binary_margin == "probit"){ binary_responsefunction <- pnorm  }
  if(binary_margin == "cloglog"){ binary_responsefunction <- function(eta){ 1 - exp(- exp( eta) ) } }
  
  theta1 <- binary_responsefunction(eta1)
  
  
  # true spline in binary margin:
  xsynth <- seq(-1, +1, length.out = 100)
  
  
  true_spline_margin1 <- f1(xsynth)
  true_spline_margin1_centered <- f1(xsynth) - mean(f1(xsynth))
  
  
  ############################################################
  # GENERATE UNIFORM RANDOM NUMBERS U1, U2, FROM COPULA:
  
  # check how the additive predictor would look like:
  if(!(dependence_predictor == "fixed") ){
    
    if(copulatype == 1){
      
      eta3 <-  1.75*x5
      
      theta3 <- tanh(eta3)
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    # GUMBEL
    if(copulatype == 4){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3) + 1
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    # GUMBEL NEGATIVE
    if( (copulatype == 4) && (dependence_direction == "negative") ){
      
      eta3 <-  + 1.75*x5 
      
      theta3 <- - ( exp(eta3) + 1 )
      
      original_tau <- BiCopPar2Tau(family = 24, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    # CLAYTON
    if(copulatype == 3){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    
    # CLAYTON NEGATIVE
    if( (copulatype == 3) && (dependence_direction == "negative") ){
      
      eta3 <-  1.75*x5
      
      theta3 <- -exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = 23, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 1.75
      
    }
    
    
    # FRANK
    if( copulatype == 5 ){
      
      eta3 <-  12*x5
      
      theta3 <- eta3
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
      beta_copula_param <- 12
      
    }
    
    
  }else{
    
    
    # CONSTRUCT COPULA PARAMETER
    if(dependence_direction == "positive"){
      
      if(dependence == "independence"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0)
        
        original_tau <- 0
      }
      
      if(dependence == "weak"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0.15)
        
        original_tau <- 0.15
      }
      
      if(dependence == "mild"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0.25)
        
        original_tau <- 0.25
      }
      
      if(dependence == "strong"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0.6)
        
        original_tau <- 0.6
      }
      
    }else{
      
      if(!(copulatype == 3 || copulatype == 4 || copulatype == 6) ){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(copulatype, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(copulatype, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(copulatype, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(copulatype, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }else{
        
        if(copulatype == 3){
          
          if(dependence == "independence"){
            theta3 <- BiCopTau2Par(copulatype, tau = 0)
            
            original_tau <- 0
          }
          
          if(dependence == "weak"){
            theta3 <- BiCopTau2Par(23, tau = -0.15)
            
            original_tau <- -0.15
          }
          
          if(dependence == "mild"){
            theta3 <- BiCopTau2Par(23, tau = -0.25)
            
            original_tau <- -0.25
          }
          
          if(dependence == "strong"){
            theta3 <- BiCopTau2Par(23, tau = -0.6)
            
            original_tau <- -0.6
          }
          
        }
        if(copulatype == 4){
          
          if(dependence == "independence"){
            theta3 <- BiCopTau2Par(24, tau = 0)
            
            original_tau <- 0
          }
          
          if(dependence == "weak"){
            theta3 <- BiCopTau2Par(24, tau = -0.15)
            
            original_tau <- -0.15
          }
          
          if(dependence == "mild"){
            theta3 <- BiCopTau2Par(24, tau = -0.25)
            
            original_tau <- -0.25
          }
          
          if(dependence == "strong"){
            theta3 <- BiCopTau2Par(24, tau = -0.6)
            
            original_tau <- -0.6
          }
          
        }
        if(copulatype == 6){
          
          if(dependence == "independence"){
            theta3 <- BiCopTau2Par(26, tau = 0)
            
            original_tau <- 0
          }
          
          if(dependence == "weak"){
            theta3 <- BiCopTau2Par(26, tau = -0.15)
            
            original_tau <- -0.15
          }
          
          if(dependence == "mild"){
            theta3 <- BiCopTau2Par(26, tau = -0.25)
            
            original_tau <- -0.25
          }
          
          if(dependence == "strong"){
            theta3 <- BiCopTau2Par(26, tau = -0.6)
            
            original_tau <- -0.6
          }
          
        }
        
        
      }  
      
    }
    
  }
  
  
  # determine the copula based on the argument "copulatype"
  if(copulatype == 1){
    
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 3 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 3 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 23, par = theta3)
    
  }
  
  if(copulatype == 4 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 4 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 24, par = theta3)
    
  }
  
  if(copulatype == 5){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 6 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopSim(n, copulatype, par = theta3)
    
  }
  
  if(copulatype == 6 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopSim(n, family = 26, par = theta3)
    
  }
  
  
  
  ###############################################################
  # Obtain the marginal responses from arbitrary distributinos
  # survival function of survival margin:
  f1 <- function(t1, beta_0, beta_1, u, z1, z2){ 
    
    S_0 <- 0.4 * exp(-0.04*t1^2.5) + 0.6 * exp(-0.025*t1)
    
    exp(-exp(log(-log(S_0)) #))-u 
             + beta_0*z1 + beta_1*z2)) - u
    
  }
  
  
  # Retrieve the true SURVIVAL FUNCTION, HAZARD RATE, ETC
  f0 <- function(t1){
    
    0.4 * exp(-0.04*t1^2.5) + 0.6 * exp(-0.025*t1)
    
  }
  
  # artificial time values
  time_synth <- seq(0, 8, length.out = 100)
  
  # true function
  true_survival_function <-  f0(time_synth)
  
  # true cumulative hazard function: 
  true_cumuhazard_function <- -log(f0(time_synth))
  
  # true baseline hazard function:
  
  Lambda_0 <- function(t1){
    
    -log(  0.4 * exp(-0.04*t1^2.5) + 0.6 * exp(-0.025*t1) )
    
  }
  
  
  times <- seq(0.0001, 8, length.out = n)
  
  hazards <- rep(NA,n)
  
  
  for(i in 1:n){
    
    hazards[i] <- numDeriv::grad(Lambda_0, x = times[i])
    
  }
  
  true_baselinehazard <- hazards
  true_logbaselinehazard <- log(hazards)
  true_logbaseline_centered <- true_logbaselinehazard - mean(true_logbaselinehazard)
  
  
  
  # coefficients for survival margin:
  beta_01 <- -1.5
  beta_11 <- -1
  
  ###############################################################
  # SAMPLE THE MARGINAL RESPONSES
  
  # y1 ~ Bernoulli
  y1 <- qbinom(copula_samples[,1], prob = theta1, size = 1)
  
  ## initialize times:
  t1 <- rep(NA, n)
  
  
  # SAMPLE TRUE SURVIVAL TIMES (CENSORING HAPPENS BELOW)
  for (i in 1:n){
    
    t1[i] <- uniroot(f1, c(0, 8), 
                     tol = .Machine$double.eps^0.5, 
                     beta_0 = beta_01, 
                     beta_1 = beta_11, 
                     u = copula_samples[i,2], 
                     z1 = x2[i],
                     z2 = x4[i], 
                     extendInt = "yes" )$root
  }
  
  
  survival_times <- t1
  
  
  if(censoring_type == "none"){
    
    status <- rep(1, length(survival_times))
    
    observed_time <- survival_times
    
    
  }
  
  if(censoring_type == "fixed"){
    
    # RIGHT CENSORING AND CREATION OF CENSORING STATUS:
    # censoring status
    status <- 1L * (survival_times <= censoring_cut )
    
    # survival response!  min( true_survival_time, Censoring_time)
    observed_time <- pmin(survival_times, censoring_cut )
    
  }
  
  if(censoring_type == "random"){
    
    
    random_censtimes <- runif(n, 0, 6.75)  
    
    
    
    
    # censoring indicators:
    status  <- ifelse(survival_times < random_censtimes, 1, 0)
    
    table(status)/n
    
    # # RIGHT CENSORING AND CREATION OF CENSORING STATUS:
    # # censoring status
    # status <- 1L * (survival_times <= random_censtimes )
    # survival response!  min( true_survival_time, Censoring_time)
    observed_time <- pmin(survival_times, random_censtimes)
    
  }
  
  ###############################################################
  # STEP 5:
  # PUT EVERYTHING TOGETHER:
  dat <- data.frame(y1 = y1,
                    time = observed_time,
                    status = status,
                    x1 = x1,
                    x2 = x2,
                    x3 = x3,
                    x4 = x4,
                    x5 = x5)
  
  
  # true coefficients: 
  true_coefficients <- list(non_surv = c(beta_1[1], beta_1[2], beta_1[3]),
                            surv = c(beta_01, beta_11),
                            copula_param = c(0, beta_copula_param)
  )
  
  
  
  # return a bit more than the dataset (!)
  output <- list(dataset = dat,
                 mean_y1 = mean(dat$y1),
                 original_tau = original_tau,
                 true_copula_param = theta3,
                 type_copula = copulatype, 
                 #
                 true_coefficients = true_coefficients,
                 #
                 censoring_rate = table(status),
                 censoring_rate_percent = table(status)/n,
                 true_survival_function = true_survival_function,
                 true_cumuhazard_function = true_cumuhazard_function, 
                 true_baselinehazard = true_baselinehazard,
                 true_log_baselinehazard = true_logbaselinehazard,
                 true_log_baselinehazard_centered = true_logbaseline_centered,
                 true_spline_margin1 = true_spline_margin1,
                 true_spline_margin1_centered = true_spline_margin1_centered
  )
  
  
  
  
  
  return(output)
  
}




# This function returns a dataset with covariates (DGP is fixed for now)
# and the marginal responses: continuous (y1 in R ) x survival(t, status)

# This version of the function generates data based on the SURVIVAL FUNCTION:
create_data_binsurv_recursive <- function(n, 
                                          dependence = "mild", 
                                          dependence_direction = "positive", 
                                          binary_margin = "logit",
                                          copulatype = 1, 
                                          censoring_type = "random",
                                          censoring_rate = "mild",
                                          censoring_cut = 4,
                                          dependence_predictor = "fixed"){
  
  # sample size
  n <- n
  
  # Generate covariates, independent:
  x1 <- runif(n, -1, +1)
  x2 <- runif(n, -1, +1)
  x3 <- runif(n, -1, +1)
  x4 <- runif(n, -1, +1)
  x5 <- runif(n, -1, +1)
  
  
  ############################################################
  # STEP 1: Obtain PARAMETER THETA 1 FOR MARGIN 1
  beta_1 <- c(0, -0.75, 1, 1.5)
  
  # smooth function on additive predictor OF MARGIN 1
  f1 <- function(x){
    
    
    eff <- 1*sin(pi* x)
    
    return(eff)
    
  }
  
  
  eta1 <- beta_1[1] + beta_1[2]*x1 + beta_1[3]*x2 
  
  # Distribution parameter of non-survival margin:
  if(binary_margin == "logit"){ binary_responsefunction <- plogis }
  if(binary_margin == "probit"){ binary_responsefunction <- pnorm  }
  if(binary_margin == "cloglog"){ binary_responsefunction <- function(eta){ 1 - exp(- exp( eta) ) } }
  
  theta1 <- binary_responsefunction(eta1)
  
  
  # true spline in binary margin:
  xsynth <- seq(-1, +1, length.out = 100)
  
  
  true_spline_margin1 <- f1(xsynth)
  true_spline_margin1_centered <- f1(xsynth) - mean(f1(xsynth))
  
  
  ############################################################
  # GENERATE UNIFORM RANDOM NUMBERS U1 
  u1 <- runif(n)
  
  # Obtain y1 
  y1 <- qbinom(u1, size = 1, prob = theta1)
  
  
  
  if(!(dependence_predictor == "fixed") ){
    
    if(copulatype == 1){
      
      eta3 <-  1.75*x5
      
      theta3 <- tanh(eta3)
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    # GUMBEL
    if(copulatype == 4){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3) + 1
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    # GUMBEL NEGATIVE
    if( (copulatype == 4) && (dependence_direction == "negative") ){
      
      eta3 <-  + 1.75*x5 
      
      theta3 <- - ( exp(eta3) + 1 )
      
      original_tau <- BiCopPar2Tau(family = 24, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    # CLAYTON
    if(copulatype == 3){
      
      
      eta3 <-  + 1.75*x5
      
      theta3 <- exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    
    # CLAYTON NEGATIVE
    if( (copulatype == 3) && (dependence_direction == "negative") ){
      
      eta3 <-  1.75*x5
      
      theta3 <- -exp(eta3)
      
      original_tau <- BiCopPar2Tau(family = 23, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    
    # FRANK
    if( copulatype == 5 ){
      
      eta3 <-  12*x5
      
      theta3 <- eta3
      
      original_tau <- BiCopPar2Tau(family = copulatype, theta3)
      
      original_tau_range <- range(original_tau)
      
    }
    
    
  }else{
  
  # CONSTRUCT COPULA PARAMETER
  if(dependence_direction == "positive"){
    
    if(dependence == "independence"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0)
      
      original_tau <- 0
    }
    
    if(dependence == "weak"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.15)
      
      original_tau <- 0.15
    }
    
    if(dependence == "mild"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.25)
      
      original_tau <- 0.25
    }
    
    if(dependence == "strong"){
      theta3 <- BiCopTau2Par(copulatype, tau = 0.6)
      
      original_tau <- 0.6
    }
    
  }else{
    
    if(!(copulatype == 3 || copulatype == 4 || copulatype == 6) ){
      
      if(dependence == "independence"){
        theta3 <- BiCopTau2Par(copulatype, tau = 0)
        
        original_tau <- 0
      }
      
      if(dependence == "weak"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.15)
        
        original_tau <- -0.15
      }
      
      if(dependence == "mild"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.25)
        
        original_tau <- -0.25
      }
      
      if(dependence == "strong"){
        theta3 <- BiCopTau2Par(copulatype, tau = -0.6)
        
        original_tau <- -0.6
      }
      
    }else{
      
      if(copulatype == 3){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(copulatype, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(23, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(23, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(23, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      if(copulatype == 4){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(24, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(24, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(24, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(24, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      if(copulatype == 6){
        
        if(dependence == "independence"){
          theta3 <- BiCopTau2Par(26, tau = 0)
          
          original_tau <- 0
        }
        
        if(dependence == "weak"){
          theta3 <- BiCopTau2Par(26, tau = -0.15)
          
          original_tau <- -0.15
        }
        
        if(dependence == "mild"){
          theta3 <- BiCopTau2Par(26, tau = -0.25)
          
          original_tau <- -0.25
        }
        
        if(dependence == "strong"){
          theta3 <- BiCopTau2Par(26, tau = -0.6)
          
          original_tau <- -0.6
        }
        
      }
      
      
    }  
    
  }
    
  }
  

  
  if(copulatype == 1){
    
   
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = copulatype, par = theta3)
    
  }
  
  
  if(copulatype == 3 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = copulatype, par = theta3)
    
  }
  
  if(copulatype == 3 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = 23, par = theta3)
    
  }
  
  
  if(copulatype == 4 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = copulatype, par = theta3)
    
  }
  
  if(copulatype == 4 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = 24, par = theta3)
    
  }
  
  if(copulatype == 5){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = copulatype, par = theta3)
    
  }
  
  
  if(copulatype == 6 && !(dependence_direction == "negative") ){
    
    # SAMPLE U1, U2 FROM CONSTRUCTED COPULA
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = copulatype, par = theta3)
    
  }
  
  if(copulatype == 6 && (dependence_direction == "negative") ){
    
    copula_samples <- BiCopCondSim(n, cond.val = u1, cond.var = 1,  
                                   family = 26, par = theta3)
    
  }
  
  
  
  ###############################################################
  # Obtain the marginal responses from arbitrary distributinos
  # survival function of survival margin:
  
  if(censoring_rate == "mild"){
  
  f1 <- function(t1, beta_0, beta_1, beta_2, beta_00, u, z1, z2, z3){ 
    
    S_0 <- 0.9 * exp(-0.06*t1^2.5) + 0.1*exp(-0.01*t1)
    
    beta_00 + exp(-exp(log(-log(S_0))  
             + beta_0*z1 + 
               beta_1*z2 + 
               beta_2*z3)) - u
    
  }
  
  
  # Retrieve the true SURVIVAL FUNCTION, HAZARD RATE, ETC
  f0 <- function(t1){
    
    
    0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1)
    
  }
  
  # artificial time values
  time_synth <- seq(0, 8, length.out = 100)
  
  # true function
  true_survival_function <-  f0(time_synth)
  
  # true cumulative hazard function: 
  true_cumuhazard_function <- -log(f0(time_synth))
  
  # true baseline hazard function:
  
  Lambda_0 <- function(t1){
    
    
    -log( 0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1) )  
    
  }
  
  
  times <- seq(0.0001, 8, length.out = n)
  
  hazards <- rep(NA,n)
  
  
  for(i in 1:n){
    
    hazards[i] <- numDeriv::grad(Lambda_0, x = times[i])
    
  }
  
  }
  
  if(censoring_rate == "heavy"){
    
    
    f1 <- function(t1, beta_0, beta_1, beta_2, beta_00, u, z1, z2, z3){ 
      
      
      S_0 <- 0.4 * exp(-0.04*t1^2.5) + 0.6 * exp(-0.025*t1)
      
      beta_00 + exp(-exp(log(-log(S_0)) 
                         + beta_0*z1 + 
                           beta_1*z2 + 
                           beta_2*z3)) - u
      
    }
    
    
    # Retrieve the true SURVIVAL FUNCTION, HAZARD RATE, ETC
    f0 <- function(t1){
      
      0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1)
      
    }
    
    # artificial time values
    time_synth <- seq(0, 8, length.out = 100)
    
    # true function
    true_survival_function <-  f0(time_synth)
    
    # true cumulative hazard function: 
    true_cumuhazard_function <- -log(f0(time_synth))
    
    # true baseline hazard function:
    
    Lambda_0 <- function(t1){
      
      -log( 0.9 * exp(-0.4*t1^2.5) + 0.1*exp(-0.1*t1) )  
      
    }
    
    
    times <- seq(0.0001, 8, length.out = n)
    
    hazards <- rep(NA,n)
    
    
    for(i in 1:n){
      
      hazards[i] <- numDeriv::grad(Lambda_0, x = times[i])
      
    }
    
  }
  
  true_baselinehazard <- hazards
  true_logbaselinehazard <- log(hazards)
  true_logbaseline_centered <- true_logbaselinehazard - mean(true_logbaselinehazard)
  
  
  
  # coefficients for survival margin:
  beta_00 <- 5
  
  beta_01 <- +1
  beta_11 <- +.5
  beta_12 <- -0.5
  
  ###############################################################
  ## initialize times:
  t1 <- rep(NA, n)
  
  
  # SAMPLE TRUE SURVIVAL TIMES (CENSORING HAPPENS BELOW)
  for (i in 1:n){
    
    t1[i] <- uniroot(f1, c(0, 8), 
                     tol = .Machine$double.eps^0.5, 
                     beta_0 = beta_01, 
                     beta_1 = beta_11, 
                     beta_2 = beta_12,
                     beta_00 = 0,
                     u = copula_samples[i], 
                     z1 = x2[i],
                     z2 = x4[i], 
                     z3 = y1[i],
                     extendInt = "yes" )$root
  }
  
  
  survival_times <- t1
  
  
  if(censoring_type == "none"){
    
    status <- rep(1, length(survival_times))
    
    observed_time <- survival_times
    
    
  }
  
  if(censoring_type == "fixed"){
    
    # RIGHT CENSORING AND CREATION OF CENSORING STATUS:
    # censoring status
    status <- 1L * (survival_times <= censoring_cut )
    
    # survival response!  min( true_survival_time, Censoring_time)
    observed_time <- pmin(survival_times, censoring_cut )
    
  }
  
  if(censoring_type == "random"){
    
    
    if(censoring_rate == "heavy"){
      
      #random_censtimes <- runif(n, 0, 4.4)  
      random_censtimes <- runif(n, 0, 10)
      
      # censoring indicators:
      status  <- ifelse(survival_times < random_censtimes, 1, 0)
      
    
      observed_time <- pmin(survival_times, random_censtimes)
      
      
    }else{
      
      random_censtimes <- runif(n, 5.5, 6)  

      # censoring indicators:
      status  <- ifelse(survival_times < random_censtimes, 1, 0)
      
      observed_time <- pmin(survival_times, random_censtimes)
      
    }
    
    
    
   
    
  }
  
  ###############################################################
  # STEP 5:
  # PUT EVERYTHING TOGETHER: Marginal responses, covariates
  
  dat <- data.frame(y1 = y1,
                    time = observed_time,
                    status = status,
                    x1 = x1,
                    x2 = x2,
                    x3 = x3,
                    x4 = x4,
                    x5 = x5)
  
  
  beta_copula_param <- ifelse(copulatype == 5, 12, 1.75)
  
  # true coefficients: 
  true_coefficients <- list(non_surv = c(beta_1[1], beta_1[2], beta_1[3]),
                            surv = c(beta_12, beta_01, beta_11),
                            copula_param = c(0, beta_copula_param)
  )
  
  
  # return a bit more than the dataset (!)
  output <- list(dataset = dat,
                 mean_y1 = mean(dat$y1),
                 original_tau = original_tau,
                 true_copula_param = theta3,
                 type_copula = copulatype, 
                 true_coefficients = true_coefficients,
                 censoring_rate = table(status),
                 censoring_rate_percent = table(status)/n,
                 true_survival_function = true_survival_function,
                 true_cumuhazard_function = true_cumuhazard_function, 
                 true_baselinehazard = true_baselinehazard,
                 true_log_baselinehazard = true_logbaselinehazard,
                 true_log_baselinehazard_centered = true_logbaseline_centered,
                 true_spline_margin1 = true_spline_margin1,
                 true_spline_margin1_centered = true_spline_margin1_centered
  )
  
  
  
  
  
  return(output)
  
}


# testobject <- create_data_binsurv_recursive(n = 2000)
