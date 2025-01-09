

prepare_data <- function(type = c("DT", "PW"), the_intervals, data, PW_formula = NULL, PW_maximum_time_cut = NULL, DT_timeCol = NULL, DT_eventCol = NULL){
  
  output <- list()
  
  
  if(type == "DT"){
    
    # 0 is always included in this function, so remove it from the vector of the_used_cuts!
    dat_disc <-  discSurv:::contToDisc(dataShort = as.data.frame(data), 
                            timeColumn = DT_timeCol, 
                            intervalLimits = the_intervals[-1]
    )
    
    
    
    the_data_LONG <- discSurv:::dataLong(dat_disc, 
                                                timeColumn = "timeDisc", 
                                                eventColumn = DT_eventCol)
    
    # create list of indices for margin 2 
    INDCS_list <- sapply(levels(as.factor(the_data_LONG$obj)), 
                         function(i) which(the_data_LONG$obj == i), 
                         simplify = FALSE)
    
    the_data_short <- data
    
    
    ### Small modifications required for GJRM: 
    the_data_short$y_margin2 <- sample(the_data_LONG$y, size = nrow(data), replace = TRUE)
    the_data_short$timeInt_margin2 <- sample(the_data_LONG$y, size = nrow(data), replace = TRUE)
    
    # rename in long dataset to avoid any issues
    the_data_LONG$y_margin2 <- the_data_LONG$y
    the_data_LONG$timeInt_margin2 <- the_data_LONG$timeInt
    
  }
  
  
  
  if(type == "PW"){
      
    
    number_of_ints <- (length(the_intervals)-1)
    
    
    ### Create long-format data:
    dat_ped <- as_ped(PW_formula, 
                      max_time = PW_maximum_time_cut,
                      cut = seq(from = 0,
                                to = PW_maximum_time_cut,
                                length.out = number_of_ints),
                      data = data)
    
    
    the_data_short <- data
    
    # declare IDs as factor:
    dat_ped$id <- factor(dat_ped$id)
    
    
    # Create list of indices: (ABSOLUTELY NECESSARY FOR MBS)
    INDCS_list <- sapply(levels(dat_ped$id),
                         function(i) which(dat_ped$id == i),
                         simplify = FALSE)
    
    
    # attach ped_status in the model:
    the_data_short$ped_status <- unlist(lapply(INDCS_list, function(i) tail(i, 1)))
    the_data_short$tend <- dat_ped$tend[the_data_short$ped_status]
    
    the_data_LONG <- dat_ped
    
    
  }
  
  output$DataShort <- the_data_short
  output$DataLong <- the_data_LONG
  output$ListOfIndices <- INDCS_list
  
  
  return(output)
}
