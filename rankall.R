rankall <- function(outcome, rank_num= "best"){
  # Import libraries
  library(dplyr)
  library(rlist)
  library(stringr)
  library(tidyr)
  

  if(is.null(outcome) | str_length(outcome)<2){
    print("Invalid outcome")
    stop()
  }
  
  # Read data
  outcome_data = read.csv("outcome-of-care-measures.csv", header = TRUE, colClasses = "character",
                          check.names = FALSE)
  
  # Check validity of outcome
  valid_outcomes = list("heart attack", "heart failure", "pneumonia")
  if((outcome %in% valid_outcomes) == FALSE){
    print("Invalid outcome")
    stop()
  }
  
  outcome_data_cols = colnames(outcome_data)
  
  # Match the column to the outcome 
  regex1 = "[*:alnum*](.*)Mortality(.*)[*:alnum*](.*)"
  regex_final = tolower(paste(regex1, outcome))
  outcome_idx = 0
  for (i in 1:length(outcome_data_cols))
  {
    if(regexec(regex_final, tolower(outcome_data_cols[i]))[[1]][1]>0 && 
       regexec("^H", outcome_data_cols[i])[[1]][1]>0) 
    {
      outcome_idx = i
    }
  }
  
  # Convert the format of the outcome column to numeric
  outcome_data[,outcome_data_cols[outcome_idx]] <- as.numeric(outcome_data[,outcome_data_cols[outcome_idx]])
  
  states_list = list.cases(outcome_data$State)
  
  final_df <- data.frame(State=c(),Hospital_Name=c(), Rate=c())
  for (state_itr in states_list){
    # Extract a filtered dataframe for the state
    selected_data <- filter(outcome_data,State==state_itr, nm.rm=TRUE)
    final_filtered_data <- selected_data %>% select(outcome_data_cols[1:2],
                                                    outcome_data_cols[outcome_idx],
                                                    outcome_data_cols[7])
    final_filtered_data <- drop_na(final_filtered_data)
    
    # Sort by Hospital Name
    final_filtered_data <- arrange(final_filtered_data, outcome_data_cols[2])
    
    remove(Rank)
    
    # Rank the hospitals
    Rank <- rank(final_filtered_data[,outcome_data_cols[outcome_idx]],ties.method = 'last')
    #print(paste(state_itr,length(Rank)))
    
    # Add the rank column
    final_filtered_data <- cbind(final_filtered_data, Rank)
    
    # Sort by outcome index
    final_filtered_data <- arrange(final_filtered_data,Rank)
    
    
    final_filtered_data <- final_filtered_data %>% select(outcome_data_cols[2],
                                                          outcome_data_cols[7],
                                                          outcome_data_cols[outcome_idx])
    # Check validity of rank_num
    if(rank_num=="best"){
      rank_num <- 1
      dummy_df <- data.frame(State=state_itr, 
                             Hospital_Name= final_filtered_data[rank_num,outcome_data_cols[2]], 
                             Rate=final_filtered_data[rank_num,outcome_data_cols[outcome_idx]])
      final_df <- rbind(final_df,dummy_df)
    }
    else if (rank_num=="worst"){
      rank_num <- length(Rank)
      dummy_df <- data.frame(State=state_itr, 
                             Hospital_Name= final_filtered_data[rank_num,outcome_data_cols[2]], 
                             Rate=final_filtered_data[rank_num,outcome_data_cols[outcome_idx]])
      final_df <- rbind(final_df,dummy_df)
    }
    else if (rank_num > length(Rank))
    {
      dummy_df <- data.frame(State=state_itr, 
                             Hospital_Name= NaN, 
                             Rate=NaN)
      final_df <- rbind(final_df,dummy_df)      
      #print(final_df)
    }
    else
    {
      # Extract the result
      #final_df <- rbind(final_df,final_filtered_data[rank_num,])
      dummy_df <- data.frame(State=state_itr, 
                             Hospital_Name= final_filtered_data[rank_num,outcome_data_cols[2]], 
                             Rate=final_filtered_data[rank_num,outcome_data_cols[outcome_idx]])
      final_df <- rbind(final_df,dummy_df)
    }
    
  }
  return(final_df)
}