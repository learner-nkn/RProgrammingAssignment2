rankhospital <- function(state, outcome, rank_num){
  
  # Import libraries
  library(dplyr)
  library(rlist)
  library(stringr)
  library(tidyr)
  
  # Check validity of state and outcome
  if(is.null(state) | str_length(state)<2){
    print("Invalid state")
    stop()
  }
  
  if(is.null(outcome) | str_length(outcome)<2){
    print("Invalid outcome")
    stop()
  }
  
  # Read data
  outcome_data = read.csv("outcome-of-care-measures.csv", header = TRUE, colClasses = "character",
                          check.names = FALSE)
  
  # Check validity of state
  states_list = list.cases(outcome_data$State)
  if((state %in% states_list) == FALSE){
    print("Invalid state")
    stop()
  }
  
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
  
  # Extract a filtered dataframe for the state
  selected_data <- filter(outcome_data,State==state, nm.rm=TRUE)
  final_filtered_data <- selected_data %>% select(outcome_data_cols[1:2],
                                                  outcome_data_cols[outcome_idx])
  final_filtered_data <- drop_na(final_filtered_data)
  
  # Sort by Hospital Name
  final_filtered_data <- arrange(final_filtered_data, outcome_data_cols[2])
  
  # Rank the hospitals
  Rank <- rank(final_filtered_data[,outcome_data_cols[outcome_idx]],ties.method = 'last')
  
  # Add the rank column
  final_filtered_data <- cbind(final_filtered_data, Rank)
  
  # Sort by outcome index
  final_filtered_data <- arrange(final_filtered_data,Rank)
  
  # Check validity of rank_num
  if(rank_num=="best"){
    rank_num <- 1
  }
  else if (rank_num=="worst"){
    rank_num <- length(Rank)
  }
  else if (rank_num > length(Rank))
  {
    return(NA)
  }
  # Extract the result
  hosp_name <- final_filtered_data[rank_num,outcome_data_cols[2]]
  
  return(hosp_name)
  
}