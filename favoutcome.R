favoutcome <- function(state, outcome){
  
  # Import packages
  library(dplyr)
  library(rlist)
  library(stringr)
  
  # Check validity of state and outcome
  if(is.null(state) | str_length(state)<2)
  {
    print("Invalid state, input state")
    stop()
  }
  if(is.null(outcome) | str_length(outcome)<2)
  {
    print("Invalid outcome, input outcome")
    stop()
  }
  valid_outcomes <- list("heart attack", "heart failure", "pneumonia")
  
  if ((outcome %in% valid_outcomes)==FALSE){
    print("Invalid outcome")
    stop()
  }
  
  # Read data and column names
  outcome_data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character', header = TRUE,
                           check.names = FALSE)
  outcome_data_cols <- colnames(outcome_data)
  
  # Check validity of state
  states_lst <- list.cases(outcome_data$State)
  if((state %in%states_lst)==FALSE){
    print("Invalid state")
    stop()
  }
  
  # Match the outcome with the column index
  regexp1 <- "[*:alnum:*](.*)Mortality(.*)[*:alnum:*](.*)"
  regexp_chk <- tolower(paste(regexp1,outcome))
  outcome_idx = 0
  for (i in 1:length(outcome_data_cols))
  {
    if(regexec(regexp_chk, tolower(outcome_data_cols[i]))[[1]][1]>0 && 
       regexec("^H", outcome_data_cols[i])[[1]][1]>0) 
    {
      outcome_idx = i
    }
  }
  
  # Convert the format of the outcome column to numeric
  outcome_data[,outcome_data_cols[outcome_idx]] <- as.numeric(outcome_data[,outcome_data_cols[outcome_idx]])
  
  # Find the min outcome in the specified state
  result <- tapply(outcome_data[,outcome_data_cols[outcome_idx]], outcome_data$State, min, na.rm =TRUE)[state][[1]]
  
  # Extract a filtered dataframe for the state
  selected_data <- filter(outcome_data,State==state)
  
  # Filter the dataframe further to extract the hospital name
  vars <- c(outcome_data_cols[outcome_idx], "State")
  cond <- c(result, state)
  hosp_data <- selected_data%>%
    filter(.data[[vars[[1]]]] == cond[[1]],
           .data[[vars[[2]]]] == cond[[2]])
  
  hosp_name = hosp_data[['Hospital Name']]    
  
  return(hosp_name)
}