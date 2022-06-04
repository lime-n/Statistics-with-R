
require(tidyverse)
require(MuMIn)

lm_hypothesis<-function(data, ...){
  #set the arguments
  pr <- list(...) %>% unlist() %>% data.frame(args=.)
  response=pr[1,]
  nm <- names(data)
  #get the predictors
  nm<-nm[!nm %in% response]
  #create the formula
  form<-reformulate(paste0(nm, collapse="+"), response=response)
  #get all combinations of the linear model
  table_dredge_form <- coefTable(dredge(lm(form, data=data,na.action = "na.fail")))
  #get only those that match the predictor of interest
  bool_rows <-table_dredge_form %>% seq_along() %>% 
    data.frame(row=., boolean=(sapply(table_dredge_form, rownames) %like% pr[2,])) 
  bool_rows <- bool_rows[bool_rows$boolean == 'TRUE',]%>% 
    data.frame(., row.names = 1:nrow(.)) %>% rownames_to_column()
  dredge_list <- list()
  #store the results
  for(nm in bool_rows$row){
    dredge_list[nm] <- table_dredge_form[nm]
  }
  dredge_list=dredge_list[lengths(dredge_list)!=0] %>% lapply(., data.frame)
  #create a function for the p-value
  p_value <- function(p, df){
    ifelse(p < 0, 2*pt(p, df), 2*pt(p, df, lower.tail = FALSE))
  }
  #create a function for the t-statistic and p-value
  hypothesis_test <- function(data){
    data%>% 
      rownames_to_column() %>% 
      mutate(t_statistic =((Estimate-as.numeric(pr[3,]))/Std..Error)) %>% 
      mutate(p_value=mapply(p_value,.[,5],.[,4])) %>% 
      select(rowname, df, Estimate, Std..Error, t_statistic, p_value)
  }
  #return the results
  dredge_list<-lapply(dredge_list,hypothesis_test)
  return(dredge_list)
}

#beta represents the hypotheses we're testing under. We're assuming a two-sided t-test.

add_model<-test_data(data=teengamb, response='gamble',predictor='sex',beta=0)


