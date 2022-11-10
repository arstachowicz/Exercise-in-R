```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)

#Environmental Variables
Sys.setenv(TZ = "America/Chicago")
```
```{r message=FALSE}
#load raw csv file
dispatch_item_list <- read_csv("C:\\Users\\astachowicz\\Downloads\\XXICHOR_DISCRETE_JOB_DISPATCH_RPT.csv") # nolint
```
#clean up columns
#remove spaces and hash in header
colnames(dispatch_item_list) = gsub(" ", "_", colnames(dispatch_item_list)) 
colnames(dispatch_item_list) = gsub("#", "", colnames(dispatch_item_list)) 

dispatch_item_list %>%
    colnames()
    class(Operation_Completion_Date)
    
#select necessary columns
clean_dispatch <- dispatch_item_list %>% 
    select(Operation_Completion_Date, Job_Completion_Date, Current_Work_Center, Job_Status, Quantity_Remaining, Assembly, Job_Number)
```

```{r}
#narrow to clean room only
cr_items <- clean_dispatch %>%
    filter(Current_Work_Center == "Cleanroom")
```

```{r}
#change chr to date format in new column
cr_items <- cr_items %>%
    mutate(op_comp_date = mdy(Operation_Completion_Date)) %>%
    select(-Operation_Completion_Date)
```

```{r}
#Subtract system date from last operation date
#clean up duplicates & filter by over 30 days old
cr_items <- cr_items %>%
    mutate(wait_time = difftime(Sys.Date(), op_comp_date, units = "days")) %>%
    filter(wait_time > 30 & Job_Status == "Released") %>%
    distinct()
```

#Export df
#Generate file name, paste0 removes space that occurs before periods
file_path <- paste0("C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\Stagnant Jobs", format(Sys.Date(),"%m-%d"), ".csv", .sep="")
write.csv(cr_items, file_path)

