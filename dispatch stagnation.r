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

```{r}
#select necessary columns
clean_dispatch <- dispatch_item_list %>%
    select(Operation_Completion_Date, Job_Completion_Date, Current_Work_Center, Job_Status, Quantity_Remaining, Assembly, Job_Number) # nolint
#narrow to clean room only
cr_items <- clean_dispatch %>%
    filter(Current_Work_Center == "Cleanroom") %>%
#change chr to date format in new column
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

```{r}
grouped_by_part <- cr_items %>%
    group_by(Assembly, .add = TRUE) %>%
    summarise(avg_wait = round(mean(wait_time), digits = 2),
        sum_quantity = sum(Quantity_Remaining)) %>%
    #use arrange(), order() fails because of group_by
    arrange(-avg_wait)
```

```{r}
#drop rows where certain part #'s are listed
dia_removed <- grouped_by_part %>%
    subset(!(Assembly == "03-10-0265" | Assembly == "03-10-0266"))
```

```{r}
#Export df
#Generate file name, paste0 removes space that occurs before periods
file_path <- paste0("C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\Dispatch List CR ", format(Sys.Date(),"%b%d%y"), ".csv") # nolint
write.csv(dia_removed, file_path)
```