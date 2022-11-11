```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)
library(viridis)

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

```{r}
#build "parts with top 10 wait times" bargraphs
#must be made numeric, diffdate not accepted in vectors:
top10_waits_values <- as.numeric(dia_removed$avg_wait[1:10])
top10_waits_values <- rev(top10_waits_values)
top10_waits_names <- rev(dia_removed$Assembly[1:10])
top10_waits <- setNames(top10_waits_values, top10_waits_names)

#create and save bar graph
png(filename = "parts top10waittimes.png", width = 650, height = 580)
par(mar = c(5, 9, 4, 1) + 1) #margins

#increase x-axis to include largest value
x_max1 <- ceiling(max(top10_waits_values) * 1.10)
barplot(top10_waits,
        xlab = "# of Days",
        las = 2,
        horiz = TRUE,
        xlim = c(0, x_max1),
        col = viridis(10)) # nolint
title("Parts with Top 10 Wait Times")
dev.off()
```

```{r}
#build "parts with top 10 quantities" bargraphs
#must be made numeric, diffdate not accepted in vectors:
dia_removed_arr_qty <- dia_removed %>%
    arrange(-sum_quantity)
top10_qty_values <- rev(dia_removed_arr_qty$sum_quantity[1:10])
top10_qty_names <- rev(dia_removed_arr_qty$Assembly[1:10])
top10_qty <- setNames(top10_qty_values, top10_qty_names)

#create and save bar graph
png(filename = "parts top10qtys.png", width = 650, height = 580)
par(mar = c(5, 9, 4, 1) + 1) #margins

#increase x-axis to include largest value
x_max2 <- ceiling(max(top10_qty_values) * 1.10)
barplot(top10_qty,
        xlab = "# of Parts",
        las = 2,
        horiz = TRUE,
        xlim = c(0, x_max2),
        col = viridis(10)) # nolint
title("Top 10 Largest Quantities by Part Number",
        cex = 3)
dev.off()
```

