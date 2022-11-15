```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)
library(viridis)
library(stringr)
library(ggplot2)

#Environmental Variables
Sys.setenv(TZ = "America/Chicago")
```
```{r message=FALSE}
#load raw csv file
raw_csv <- file.choose()
if (str_sub(raw_csv, -4, -1) == ".csv") {
    dispatch_item_list <- read_csv(raw_csv)
} else {
    print("Not a CSV file.")
}
```
#clean up columns
#remove spaces and hash in header
colnames(dispatch_item_list) = gsub(" ", "_", colnames(dispatch_item_list))
colnames(dispatch_item_list) = gsub("#", "", colnames(dispatch_item_list))
```{r}
#select necessary columns
clean_dispatch <- dispatch_item_list %>%
    select(Operation_Completion_Date, Job_Completion_Date, Current_Work_Center,
            Job_Status, Quantity_Remaining, Assembly, Job_Number, Operation_Description) # nolint
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
    mutate(wait_time = as.numeric(wait_time)) %>%
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
grouped_by_wc <- cr_items %>%
    subset(!(Assembly == "03-10-0265" | Assembly == "03-10-0266")) %>%
    group_by(Operation_Description, .add = TRUE) %>%
    summarise(sum_group = sum(Quantity_Remaining)) %>%
    arrange(-sum_group)
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
#build graph matrices
df10wait <- dia_removed %>%
    slice(1:10)
df10qty <- dia_removed %>%
    arrange(-sum_quantity) %>%
    slice(1:10)

#Top 10 Longest Waits
viz_10waits <- ggplot(data = df10wait,
    aes(x = reorder(Assembly, -avg_wait), y = avg_wait,
                    label = avg_wait)) +
    geom_text(nudge_y = 1) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 14),
        plot.margin = margin(15, 10, 5, 5, "pt")) +
    labs(title = "Top 10 Longest Waits by Part Number",
        subtitle = "Stagnant Jobs in CR over 30 Days Old",
        x = "", y = "# of Days") +
    #removes gap between axis & bars
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave(paste0(format(Sys.Date(), "%b%d%y"), " top 10 waits.png"))

#Top 10 Largest Quantities
viz_10qty <- ggplot(data = df10qty,
    aes(x = reorder(Assembly, -sum_quantity), y = sum_quantity,
                    label = sum_quantity)) +
    geom_text(nudge_y = 15) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 14),
        plot.margin = margin(15, 10, 5, 5, "pt")) +
    labs(title = "Top 10 Job Quantities by Part Number",
        subtitle = "Stagnant Jobs in CR over 30 Days Old",
        x = "", y = "# of Parts") +
    #removes gap between axis & bars
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave(paste0(format(Sys.Date(), "%b%d%y"), " top 10 qty.png"))

#Top 10 Largest Quantities
viz_wc <- ggplot(data = grouped_by_wc,
    aes(x = reorder(Operation_Description, -sum_group), y = sum_group,
                    label = sum_group)) +
    geom_text(nudge_y = 15) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12),
        plot.margin = margin(15, 10, 5, 5, "pt")) +
    labs(title = "Job Quantities by Operation",
        subtitle = "Stagnant Jobs in CR over 30 Days Old",
        x = "", y = "# of Parts") +
    #removes gap between axis & bars
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave(paste0(format(Sys.Date(), "%b%d%y"), " wc qty.png"))
```
