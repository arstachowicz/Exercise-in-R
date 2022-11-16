```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
```
```{r message=FALSE}
#load raw csv file
raw_weldment_list <- read_csv("C:\\Users\\astachowicz\\Documents\\GitHub\\Dispatch List\\Weldment List.csv", # nolint
    col_names = FALSE)
dispatch_list_raw <- read_csv("C:\\Users\\astachowicz\\Downloads\\XXICHOR_DISCRETE_JOB_DISPATCH_RPT_XXICHOR_DISCRETE_JOB_DISPATCH_TMPL.csv") # nolint
```
```{r}
#clean up data
#some strings parsed as dates, remove data format (if present)
#change string to recognizeable pattern/mask
colnames(raw_weldment_list)[1] <- "Weldments"
raw_weldment_list$Weldments <- gsub("/", "-", raw_weldment_list$Weldments)
adj_weldments <- str_pad(raw_weldment_list$Weldments, 10,
    side = "left", pad = "0")
adj_weldments <- sort(adj_weldments)
```
```{r}
#load data sets
df1 <- file.choose()
if (str_sub(df1, -4, -1) == ".csv") {
    raw_df1 <- read_csv(df1)
} else {
    print("Not a CSV file.")
}
df2 <- file.choose()
if (str_sub(df2, -4, -1) == ".csv") {
    raw_df2 <- read_csv(df2)
} else {
    print("Not a CSV file.")
}
if (df1 == df2) {
    print("Same file selected twice.")
} else {
    raw_job_list <- bind_rows(raw_df1, raw_df2)
}
```
#clean up columns
#remove spaces and hash in header
colnames(raw_job_list) = gsub(" ", "_", colnames(raw_job_list))
colnames(raw_job_list) = gsub("#", "", colnames(raw_job_list))
colnames(dispatch_list_raw) = gsub(" ", "_", colnames(dispatch_list_raw))
colnames(dispatch_list_raw) = gsub("#", "", colnames(dispatch_list_raw))

```{r}
#Filtered to Weldments
job_list_col <- raw_job_list %>%
    select(Item_Name, Using_Assembly, Schedule_Ship_Date,
            Order_Type, Standard_Cost, Order_Quantity) %>%
    #convert currency to double to use sum(), convert date
    mutate(Standard_Cost = as.numeric(gsub("[$,]", "", Standard_Cost))) %>%
    mutate(Schedule_Ship_Date = mdy(Schedule_Ship_Date)) %>%
    mutate(Order_Quantity = abs(Order_Quantity)) %>%
    #sort through two columns and only keep rows that match weldment vector
    filter(Item_Name %in% adj_weldments) %>% # nolint
    distinct()

#Filtered to Weldments, Dispatch
dispatch_weld <- dispatch_list_raw %>%
    select(Assembly, Work_Center, Quantity_In_Queue, Job_Number,
        Quantity_Remaining, Job_Completion_Date) %>%
    #convert currency to double to use sum(), convert date
    mutate(Job_Completion_Date = mdy(Job_Completion_Date)) %>%
    #sort through two columns and only keep rows that match weldment vector
    filter((Assembly %in% adj_weldments) & (Work_Center == "Cleanroom")) %>%
    distinct() %>%
    select(-Work_Center)
#different quantity values???
quantity_df <- dispatch_weld %>%
    filter(Quantity_Remaining != Quantity_In_Queue)
nrow(quantity_df)
```
```{r}
#line plot of anticipated demand
job_list_filt <- job_list_col %>%
    filter(Order_Type == "Forecast")
#use 'aggregate' to group by date and summarize
job_list_filt <- aggregate(job_list_col["Order_Quantity"],
    by = job_list_col["Schedule_Ship_Date"], sum)
dispatch_date <- aggregate(dispatch_weld["Quantity_In_Queue"],
    by = dispatch_weld["Job_Completion_Date"], sum)

viz_dispatch_released <- ggplot() +
    geom_point(data = job_list_filt, aes(x = Schedule_Ship_Date,
        y = Order_Quantity), color = "green") +
    geom_point(data = dispatch_date, aes(x = Job_Completion_Date,
        y = Quantity_In_Queue), color = "red") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 14),
        plot.margin = margin(15, 10, 5, 5, "pt")) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_x_date() +
    labs(title = "Weldment Forecast", x = "", y = "Order Quantity")
windows(); viz_dispatch_released
```