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
raw_weldment_list <- read_csv("C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\Weldment Library\\weld number list.csv") # nolint
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

```{r}
#Filtered to Weldments
forecast_weld <- raw_job_list %>%
    select(Item_Name, Using_Assembly, Schedule_Ship_Date,
            Order_Type, Standard_Cost, Order_Quantity) %>%
    #convert currency to double to use sum(), convert date
    mutate(Standard_Cost = as.numeric(gsub("[$,]", "", Standard_Cost))) %>%
    mutate(Schedule_Ship_Date = mdy(Schedule_Ship_Date)) %>%
    mutate(Order_Quantity = abs(Order_Quantity)) %>%
    #sort through two columns and only keep rows that match weldment vector
    filter(Item_Name %in% adj_weldments) %>% # nolint
    distinct() %>%
    filter(Order_Type == "Forecast" | Order_Type == "Sales order") %>%
    select(-Order_Type)
```
```{r}
#Pull old jobs
job_list_raw <- read_csv("C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\transactions resaved.csv") # nolint
wo_sau_raw <- read_csv("C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\Weldment Library\\wo-sau number qty.csv") # nolint

colnames(job_list_raw) = gsub(" ", "_", colnames(job_list_raw))
colnames(job_list_raw) = gsub("#", "", colnames(job_list_raw))
colnames(wo_sau_raw) = gsub(" ", "_", colnames(wo_sau_raw))
colnames(wo_sau_raw) = gsub("#", "", colnames(wo_sau_raw))

#LEFT JOIN on work order # to get job quantity
job_list_merg <- merge(job_list_raw, wo_sau_raw,
    by.x = "Work_Order",
    by.y = "Job")

#eliminate returns
colnames(job_list_merg) = gsub("\n", "_", colnames(job_list_merg))

#completed weldments
job_list_qty <- job_list_merg %>%
    select(Completion_Date, Work_Order, Item,
        Quantity_Completed, Quantity_Start,
        Job_Status, Component_or_Resource) %>%
    mutate(Completion_Date = mdy(Completion_Date)) %>%
    filter(Item %in% adj_weldments) %>%
    filter(str_starts(Component_or_Resource, "W/C 80 Cleanline")) %>%
    distinct()
```
```{r}
#moving averages, grouped by month for line graphs
forecast_weld_avg <- forecast_weld %>%
    mutate(fore_year = year(Schedule_Ship_Date),
        fore_month = month(Schedule_Ship_Date)) %>%
    group_by(fore_month, fore_year) %>%
    summarise(qty_result = sum(Order_Quantity, na.rm = TRUE), .groups = "keep") %>% # nolint
    mutate(comb_date = mdy(paste0(fore_month, "/01/", fore_year))) %>%
    mutate(data_source = "Forecast")
completed_weld_avg <- job_list_qty %>%
    mutate(fore_year = year(Completion_Date),
        fore_month = month(Completion_Date),
        fore_week = week(Completion_Date)) %>%
    group_by(fore_month, fore_year) %>%
    summarise(qty_result = sum(Quantity_Start, na.rm = TRUE), .groups = "keep") %>% # nolint
    mutate(comb_date = mdy(paste0(fore_month, "/01/", fore_year))) %>%
    mutate(data_source = "Completed")

file_path <- "C:\\Users\\astachowicz\\Documents\\Weldment Tracker\\R Coded\\" # nolint
write.csv(forecast_weld_avg, paste0(file_path, "Forecast ",
    format(Sys.Date(), "%b%d%y"), ".csv"))
write.csv(completed_weld_avg, paste0(file_path, "Completed ",
    format(Sys.Date(), "%b%d%y"), ".csv"))
```
```{r}
#to create a legend
#better way to do this????
df_test <- bind_rows(forecast_weld_avg, completed_weld_avg)

#use 'aggregate' to group by date and summarize
viz_complete_forecast <- ggplot(data = df_test, aes(x = comb_date,
        y = qty_result, col = data_source)) +
    geom_line(size = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 22),
        plot.margin = margin(5, 5, 5, 15, "pt"),
        aspect.ratio = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    scale_x_date() +
    labs(title = "Weldment Forecast", x = "Completion/Ship Date",
        y = "Order Quantity", col = "Source")
windows(); viz_complete_forecast
ggsave(paste0(format(Sys.Date(), "%b%d%y"), " WELDMENTS avg month.png"))
```