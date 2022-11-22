```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(stringr)
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
colnames(raw_weldment_list) = gsub(" ", "_", colnames(raw_weldment_list))

adj_weldments <- raw_weldment_list %>%
    filter(Number_Of_Welds >= 4)

adj_weldments$Weldments <- str_pad(adj_weldments$Weldments, 10, side = "left", pad = "0") # nolint
adj_weldments <- adj_weldments %>%
    arrange(Weldments)
head(adj_weldments)
nrow(adj_weldments)

file_path <- "C:\\Users\\astachowicz\\Desktop\\" # nolint
write.csv(adj_weldments, paste0(file_path, " Weld List.csv"))
```
```{r message=FALSE}
#load raw csv file
df_raw <- file.choose()
csv_weld <- read_csv(df_raw)

colnames(csv_weld)
colnames(csv_weld) = gsub(" ", "_", colnames(csv_weld))

#clean up data
#some strings parsed as dates, remove data format (if present)
#change string to recognizeable pattern/mask

weld_list01 <- csv_weld %>%
    select(ITEM_NUMBER, PRODUCT_FAMILY, PRODUCT_GROUP) %>%
    filter(str_detect(PRODUCT_FAMILY, "Weldment") | str_detect(PRODUCT_GROUP, "Weldment")) %>% # nolint
    filter(!str_detect(ITEM_NUMBER, "03-10-0338")) %>%
    filter(ITEM_NUMBER %in% adj_weldments) %>% # nolint
    distinct() %>%
    arrange(ITEM_NUMBER)

amat_lam_weld <- weld_list01 %>%
    filter(str_detect(ITEM_NUMBER, "03-33-209"))

file_path <- "C:\\Users\\astachowicz\\Desktop\\" # nolint
write.csv(weld_list01, paste0(file_path, " Weld List.csv"))
nrow(weld_list01)

```