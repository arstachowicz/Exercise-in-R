```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

#Environmental Variables
Sys.setenv(TZ = "America/Chicago")
```
```{r message=FALSE}
#load raw csv file
file_locat <- "C:\\Users\\astachowicz\\Documents\\"
raw_fume_csv <- read_csv(paste0(file_locat, "qryFUMEHOODS.csv"))

#prepare column heads
colnames(raw_fume_csv) = gsub(" ", "_", colnames(raw_fume_csv))
colnames(raw_fume_csv) = gsub("\\.", "", colnames(raw_fume_csv))

fume_csv <- raw_fume_csv %>%
    rename("Velocity" = 5) %>%
    select(Date, Chemical_Name, Line, Velocity) %>%
    mutate(Date = mdy(Date)) %>%
    mutate(mon = month(Date))

#remove outliers
#clean up fume_csv
quartiles <- quantile(fume_csv$Velocity,
    probs = c(.25, .75),
    na.rm = TRUE)
veloIQR <- IQR(fume_csv$Velocity)
lowerQ <- quartiles[1] - 1.5 * veloIQR
upperQ <- quartiles[2] + 1.5 * veloIQR
fume_csv_no_outliers <- fume_csv %>%
    subset(Velocity > lowerQ & Velocity < upperQ)

#group and summarize, ignoring chemical/line
grouped_date <- fume_csv_no_outliers %>%
    group_by(Date, .add = TRUE) %>%
    arrange(Date) %>%
    summarise(Velocity = mean(Velocity)) 

grouped_mont <- fume_csv_no_outliers %>%
    mutate(num_month = month(Date)) %>%
    group_by(num_month) %>%
    summarise(avg_Velocity = mean(Velocity, na.rm = FALSE)) %>%
    mutate(abbr_month = month.abb[num_month])

#atmosphere info
file_locat <- "C:\\Users\\astachowicz\\Documents\\Research\\Connection between Humidity and Baths\\" # nolint
raw_atmos_csv <- read_csv(paste0(file_locat, "tblATMOSPHERE.csv")) # nolint
colnames(raw_atmos_csv) = gsub(" ", "_", colnames(raw_atmos_csv))
colnames(raw_atmos_csv) = gsub("\\.", "", colnames(raw_atmos_csv))
atmos_csv <- raw_atmos_csv %>%
    rename("Temp" = 5) %>%
    select(-ID, -Time, -Temp) %>%
    mutate(Date = mdy(Date)) %>%
    mutate(monDate = month(Date)) %>%
    mutate(Rel_Humidity = as.numeric(gsub("\\%", "", Rel_Humidity))) %>%
    group_by(monDate) %>%
    summarise(Rel_Humidity = mean(Rel_Humidity))
head(atmos_csv)
head(grouped_mont)
nrow(atmos_csv)
nrow(grouped_mont)

atmos_season <- merge(grouped_mont, atmos_csv, by.x = "num_month",
        by.y = "monDate")

head(atmos_season)
#generate line graphs to show relationship
bar_season_graph <- ggplot(data = atmos_season,
        aes(x = reorder(abbr_month, num_month),
        y = avg_Velocity, fill = Rel_Humidity)) +
    geom_bar(stat = "identity", show.legend = TRUE) +
    labs(title = "Seasonal Effect on Ventilation",
        x = "Month",
        y = "Avg. Velocity (ft/min)",
        fill = "Rel.\nHumidity\n(%)") +
    coord_cartesian(ylim = c(100, 140)) +
    theme(aspect.ratio = .8,
        plot.title = element_text(size = 18, hjust = 0.5,
            margin = margin(b = 10), face = "bold"),
        axis.text = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(15, 10, 5, 5, "pt"),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.title.x = element_text(margin = margin(t = 10)))
windows(); bar_season_graph
ggsave(paste0(format(Sys.Date(), "%b%d%y"), " fume bargraph.png"))
```
