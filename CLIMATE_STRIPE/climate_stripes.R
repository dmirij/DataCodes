# This is Dina's Climate Stripe code using data from Berkeley Earth -------
# I decided to plot the raw temperature instead of the anomaly (temperature compared to long term average).
# I extracted the raw temperature using the monthly anomaly data and the Estimated Jan 1951-Dec 1980 
# monthly absolute temperature determined by Berkeley Earth.
library(tidyverse)
library(scico)
library(zoo)
# Get local summary of land-surface temperature results produced by the Berkeley Earth averaging method for Yerevan, Armenia
yerevan <- read.delim("https://data.berkeleyearth.org/auto/Local/TAVG/Text/40.99N-44.73E-TAVG-Trend.txt") 

# extract monthly average temperature for baseline ------------------------
months_vector  <- (yerevan[51, 1] %>% 
  str_split("    "))[[1]][2:13] %>% 
  trimws(which = "both", whitespace = "[ \t\r\n]")
temps_vector   <- (yerevan[52, 1] %>% 
  str_split("  "))[[1]][4:15] %>% 
  trimws(which = "both", whitespace = "[ \t\r\n]")
monthly_temps_base <- data.frame(month = months_vector, temperature = as.numeric(temps_vector))

# extract year, month, monthly anomaly (this is a difference in temperature not the actual temperature) as my raw data ---------------------
extract_monthly <- function(stringy) {
  monthly_temp <- (str_split(stringy, "  "))[[1]][c(2, 4, 6)] %>%
    trimws(which = "both", whitespace = "[ \t\r\n]") 
}

yerevan_monthly <- as_tibble(t
                             (sapply(yerevan[69:2960, ], 
                                      extract_monthly, 
                                      simplify = TRUE, 
                                      USE.NAMES = FALSE)
                               ),
                             .name_repair = 'unique')

colnames(yerevan_monthly) <- c("year", "month", "num")
yerevan_monthly$month <- as.integer(yerevan_monthly$month)
yerevan_monthly$num <- as.numeric(yerevan_monthly$num) 

# Find year starting which there is complete data -------------------------
# Get full range of years
year_list <- table(yerevan_monthly$year) %>% 
  names()
# Write loop to find the year after which there are no NAs
for (yearStart in rev(year_list)) {    # loop year_list form latest year to earliest year 
  my_flag <- yerevan_monthly %>% 
    filter(year == yearStart) %>% 
    subset(is.na(num) == TRUE) %>% 
    count() 
  if (my_flag != 0) {
    break
  } 
}

yerevan_monthly_yearStart <- yerevan_monthly %>% 
  filter(year >= as.character(as.integer(yearStart) +1))

# actual monthly temperatures starting from yearStart ----------------------
# Calculate the actual temperature for each month
yerevan_monthly_yearStart <- yerevan_monthly_yearStart %>%
  add_column(Temperature =1)
monthly_temps_base <- monthly_temps_base %>% 
  add_column(monthNum = 1:12)

# The actual monthoy temperature is the anomaly (yerevan_monthly_yearStart$num) 
# added to the Monthly baseline (monthly_temps_base$temperature)
for (mname in 1:12) {
  yerevan_monthly_yearStart <- yerevan_monthly_yearStart %>%
    mutate(Temperature = ifelse(month == mname, 
           num + (monthly_temps_base %>% 
                    filter(monthNum == mname))$temperature, 
           Temperature)
           )
}

# yearly average from monthly temperatures --------------------------------
yerevan_yearly_yearStart <- as.data.frame(yerevan_monthly_yearStart %>% 
                                            group_by(year) %>% 
                                            summarise(Temperature = mean(Temperature)))
# Climate Stripe graph of yearly average temperature with legends ---------------------------
theme1_stripe <- theme_minimal() +
  theme(axis.text.x = element_text(color = "#6C6C6C",size = 10, vjust = 5),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(color = "#6C6C6C"),
        legend.text = element_text(color = "#6C6C6C"),
        plot.title = element_text(color = "#6C6C6C",size = 15, face = "bold")
  )

ggplot(data = yerevan_yearly_yearStart, aes(x = as.numeric(year), y = 1, fill = Temperature)) +
  geom_bar(stat = "identity") +
  scale_fill_scico(palette = "vik",name ="\u00B0C") + 
  ggtitle("Climate Stripes Yerevan, Armenia") +
  theme1_stripe 
ggsave("Yerevan_climate_stripe_legend.png")

theme2_stripe <- theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(color = "#6C6C6C",size = 15, face = "bold")
  )
ggplot(data = yerevan_yearly_yearStart_anom, aes(x = as.numeric(year), y = 1, fill = num)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  scale_fill_scico(palette = "vik") +
  ggtitle("Climate Stripes Yerevan, Armenia") +
  theme2_stripe 
ggsave("Yerevan_climate_stripe_nolegend.png")
