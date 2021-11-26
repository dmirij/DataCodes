# This is Dina's Climate Stripe code using data from Berkeley Earth -------
library(tidyverse)
library(scico)
library(zoo)
# Data downloaded on 6/22/2021 from http://berkeleyearth.lbl.gov/auto/Local/TAVG/Text/40.99N-44.73E-TAVG-Trend.txt
yerevan_berkeley <- read.delim("Yerevan-TAVG-Trend.txt") 
# This data was from an analysis that was run on 06-Jan-2021 15:35:34" by Berkeley Earth

# extract monthly average temperature for baseline ------------------------
months_vector  <- (yerevan_berkeley[51, 1] %>% 
  str_split("    "))[[1]][2:13] %>% 
  trimws(which = "both", whitespace = "[ \t\r\n]")
temps_vector   <- (yerevan_berkeley[52, 1] %>% 
  str_split("  "))[[1]][4:15] %>% 
  trimws(which = "both", whitespace = "[ \t\r\n]")
monthly_temps_base <- data.frame(month = months_vector, temperature = as.numeric(temps_vector))

# extract year, month, monthly anomaly as my raw data ---------------------
extract_monthly <- function(stringy) {
  monthly_temp <- (str_split(stringy, "  "))[[1]][c(2, 4, 6)] %>%
    trimws(which = "both", whitespace = "[ \t\r\n]") 
}
yerevan_monthly <- as_tibble(t(sapply(yerevan_berkeley[69:2960, ], extract_monthly, simplify = TRUE, USE.NAMES = FALSE)))
colnames(yerevan_monthly) <- c("year", "month", "num")
yerevan_monthly$month <- as.integer(yerevan_monthly$month)
yerevan_monthly$num <- as.numeric(yerevan_monthly$num) 

# Find year starting which there is complete data -------------------------
year_list <- table(yerevan_monthly$year) %>% 
  names()
# Write loop to find the year after which there are no NAs
for (yearStart in rev(year_list)) {
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
yerevan_monthly_yearStart <- yerevan_monthly_yearStart %>%
  add_column(Temperature =1)
monthly_temps_base <- monthly_temps_base %>% 
  add_column(monthNum = 1:12)
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
yerevan_yearly_yearStart_anom <- as.data.frame(yerevan_monthly_yearStart %>% 
                                                 group_by(year) %>% 
                                                 summarise(num = mean(num)))

# Stripe graph of yearly average temperature V1 ---------------------------
theme1_stripe <- theme_minimal() +
  theme(axis.text.x = element_text(size = 10, vjust = 5),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_text(),
        plot.title = element_text(size = 15, face = "bold")
  )
ggplot(data = yerevan_yearly_yearStart_anom, aes(x = as.numeric(year), y = 1, fill = num)) +
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
        plot.title = element_text(size = 15, face = "bold")
  )
ggplot(data = yerevan_yearly_yearStart_anom, aes(x = as.numeric(year), y = 1, fill = num)) +
  geom_bar(stat = "identity",show.legend = FALSE) +
  scale_fill_scico(palette = "vik") +
  ggtitle("Climate Stripes Yerevan, Armenia") +
  theme2_stripe 
ggsave("Yerevan_climate_stripe_nolegend.png")

theme3_stripe <- theme_minimal() +
  theme(axis.text.x = element_text(size = 10, vjust = 5),
        axis.text.y = element_text(size = 10),
        axis.line.y = element_blank(),
        axis.title.y =  element_text(size = 13),
        axis.title.x =  element_blank(),
        panel.grid.major = element_line(color = "grey",size=0.25,linetype = "dashed"),
        plot.title = element_text(size = 15, face = "bold"),
        legend.position = c(0.15, 0.95)
  )
ggplot(data = yerevan_monthly_yearStart) +
  geom_line(aes(y=rollmean(Temperature, 12, fill = NA),
                x=as.numeric(year),
                color = "12-month moving average"),
            size = 1
            ) +
   geom_line(aes(y=rollmean(Temperature, 120, fill = NA),
                x=as.numeric(year),
                color = "10-year moving average"),
            size = 1.1
            ) +
  scale_color_manual(name = "", values = c("12-month moving average" = "darkblue",
                                           "10-year moving average" = "darkred"))  +
  ggtitle("Local Climate Yerevan, Armenia") +
  ylab("Mean Temperature (\u00B0C)") +
  theme3_stripe 
ggsave("Yerevan_climate_moving_average.png")
