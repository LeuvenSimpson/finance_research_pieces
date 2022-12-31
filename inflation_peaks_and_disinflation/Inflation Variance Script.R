library(tidyverse)
library(readxl)
library(zoo)

data <- read_xlsx("OECD 1970 Onwards CPI Data.xlsx")

data <- t(data)
colnames(data) <- data[1,]
data <- data[-1,]
data <- as.data.frame(data) %>% select(-Country)
rownames(data) <- NULL
data[2:13] <- lapply(data[2:13], as.numeric)
data <- data %>% mutate_if(is.numeric, round, digits = 2)
data[1] <- as.yearmon(data[[1]], "%b-%Y")

countries <- colnames(data[2:13])

inflation_graph <- function(country){
  
  plt <-  data %>% select(Month, country) %>% ggplot(aes(x=Month))+
    geom_line(aes(y = !!sym(country)), color = "#00bfc4", size = 1)+
    geom_hline(yintercept = 4, color = "#f8766d", size = 1)
    theme_classic()
  ggsave(paste(country,"_inflation.png", sep=""),
          width = 1920, height = 1080, units = "px")
}

#lapply(countries, inflation_graph)



peaks_finder <- function(country){
  data <- data %>% select(Month, country) %>% na.omit()
  peaks <- c()
  for(i in 6: nrow(data)){
    months_to_test <- data[i, "Month"]+ c(-6:-1,1:6)/12
    if(data[i, country] >= max(data[data$Month %in% months_to_test, country])){
      if(data[i, country] >= 4){
        peaks <- c(peaks, as.character(data[i, "Month"]))
        }
      }
  }

    return(peaks)
}

all_peaks <- lapply(countries, peaks_finder)
names(all_peaks) <- countries

remove_2022 <- function(peaks){
  peaks <- peaks[!grepl("2022", peaks)]
  return(peaks)

}

all_peaks <- lapply(all_peaks, remove_2022)


stats_setter <- function(peaks){
  res_data_frame <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(res_data_frame) <- c("Country", "Peak_Month", "Peak", "Threshold", "Standard_Dev", "Range")
  country <- names(peaks)
  for(i in 1:length(peaks[[1]])){
    
    peak_month <- as.yearmon(peaks[[1]][[i]], "%b %Y")
    
    months_to_test <- peak_month + c(0:6)/12
    inflations <- data %>% filter(Month %in% months_to_test) %>% select(country)
    inflations <- inflations[[1]]
    res_std <- sd(inflations)
    res_range <- max(inflations) - min(inflations)
    peak <- inflations[1]
    threshold <- floor(inflations[1])
    if(threshold %%2 != 0){
      threshold <- threshold - 1
    }
    new_row <- data.frame(country, peak_month, peak, threshold, res_std, res_range)
    
    names(new_row) <- c("Country", "Peak_Month", "Peak", "Threshold", "Standard_Dev", "Range")
    res_data_frame <- res_data_frame %>% rbind(new_row)
      
  }
  return(res_data_frame)
  
}
  
post_peak_movements <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(post_peak_movements) <- c("Country", "Peak_Month", "Peak", "Threshold", "Standard_Dev", "Range")

for(i in 1:length(all_peaks)){
  post_peak_movements <- post_peak_movements %>% rbind(stats_setter(all_peaks[i]))
  
}


std_model_threshold <- lm(Standard_Dev ~ Threshold, data = post_peak_movements)
summary(std_model_threshold)
range_model_threshold <- lm(Range ~ Threshold, data = post_peak_movements)
summary(range_model_threshold)

std_model <- lm(Standard_Dev ~ Peak, data = post_peak_movements)
summary(std_model)
range_model_threshold <- lm(Range ~ Peak, data = post_peak_movements)
summary(range_model)



post_peak_movements %>%
  ggplot(aes(x = Peak, y= Standard_Dev))+
  geom_point(size =2)+
  geom_abline(slope = coef(std_model)[[2]],
              intercept = coef(std_model)[[1]], color = "red")+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peaks (% pts)",y = "Inflation Standard Deviation over the next 6 Months (% pts)")+
  theme_minimal()

post_peak_movements %>%
  ggplot(aes(x = Peak, y= Range))+
  geom_point(size =2)+
  geom_abline(slope = coef(range_model)[[2]],
              intercept = coef(range_model)[[1]], color = "red")+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peaks (% pts)",y = "Inflation Range over the next 6 Months (% pts)")+
  theme_minimal()


post_peak_movements %>%
  ggplot(aes(x = Threshold, y= Standard_Dev))+
  geom_point(size =2)+
  geom_abline(slope = coef(std_model_threshold)[[2]],
              intercept = coef(std_model_threshold)[[1]], color = "red")+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peak Threshold (% pts)",y = "Inflation Standard Deviation over the next 6 Months (% pts)")+
  theme_minimal()

post_peak_movements %>%
  ggplot(aes(x = Threshold, y= Range))+
  geom_point(size =2)+
  geom_abline(slope = coef(range_model_threshold)[[2]],
              intercept = coef(range_model_threshold)[[1]], color = "red")+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peak Threshold (% pts)",y = "Inflation Range over the next 6 Months (% pts)")+
  theme_minimal()

post_peak_summary <- post_peak_movements %>% group_by(Threshold) %>% summarise(
  Med_Std = median(Standard_Dev),
  Mean_Std = mean(Standard_Dev),
  Med_Range = median(Range),
  Mean_Range = mean(Range)
)

post_peak_summary %>% 
  ggplot(aes(x = Threshold))+
  geom_line(aes(y = Med_Std, colour = "Median"), size = 1)+
  geom_line(aes(y = Mean_Std, colour = "Mean"), size = 1)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peak Threshold (% pts)", y = "Inflation Standard Deviation over the next 6 Months (% pts)")+
  scale_colour_manual("Legend", values = c("Median" = "#00bfc4","Mean" = "#f8766d"))


post_peak_summary %>% 
  ggplot(aes(x = Threshold))+
  geom_line(aes(y = Med_Range, colour = "Median"), size = 1)+ 
  geom_line(aes(y = Mean_Range, colour = "Mean"), size = 1)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(4,30, by =2))+
  labs(x = "Inflation Peak Threshold (% pts)", y = "Inflation Range over the next 6 Months (% pts)")+
  scale_colour_manual("Legend", values = c("Median" = "#00bfc4","Mean" = "#f8766d"))







