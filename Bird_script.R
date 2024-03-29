#Built April 2023 
#Author: Kyle Hudson
#Group members: Jodie ***** and Jenni *****
#Class: REN R 401B, Northern Avian Ecology

##### Load packages and data #####

library(tidyverse)
library(AICcmodavg) #to run AIC models
library(gridExtra) #to export tables nicely
library(grid) #for setting the plot backgrounds 
library(multcompView) #add the letters for pairwise comparisons 
library(ggpattern) #for the gradient pattern background
library(knitr)

bird.data <- read.csv("Bird_Data.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

##### Clean the data #####

#use stringr to remove underscores and whitespace
bird.data$Species <-
  bird.data$Species %>%
  str_replace_all(., "_", " ") %>% #remove underscores
  str_squish(.) %>% #remove spaces
  str_remove_all(., "[*]") %>%
  str_remove_all(., "Unknown") %>%
  str_to_title(.) #capitalize the letters 

##### Check the data #####

#see unique birds in each country 
spp_list <- 
  bird.data %>%
  group_by(Location) %>%
  summarize(unique(Species))

#total count of bird species in each location
total_count <-
  bird.data %>% 
  group_by(Location) %>% 
  summarize(Richness = length(unique(Species)))

##### Create Richness Data Frames #####

#Richness per location per day
Richness_loc <-
  bird.data %>%
  group_by(Location, Date) %>%
  summarise(Richness = length(unique(Species)))


#Richness per session
Richness_per <- 
  bird.data %>%
  group_by(Location, Period, Session) %>%
  summarise(Richness = length(unique(Species)))


##### Statistics #####

#this will get overwritten but its one way to code a t-test
t_model <- with(Richness_loc, 
                t.test(Richness[Location == "Canada"],
                       Richness[Location == "Panama"]))

#a t-test to see if there is a difference between the countries 
t_model <- t.test(Richness ~ Location, data = Richness_loc)

#run an additive ANOVA model 
add_model <- aov(Richness ~ Period + Location, 
                 data = Richness_per)

summary(add_model)

#run an ANOVA model with the interaction effect
full_model <- aov(Richness ~ Period * Location, 
                  data = Richness_per)

summary(full_model)

#an AIC to compare the models
AIC(add_model, full_model)


#Tukey test to see which ones are different 
tukey_out <- TukeyHSD(full_model, conf.level = 0.95)


##### Mean and SD Tables #####

summary_loc <-
  Richness_loc %>% 
  group_by(Location) %>% 
  summarise(Mean = mean(Richness, na.rm=T), #adds the mean
            SD = sd(Richness, na.rm = T)) %>% #adds with sd
  as.data.frame() %>% 
  `row.names<-`(.$Location) %>% #how to set row names within a pipe
  subset(., select = -c(1)) %>%  #cut out the first column
  mutate(across(c(1:2), round, 2)) 

#export the summary stats
png("Location_Chart.png", 
    height = 110*nrow(summary_loc), 
    width = 300*ncol(summary_loc),
    res = 288)
grid.table(summary_loc)
dev.off()

#set the row names for the descriptave statistics table
summary_order <- c("Dawn:Canada",
                   "Noon:Canada",
                   "Dusk:Canada",
                   "Dawn:Panama",
                   "Noon:Panama", 
                   "Dusk:Panama")

#data frame for mean and sd per location and period
summary_per <-
  Richness_per %>% 
  group_by(Location, Period) %>% 
  summarise(Mean = mean(Richness, na.rm=T), #adds the mean
            SD = sd(Richness, na.rm = T)) %>% #adds with sd
  unite("r_names", 
        Period:Location, 
        sep = ":", 
        remove = TRUE) %>% #combine two columns
  as.data.frame() %>% 
  `row.names<-`(.$r_names) %>%   #how to set row names within a pipe
  subset(., select = -c(1)) %>%  #cut out the first column
  mutate(across(c(1:2), round, 2)) %>% 
  arrange(factor(rownames(.), levels = summary_order))
  
#export the summary stats
png("Per_Chart.png", 
    height = 100*nrow(summary_per), 
    width = 360*ncol(summary_per),
    res = 288)
grid.table(summary_per)
dev.off()

##### ANOVA and Tukey Tables #####

#create a data frame of the anova output 
full_out <- summary(full_model)

full_chart <- 
  as.data.frame(full_out[[1]]) %>% 
  mutate(across(c(2:5), round, 2)) %>% 
  replace(., is.na(.), "")

#turn all the small p values into <0.05
for(i in 1:nrow(full_chart)) {
  if(full_chart[i,5] < 0.05) {
    full_chart[i,5] = "<0.05"
  }
}

#remove that unneccesary p-value
full_chart[4,5] <- "" 

#export the summary stats
png("ANOVA_Chart.png", 
    height = 110*nrow(full_chart), 
    width = 280*ncol(full_chart),
    res = 288)
grid.table(full_chart)
dev.off()

#Turn the tukey pairwise output into a table
TuChart <- as.data.frame(tukey_out$`Period:Location`)

#change the header names
names(TuChart) <- c("Difference", 
                    "Lower Est.",
                    "Upper Est.",
                    "Adjusted P-Value")

#round and clean the tukey output
TuChart <- 
  TuChart %>% 
  mutate(across(c(1:3), round, 3)) %>% 
  mutate(`Adjusted P-Value` = round(`Adjusted P-Value`, 2)) %>% 
  mutate(`Adjusted P-Value` = 
           format(`Adjusted P-Value`, scientific = FALSE)) %>% 
  mutate(`Adjusted P-Value` = as.numeric(`Adjusted P-Value`))

#turn all the small p values into <0.05
for(i in 1:nrow(TuChart)) {
  if(TuChart[i,4] < 0.05) {
    TuChart[i,4] = "<0.05"
  }
}

#set the order to reoder the pairwise analysis 
pair_order <- c("Noon:Canada-Dawn:Canada", 
                "Noon:Canada-Dusk:Canada",
                "Dusk:Canada-Dawn:Canada",
                "Dawn:Panama-Dawn:Canada",
                "Dawn:Panama-Noon:Canada",
                "Dawn:Panama-Dusk:Canada",
                "Noon:Panama-Dawn:Canada",
                "Noon:Panama-Noon:Canada",
                "Noon:Panama-Dusk:Canada",
                "Dusk:Panama-Dawn:Canada",
                "Dusk:Panama-Noon:Canada",
                "Dusk:Panama-Dusk:Canada",
                "Noon:Panama-Dawn:Panama",
                "Noon:Panama-Dusk:Panama",
                "Dusk:Panama-Dawn:Panama")

#reorder the row names
TuChart <-
  TuChart %>% 
  arrange(factor(rownames(.), levels = pair_order))

#export the summary stats
png("Tukey_Chart.png", 
    height = 90*nrow(TuChart), 
    width = 510*ncol(TuChart),
    res = 288)
grid.table(TuChart)
dev.off()

##### T-Test Validation #####

#check for normality 
T_Hist <- 
  ggplot(Richness_loc, aes(x = Richness, y = after_stat(density))) +
  geom_histogram(binwidth = 1) +
  ylab("Density") +
  facet_grid(cols = vars(Location))

#save the histogram as a png
ggsave("T_Hist.png", T_Hist, width = 8, height = 4, dpi = 300)

#QQ plot for standarized residuals 
T_QQ <- 
  ggplot(data.frame(residuals(t_model)), aes(sample = Richness)) +
  geom_qq() + 
  geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") 

#save the histogram as a png
#ggsave("T_QQ.png", T_QQ, width = 8, height = 5, dpi = 300)

##### ANOVA Validation #####

#check for normality 
AOV_Hist <- 
  ggplot(Richness_per, aes(x = Richness, y = after_stat(density))) +
  geom_histogram(binwidth = 1) +
  ylab("Density") +
  facet_grid(cols = vars(Period), rows = vars(Location))

#save the histogram as a png
ggsave("ANOVA_Hist.png", AOV_Hist, width = 8, height = 5, dpi = 300)

#QQ plot for standarized residuals not faceted
AOV_QQ_Sing <- 
  ggplot(full_model, aes(sample = Richness)) +
  geom_qq() + 
  geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") 

#save the histogram as a png
ggsave("ANOVA_QQ_Sing.png", AOV_QQ_Sing, width = 8, height = 5, dpi = 300)

#QQ plot for standarized residuals 
AOV_QQ <- 
  ggplot(full_model, aes(sample = Richness)) +
  geom_qq() + 
  geom_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  facet_grid(cols = vars(Period), rows = vars(Location))

#save the histogram as a png
ggsave("ANOVA_QQ.png", AOV_QQ, width = 8, height = 5, dpi = 300)

##### Letters for plot #####

#create a data frame for the letters 
letters.df <- 
  as.data.frame(multcompLetters(tukey_out$`Period:Location`[,4])$Letters) 

#add columns for the location and period
#and a blank column for the max
letters.df <-
  letters.df %>%
  mutate(Period = str_sub((rownames(letters.df)), 1, 4)) %>%
  mutate(Location = str_sub((rownames(letters.df)), 6, 11)) %>%
  mutate(Max = 0)

#call it something nicer
colnames(letters.df)[1] <- "Letters"

#create a data frame of the max values
Maxs <-
  Richness_per %>%
  group_by(Location, Period) %>%
  summarise(Max = max(Richness))

#match the max values to the letters data frame 
for(i in 1:6){
  for(j in 1:6){
    if(letters.df[i,2] == Maxs[j,2] & letters.df[i,3] == Maxs[j,1]){
      letters.df[i,4] = Maxs[j,3]
    }
  }
}

##### Plots #####

#set a theme
v_theme <- theme(axis.line = element_line(colour = "black", 
                                          linewidth = 0.6),
                 panel.background = element_blank(),
                 panel.grid = element_blank(),
                 legend.background = element_blank(),
                 legend.title = element_text(face = "bold"),
                 axis.title = element_text(face = "bold"),
                 plot.background = element_rect(fill = "grey95"))

#make a plot of Richness per location
location_plot <-
  ggplot(data = Richness_loc, 
         aes(x = Location, 
             y = Richness, 
             fill = Location)) +
  geom_violin(draw_quantiles = 0.5, 
              show.legend = FALSE,
              width = 0.5) +
  v_theme +
  scale_fill_manual(values = c("darkmagenta","slategrey"),
                    name="") +
  geom_jitter(show.legend = FALSE,
              alpha = 0.5,
              size = 1.3,
              width = 0.06)

location_plot

#save the boxplot as a png
ggsave("Location_Plot.png", location_plot, width = 5, height = 5, dpi = 400)

#reorder the x axis
period_order = c("Dawn", "Noon", "Dusk")

#plot for location and period
combined_plot <- ggplot(data = Richness_per, 
                        aes(x = Period,
                            y = Richness,
                            fill = Location)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(limits = period_order) +
  scale_y_continuous(n.breaks = 7) +
  v_theme +
  geom_text(data = letters.df, 
            aes(y = Max+1, label = Letters),
            size = 5,
            position = position_dodge(0.9)) +
  scale_fill_manual(values = c("darkmagenta","slategrey"),
                    name="") +
  geom_point(position = position_jitterdodge(jitter.height = 0.15,
                                             jitter.width = 0.05,
                                             dodge.width = 0.9),
             alpha = 0.5,
             size = 1.3, 
             show.legend = FALSE) 

combined_plot

#save the histogram as a png
ggsave("Combined_Plot.png", combined_plot, width = 8, height = 5, dpi = 400)



