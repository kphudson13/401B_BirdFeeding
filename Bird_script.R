library(tidyverse)
library(AICcmodavg) #to run AIC models
library(grid) #for setting the plot backgrounds 
library(multcompView) #add the letters for pairwise comparisons 


bird.data <- read.csv("Bird_Data.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)

##### Clean the data #####

#use stringr to remove underscores and whitespace
bird.data$Species <-
  bird.data$Species %>%
  str_replace_all(., "_", " ") %>%
  str_squish(.) %>%
  str_remove_all(., "[*]") %>%
  str_remove_all(., "Unknown") %>%
  str_to_title(.)

#capitalize the letters 

##### Check the data #####

#see unique birds in each country 
spp_list <- 
  bird.data %>%
  group_by(Location) %>%
  summarize(unique(Species))

##### Create Diversity Data Frames #####

#diversity per location
diversity_loc <-
  bird.data %>%
  group_by(Location) %>%
  summarise(Diversity = length(unique(Species)))

#diversity per period
diversity_time <- 
  bird.data %>%
  group_by(Period) %>% 
  summarise(Diversity = length(unique(Species)))

#diversity per session
diversity_per <- 
  bird.data %>%
  group_by(Location, Period, Session) %>%
  summarise(Diversity = length(unique(Species)))


##### Statistics #####

#check for normality 
ggplot(diversity_per, aes(x = Diversity, y = after_stat(density))) +
  geom_histogram(binwidth = 1) +
  facet_grid(cols = vars(Period), rows = vars(Location))
  
  

#run an additave model 
add_model <- aov(Diversity ~ Period + Location, 
                 data = diversity_per)

summary(add_model)

#run a model with the interaction effect
full_model <- aov(Diversity ~ Period * Location, 
                 data = diversity_per)

summary(full_model)

#an AIC to compare the models
AIC(add_model, full_model)


#Tukey test to see which ones are different 
tukey_out <- TukeyHSD(full_model, conf.level = 0.95)

##### letters for plot #####

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
  diversity_per %>%
  group_by(Location, Period) %>%
  summarise(Max = max(Diversity))

#match the max values to the letters data frame 
for(i in 1:6){
  for(j in 1:6){
    if(letters.df[i,2] == Maxs[j,2] & letters.df[i,3] == Maxs[j,1]){
      letters.df[i,4] = Maxs[j,3]
    }
  }
}


##### Plots #####

#reorder the x axis
order = c("Dawn", "Noon", "Dusk")

#set a theme
v_theme <- theme(axis.line = element_line(colour = "black", 
                                          linewidth = 0.6),
                 panel.background = element_blank(),
                 panel.grid = element_blank(),
                 legend.background = element_blank(),
                 legend.title = element_text(face = "bold"),
                 axis.title = element_text(face = "bold"),
                 plot.background = element_blank())

#set the colours for the background
my_colour <- c("grey90", "#ccdbe4")
back <- rasterGrob(my_colour, 
                   width = unit(1, "npc"), 
                   height = unit(1, "npc"), 
                   interpolate = TRUE)

#plot for the location
violin_plot <- ggplot(data = diversity_per, 
                      aes(x = Period,
                          y = Diversity,
                          fill = Location)) +
  geom_violin(draw_quantiles = 0.5) +
  scale_x_discrete(limits = order) +
  scale_y_continuous(breaks = 4) +
  v_theme +
  geom_text(data = letters.df, 
            aes(y = Max+1, label = Letters),
            size = 5,
            position = position_dodge(0.9)) +
  scale_fill_manual(values = c("darkmagenta","slategrey"),
                  name="") 

#this sets the plot background and pastes the plot on it
grid.newpage()
grid.draw(back)
print(violin_plot, 
      newpage = FALSE)


  

  
