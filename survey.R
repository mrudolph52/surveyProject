### Survey project in R

# install and load library
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")
# import file
surveys <- read.csv('data/portal_data_joined.csv')

## ideas for project
# comparison between 

# relationship between hindfoot_length or weight and sex 

# relationship between sex and weight and time period - line graph

## relationship between hindfoot_length and sex

# link commands together

## Figure 1: Relationship between hindfoot_length and weight for 2002

# data parsing
dplyr()
filter(surveys, year == 2002)
surveys_fig1 <- surveys
surveys_fig1_small <- surveys %>%
  filter(year == 2002) %>%
  select(hindfoot_length, weight, species) %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight)) 
  
# build figure
ggplot(data = surveys_fig1_small, aes(x=hindfoot_length, y=weight, by = species)) + geom_point(aes(color = species), stat = "identity") + ggtitle("Mean hindfoot length vs. mean weight\nin different rodent species in 2002") + labs(x="Hindfoot length (mm)", y= "Weight (g)") + theme_bw() + theme(plot.title = element_text(face="bold", size=22))

ggsave(file="Figure1_revised.pdf" ) #export figure

## Figure 2: Distribution of weights for females and males in peromyscus

# data parsing
surveys_fig2_small <- surveys %>%
  filter(species_id == "PM") %>%
  group_by (sex, weight) %>%
  tally %>%
  filter (!is.na(weight)) %>%
  filter(!sex == "")

# build figure
ggplot(data = surveys_fig2_small, aes(x=sex, y=weight)) + geom_boxplot(aes(fill = sex)) + theme_classic() + ggtitle("Distribution of weights for females\nand males in peromyscus") + labs(x="Sex", y= "Weight (g)") + theme(plot.title = element_text(face="bold.italic", size=22)) + theme_bw() + theme(legend.position = "none") 

# Export figure
ggsave(file="Figure2.pdf")


## Figure 3: Compare the difference in hindfoot_lengths between species (DO) in 1996

# data parsing 
surveys_fig3_small <- surveys %>%
  group_by(species_id, hindfoot_length) %>%
  filter(year == 1996) %>%
  filter(!is.na(hindfoot_length)) %>%
  tally %>%
  summarize(mean_hindfoot_length=mean(hindfoot_length, na.rm=TRUE))

# Build figure
ggplot(data = surveys_fig3_small, aes(x=species_id, y=mean_hindfoot_length)) + geom_bar(aes(fill=species_id), color = "black", stat = "identity") + theme_bw() + ggtitle("Comparison of mean hindfoot lengths\nbetween different species in 1996") + labs(x="Species ID", y="Mean Hindfoot Length (mm)") + theme(plot.title = element_text(face="bold", size=22))

# export figure
ggsave(file="Figure3.pdf")

# statistical test - ANOva to see if there a significant difference in weights of different species
fit <- aov(weight ~ species_id, data = surveys) # fit model
fit # look at fit
summary(fit) # p value < 0.05, so there is a significant difference in weights between different species, F value = 11370


   
  




