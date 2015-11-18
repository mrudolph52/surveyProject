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



