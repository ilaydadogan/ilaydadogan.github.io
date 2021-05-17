
#path to image of upset couple
imgpath1 <- '/Users/user/documents/PSY6422_project/PSY6422/figs/depression.jpg'

#Include graphics 
knitr::include_graphics(imgpath1)

#Path the data 
library (here)

#Load the data
df <- read.csv(here("data", "rawdata.csv"))

#Display the data
head(df)

# Assigning names to vectors
no <- " Survey number"
gender <- " Female (1) or Male (2)"
education <-  " Primary (1), High school (2), Bachelor (3) or Msc or phD (4)"
working_status <- "Employed (1) or Unemployed (2)"
marriage_style <- "Arranged marriage (1) or Flirt marriage/Love (2)" 
status_of_having_a_child <- "Yes/Does have a child (1) or  No/ Does not have a child (2)"
bdi <- "Beck Depression Inventory, calculated by the sum of B1-B21"

#Creating a basic table

table <- data.frame(no, gender, education, working_status, marriage_style, status_of_having_a_child, bdi)

#Loading kableExtra to edit the table
library(kableExtra)

#Renaming the columns
kbl(table, col.names = c("No", "Gender", "Education", "Working status", "Marriage style", "Status of having a child", "BDI") ) %>%
  
  #Changing the table features
  kable_styling(bootstrap_options = "condensed", full_width = F,
                html_font = "arial", font_size=13)


#creating a copy of the data for modifications 
copy <- read.csv(here("data", "rawdata.csv"))

#removing the non essential "No" column 
copy$No <- NULL

# Defining the keys for simplicity 
library(dplyr)

copy$Gender [copy$Gender == 1] <- "Female"
copy$Gender [copy$Gender ==2] <- "Male"

copy$Education [copy$Education ==1] <- "Primary"
copy$Education [copy$Education ==2] <-  "High-school"
copy$Education [copy$Education ==3] <- "Bachelor"
copy$Education [copy$Education ==4] <- "Msc or phD"

copy$Working.Status [copy$Working.Status ==1] <- "Employed"
copy$Working.Status [copy$Working.Status==2] <- "Unemployed"

copy$Marriage.Style [copy$Marriage.Style ==1] <- "Arranged" 
copy$Marriage.Style [copy$Marriage.Style ==2] <- "Love"

copy$Status.of.Having.a.Child [copy$Status.of.Having.a.Child == 1] <- "Yes"
copy$Status.of.Having.a.Child [copy$Status.of.Having.a.Child==2] <- "No"


#Creating a column, labelled BDI sum, containing the sum of scores 
copy$BDI_score <-  rowSums(copy[1:433,6:26])

#Removing the individual scores 
copy <- copy[, -c(6:26)]

#displaying the modified dataset
head(copy)

#Creating a new dataframe containing the count and percentage of each gender
library (ggplot2)

a <-  copy %>% 
  group_by(Gender) %>%
  summarize(counts = n(),
            percentage = n()/nrow(copy))

#Creating a basic bar
piechart <- ggplot(data = a, aes(x="", y= counts, fill = Gender)) + geom_bar(stat="identity") +
  #Converting to pie 
  coord_polar("y", start=0 ) +
  #Adding labels and changing their position
  geom_text(aes(label= counts), position = position_stack(vjust = 0.5)) +
  #Adding a title 
  labs(title= "Gender from online form") +
  #Removing the axis and background 
  theme_void() +
  #Changing the colours 
  scale_fill_manual(values = c("rosybrown2", "lightblue"))

#Displaying the visualisation 
piechart

#Saving the visualisation
ggsave ("piechart.png")

#Creating a histogram
library (ggplot2)

dist <- ggplot(copy,
               aes(x=BDI_score)) + 
  #Changing the binwidth and colour of the bars
  geom_histogram(binwidth=5, colour= "grey", fill ="lightblue") +
  #Adjusting the X axis to specify limits and where the tick marks appear
  scale_x_continuous(breaks= seq(0,65,10)) +
  #Adding a mean line
  geom_vline(aes(xintercept=mean(BDI_score)),
             color="blue", linetype="dashed", size=1) + 
  #Adding a title and labelling the axis 
  labs(title = "Distribution of BDI scores over sample",  
       x = "BDI score", y= "Count") +
  #Changing the size of the text
  theme(text = element_text(size=13)) +
  #Changing the plot theme
  theme_bw() 

#Displaying the visualisation                     
dist

#Saving the visualisation                     
ggsave ("dist.png")

#Determining the most frequent score
x <- copy$BDI_score
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode <- Mode(x) 

#Determining the average score
mean <- mean(copy$BDI_score)

#Determining the maximum score
outlier <- max(copy$BDI_score, na.rm=T)

#Creating a summary table containing the results 
summary <- data.frame(mode,mean,outlier) 

#Defining the type of table and inserting column names
knitr::kable(summary, "pipe", col.names =c("Most common score", "Average score", "Outlier score"),
             #Specifying number of digits displayed
             digits=2,
             #Alligning to the left
             align = "l")

#Creating a new dataframe containing the columns of interest 
library(dplyr)

df2 <-copy %>%
  select(Education, BDI_score)

# Specifying the order of the qualification levels 
df2$Education <- factor(df2$Education,levels = c("Primary", "High-school", "Bachelor",  "Msc or phD"))

#Creating a box plot of Education and BDI score 
boxplot <- ggplot(df2,
                  aes(x=BDI_score, y=Education, fill =Education)) +
  stat_boxplot() +
  # Adjusting the X axis to specify limits and where the tick marks appear
  scale_x_continuous( breaks =seq(0, 65, 20)) +
  #Rotating the boxplot
  coord_flip() +
  #Adding error bars and changing the width
  stat_boxplot(geom ='errorbar', width = 0.2) +
  #Changing the outlier colour and shape
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  #Displaying the mean
  stat_summary(fun="mean", geom ="point", shape= 25, color="white") +
  #Adding a title and labelling the axis 
  labs(title = "Boxplot of BDI scores by education", x = "BDI score", 
       y= "Education") + 
  #Changing the size of the text
  theme(text = element_text(size=13)) +
  #Changing the theme 
  theme_bw() 

#Displaying the visualisation
boxplot

#Saving the visualisation 
ggsave ("boxplot.png")

#Creating a new data frame by grouping working status and education to determine the average BDI score and standard deviation for different working status based on each qualification 

library (dplyr)

df3 <- copy %>% 
  group_by(Working.Status, Education) %>% 
  summarise(average = mean(BDI_score), sd = sd(BDI_score))

#Determining the order of qualification 

df3$Education <- factor(df3$Education,levels = c("Primary", "High-school", "Bachelor",  "Msc or phD"))

#Creating a bar graph 

barplot <- ggplot(df3, aes(x = Education, y = average, 
                           fill = Working.Status)) + geom_col(position = "dodge") +
  #Determining y axis limits and where tick marks appear
  scale_y_continuous(breaks=seq(0,16, 2)) +
  #Changing the colour of the bars
  scale_fill_manual(values = c("salmon1", "slategray2")) +
  #Adding a title and changing the axis and legend label 
  labs(title = "Average BDI score by education and working status",
       x= "Education", y= "Average BDI score",
       fill ="Working status") +
  #Changing the size of the text
  theme(text = element_text(size=13)) +
  #Changing the plot theme
  theme_bw() 

#displaying the visualisation
barplot

#saving the visualisation 
ggsave("barplot.png")

