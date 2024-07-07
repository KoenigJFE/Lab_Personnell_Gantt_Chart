#Josh Koenig, July 7, 2024

#This is my take on a great idea by Janet E. Hill (University of Saskatchewan).
#https://github.com/HillLabSask/HillLab_gantt_chart/
#https://x.com/HillLabSask/status/1803157398359576941
#She credits "Started out with instructions from https://rpubs.com/mramos/ganttchart"


#Load Libraries ----
library(tidyverse)
library(data.table)
library(magrittr)
library(ggplot2)
library(RColorBrewer)



#Setup and Working Directory ----
setwd("R/")

#Rather than assembling data in R, which I find cumbersome, I assembled it in Excel and saved as "MS-DOS .csv".
#The csv must have the following columns: "Person", "Level", "Start", "End". Note the capitalization.
#Very importantly, for this code to work you should input dates as YYYY-MM-DD.

#Read in data ----
data <- read.csv(file = "Gantt Chart/Trainees and Staff.csv") #read in the csv, replace with your directory and file.


#Data preparation ----

dt_tidy <- as.data.table(data %>%
              mutate(Start = as.Date(Start), #Convert date strings into data of type "date".
                     End = as.Date(End)) %>%
              arrange(Start) %>% #we will want the end plot to be arranged by start date.
              gather(key = Date_type, #Gathered format is necessary for ggplot2 representation.
                     value = Date,    #Basically, this means that all dates are in one column with
                     -Level,          #a second column that determines if its a start or end date.
                     -Person))

#Adding factors to the Level means that the colours will appear in order.
dt_tidy$Level <- factor(dt_tidy$Level,
                        c("PI", "Research Associate", "Technician", "PDF",
                          "PhD", "MSc", "Undergraduate", "Co-op"))

#Here I've added factors to the Person column to ensure the plot shows individuals in order.
#Since we had previously arranged by date, the first instance of each name is when they started.
dt_tidy$Person <- factor(dt_tidy$Person, #We're overwriting person with the same data, just this time its factored.
                         dt_tidy[,.SD[which.min(Date)], by = Person]$Person)#I personally like data.table for functions like these, hence the funky syntax. 
                                                                            #Basically, this line finds and returns only the first instance of each persons 
                                                                            #By looking for the earliest date associated with the person.

#This step provides labels. We only want the labels once, otherwise people's names will appear multiple times on the plot.
dt_tidy$Labels <- NA #Assign NA to all rows, since NA labels are not plotted in ggplot.
dt_tidy$Labels[!duplicated(dt_tidy$Person)] <- as.character(dt_tidy[!duplicated(dt_tidy$Person)]$Person) #Replace NA only for the first instance of each name.


#Generate and save the plot ----
#The previous iteration of this plot used Plotrix. I opted for ggplot because of the extensive customization of the toolkit.

ggplot(dt_tidy, aes(x= Person, y=Date, color = Level, label=Labels))+
  geom_hline(yintercept = today(), linetype = "dotted")+ #adds a dotted line for today's date.
  geom_line(linewidth = 8.2, color = "black")+ #necessary for the black outline around the bars.
  geom_line(linewidth = 7.5)+ #the coloured bar.
  geom_text(hjust=1.1, color = "black")+ #puts the name beside the bar.
  scale_color_brewer(palette = "Set3")+ #Pretty discrete colours.
  scale_x_discrete(limits = rev(levels(dt_tidy$Person)))+ #reverses the x axis so the oldest dates are on the top.
  scale_y_date(date_breaks = "1 year", date_labels = "%Y")+ #adds additional grid lines.
  ggtitle("Josh Koenig's Trainees and Staff")+ #title for the plot
  coord_flip(ylim = as.Date(c('2013-06-01', '2027-01-01')))+ #Flips x and y axis and zooms into the years of interest.
  theme_bw()+ #fast way to get a clean plot.
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) #Centres the title.

ggsave("Gantt Chart/TraineeChart.png", height = 6.5, width = 9.5) #saves plot as png.

