# Josh Koenig, December 26, 2024

# This is my take on a great idea by Janet E. Hill (University of Saskatchewan).
# https://github.com/HillLabSask/HillLab_gantt_chart/
# https://x.com/HillLabSask/status/1803157398359576941
# She credits "Started out with instructions from
# https://rpubs.com/mramos/ganttchart"


# Load Libraries ----
library(tidyverse)
library(data.table)
library(RColorBrewer)

# Setup and Working Directory ----
setwd("Path/to/working/directory")

# Rather than assembling data in R, which I find cumbersome, I assembled it in
# Excel and saved as "MS-DOS .csv".
# The csv must have the following columns: "Person", "Level", "Start", "End".
# Note the capitalization.
# Very importantly, for this code to work you must input dates as YYYY-MM-DD.

# Read in data ----
# read in the csv, replace with your directory and file.
data <- read.csv(
  file =
    "filename.csv"
)

# Data preparation ----
dt_tidy <- as.data.table(data |>
  mutate(
    Start = as.Date(Start), # Convert date strings into data of type "date".
    End = as.Date(End)
  ) |>
  arrange(Start))

# Adding factors to the Level means that the colours will appear in order.
dt_tidy$Level <- factor(
  dt_tidy$Level,
  c(
    "PI", "Research Associate", "Clinic coordinator", "Technician", "PDF",
    "PhD", "MSc", "Undergraduate", "Co-op"
  )
)

# Create a data frame with persons and their earliest start dates
person_order <- dt_tidy[order(Start), ][!duplicated(Person), ]

# Create the factor with levels ordered by start date
dt_tidy$Person <- factor(dt_tidy$Person,
  levels = person_order$Person
)

# This step provides labels. We only want the labels once, otherwise people's
# names will appear multiple times on the plot.

# Assign NA to all rows, since NA labels are not plotted in ggplot.
dt_tidy$Labels <- NA

# Replace NA only for the first instance of each name.
dt_tidy$Labels[!duplicated(dt_tidy$Person)] <-
  as.character(dt_tidy[!duplicated(dt_tidy$Person)]$Person)

# Generate and save the plot ----
# The previous iteration of this plot used Plotrix. I opted for ggplot because
# of the extensive customization of the toolkit.
ggplot(dt_tidy, aes(y = Person, color = Level)) +
  geom_vline(xintercept = today(), linetype = "dotted") + # adds a dotted line for today's date.
  geom_segment(
    aes(
      x = Start - 5, xend = End + 5, # background segments in black
      y = Person, yend = Person
    ),
    linewidth = 8.2, colour = "black"
  ) +
  geom_segment(
    aes(
      x = Start, xend = End, # coloured segments
      y = Person, yend = Person
    ),
    linewidth = 7.5
  ) +
  geom_text(aes(x = Start, label = Labels),
    hjust = 1.1, color = "black",
    na.rm = TRUE
  ) +
  scale_color_brewer(palette = "Set3") + # Pretty discrete colours.
  scale_y_discrete(limits = rev(levels(dt_tidy$Person))) + # reverses the x axis so the oldest dates are on the top.
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # adds additional grid lines.
  coord_cartesian(xlim = as.Date(c("2020-01-01", "2025-12-31"))) + # add in prefered date range
  ggtitle("Lab Name") + # title for the plot
  theme_bw() + # fast way to get a clean plot.
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) # Centres the title.

ggsave(paste0(format(today(), "%Y%m%d"), "_filename.png"), height = 6.5, width = 9.5) # saves plot as png.
ggsave(paste0(format(today(), "%Y%m%d"), "_filename.eps"), height = 6.5, width = 9.5) # saves plot as eps.
