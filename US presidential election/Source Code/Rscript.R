
#get the shape file for the states excluding alaska and hawaii
usa.map = map_data("state")
#change column 5 name to "State"
colnames(usa.map)[5] = "State"
# set working directory to save image
#setwd('/Users/deepaks/Desktop/UTD/Semester 2/Statistical Methods for Data Science/Miniproject/Project 2/')
setwd("C:/Users/mastr/Desktop/final mini project 2")
#get data from csv file
election_data <- read_csv("~/us_2016_election_data.csv", 
                                  col_types = cols(`Clinton %` = col_number(), 
                                                   `Others %` = col_number(), `Trump %` = col_number()))
election_data$State = tolower(election_data$State)
trump_data = election_data
#save the dfference of percentage of votes that Trump and Clinton received 
trump_data$`Trump %`=(trump_data$`Trump %`-trump_data$`Clinton %`)
trump_data$`Clinton %`= NULL
trump_data$`Others %`= NULL
#join 2 tables by column="State" to store votes percentage
trump_states = join(usa.map, trump_data, by = "State", type = "inner")
#get longitoue, latitude and abbreviation of states
states = data.frame(state.center, state.abb)
#create array of small states whose names are to be dislayed explicitely with lines
lined_states = c('VT','NH','MA','RI','CT','NJ','DE','MD','DC')
not_lined_states=c('VT','NH','MA','RI','CT','NJ','DE','MD','DC','AK','HI')
#select states to be displayed with lines in map
projected_states = states[states$state.abb %in% lined_states, ]
#slect states to be displayed without lines in map
unprojected_states = states[!states$state.abb %in% not_lined_states, ]

#gt the map data of alaska
alaska_map = map_data("world2Hires", "USA:Alaska")
colnames(alaska_map)[6] = "State"
alaska_map$State = tolower(alaska_map$State)
#store value that contain "alaska" as their state name
alaska_data = trump_data[trump_data$State == "alaska", c("State", "Trump %")]
#join 2 tables by column="State" to store votes percentage
alaska_map = join(alaska_map, alaska_data, by = "State", type = "inner")
stateswithalaska = states[states$state.abb %in% 'AK', ]

#get map data of hawaii
hawaii_map = map_data("world2Hires", "Hawaii")
colnames(hawaii_map)[5] = "State"
hawaii_map$State = tolower(hawaii_map$State)
#store value that contain "hawaii" as their state name
hawaii_data = trump_data[trump_data$State == "hawaii", c("State", "Trump %")]
#join 2 tables by column="State" to store votes percentage
hawaii_map = join(hawaii_map, hawaii_data, by = "State", type = "inner")
stateswithhawaii = states[states$state.abb %in% 'HI', ]

#defining breaks to be used in map for color variaton
brks.to.use = seq(0,100, by =25)
#display map title
figure.title = "Winning Margin of President Elect- Donald Trump over Clinton"
#call the function.R script which contains functions to display colors in map
source("functions.R")
#call 3 functions that fills color for map, alaska map, and hawaai map separetely
result1 = StatesFunction(trump_states,brks.to.use,figure.title)
result2 = AlaskaFunction(alaska_map,brks.to.use)
result3 = HawaiiFunction(hawaii_map,brks.to.use)
#used to position/layout 3 maps(states, alaska, hawaii) with respect to each other
lay = rbind(c(1,1,1,1,1),c(1,1,1,1,1),c(1,1,1,1,1),c(1,1,1,1,1),c(2,3,NA,NA,NA),c(NA,3,NA,NA,NA))
g = arrangeGrob(grobs = list(result1,result2,result3),layout_matrix = lay)
#save the map image
ggsave("election_result.jpg",g)
