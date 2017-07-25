#to fill color in states exclusing alaska and hawaai
StatesFunction = function(data, brks, title) {
  ggp = ggplot() + 
    #create a shape of map and fill color based on percentage votes using "fill" argument
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = trump_states$`Trump %`), color = "black", size = 0.15) +
    #decide graduent colors in the map and to display it on the scale
    scale_fill_gradient2(midpoint=3.6,low="deepskyblue1",mid="aliceblue",high="red2",breaks=c(-86.8,-53.525,-20.25,13.025,46.3),
                         labels=c("-80% Clinton","-50%","-20%","10%","40% Trump"),limits=c(-86.8,46.3))+
    theme_nothing(legend = TRUE) + labs(title = title, fill = "") +
    #label states with the abbreviations
    geom_text(data = unprojected_states, aes(x = x, y = y, label = state.abb), size = 3) +
    # label states using lines and abbreviations
    geom_text_repel(data = projected_states, aes(x = x, y = y,label = state.abb), size = 3,box.padding = unit(.6, "lines"),
                    point.padding = unit(.175, "lines"),nudge_x = 5.5,nudge_y = .7)
  return(ggp)
}

#to fill color in the state of Alaska
AlaskaFunction = function(data, brks) {
  ggp = ggplot() + 
    #create map boundaries and fill color based on votes data
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = alaska_map$`Trump %`), color = "black", size = 0.15) + 
    #decide graduent colors in the map and to display it on the scale
    scale_fill_gradient2(midpoint=3.6,low="deepskyblue1",mid="aliceblue",high="red2",breaks=c(-86.8,-53.525,-20.25,13.025,46.3),
                         labels=c("-80% Clinton","-50%","-20%","10%","40% Trump"),limits=c(-86.8,46.3))+
    theme_nothing(legend = FALSE) + labs(fill = "") +
    #label alaska state with its abbreviation
    annotate("text", x = min(data$long)+ 30, y = min(data$lat) + 12, label = "AK")
  return(ggp)
}

#to fill colors in the state of Hawaii
HawaiiFunction = function(data, brks) {
  ggp = ggplot() + 
    #create a shape of alaska map and fill colors based on votes data using "fill" argument
    geom_polygon(data = data, aes(x = long, y = lat, group = group, 
                                  fill = hawaii_map$`Trump %`), color = "black", size = 0.15) + 
     #decide graduent colors in the map and to display it on the scale
     scale_fill_gradient2(midpoint=3.6,low="deepskyblue1",mid="aliceblue",high="red2",breaks=c(-86.8,-53.525,-20.25,13.025,46.3),
                         labels=c("-80% Clinton","-50%","-20%","10%","40% Trump"),limits=c(-86.8,46.3))+
    theme_nothing(legend = FALSE) + labs(fill = "") +
    #label hawaii state with its abbreviation
    annotate("text", x = min(data$long)+13, y = min(data$lat) + 2, label = "HI")
  return(ggp)
}

