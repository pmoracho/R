#######################
# CREATING PLOT THEMES
#######################
library(extrafont)

# Create Base Theme
#------------------
simple_theme <- function() {

    # Brand Colors
    color.background = "#FFFFFF" # Chart Background
    color.grid.major = "#D9D9D9" # Chart Gridlines
    color.axis.text = "#666666" # 
    color.axis.title = "#666666" # 
    color.title = "#666666"
    color.subtitle = "#666666"
    
    # Begin construction of chart
    theme_bw(base_size=12) +
        
        # Set the entire chart region to a light gray color
        
        theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        theme(panel.border=element_rect(color=color.background)) +
        
        # Format the grid
        theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
        theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
        theme(axis.ticks=element_blank()) +
        
        # Format the legend, but hide by default
        theme(legend.position="none") +
        theme(legend.background = element_rect(fill=color.background)) +
        theme(legend.text = element_text(size=7,color=color.axis.title, family="Raleway")) +

        # Set title and axis labels, and format these and tick marks
        theme(plot.title=element_text(color=color.title, 
                                      size=20, 
                                      vjust=1.25, 
                                      family="Raleway", 
                                      hjust = 0.5
                                      )) +
        
        theme(plot.subtitle=element_text(color=color.subtitle, size=12, family="Raleway",  hjust = 0.5))  +
        theme(axis.text.x=element_text(size=10,color=color.axis.text, family="Raleway")) +
        theme(axis.text.y=element_text(size=10,color=color.axis.text, family="Raleway")) +
        theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=0, family="Raleway")) +
        theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.25, family="Raleway")) +
        theme(plot.caption=element_text(size=8,color=color.axis.title, vjust=1.25, family="Raleway")) +
        
        # Legend  
        
        theme(legend.text=element_text(size=10,color=color.axis.text, family="Raleway", margin = margin(r = 0.1, l = 0.1, unit = 'cm'))) +
        theme(legend.title=element_text(size=10,color=color.axis.text, family="Raleway")) +
        theme(legend.key=element_rect(colour = NA, fill = NA, size = 5)) +
        theme(legend.position="bottom", 
              legend.box = "horizontal", 
              legend.title=element_blank(),
              legend.spacing = unit(0.5, 'cm'), 
              legend.key.size = unit(1.5, 'lines')) +
        
        # Plot margins
        theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
    
}

# Set Theme
# theme_set(simple_theme())