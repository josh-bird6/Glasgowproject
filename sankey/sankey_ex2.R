##################
#READING FROM CSV#
##################

sankey_nodes <- read.csv("glasgow_sankey.csv") %>% 
  select(-X)


sankey_plot2 <- sankey_nodes %>% 
  plot_ly(
  type = "sankey",
  domain = list(x = c(0, 1),
                y = c(0, 1)),
  orientation = "h",
  valuesuffix = " Students",
  valueformat = ",0f",
  arrangement = "freeform",
  
  node = list(
    label = ~Node_label,
    color = ~Colour,
    pad = 15,
    thickness = 20,
    line = list(color = "black",
                width = 0.5)
  ),
  link = list(
    source = ~Source,
    target = ~Target,
    value = ~Value,
    color = ~link_colour
  )
) %>%   layout(title = "Glasgow College flows into GRES classification",
               font = list(size = 10))

###############################

sankey_test <- sankey_nodes %>%
  plot_ly(
    type = "sankey",
    transforms = list(
      type = 'filter',
      target = ~ filter,
      operation = '=',
      value = unique(sankey_nodes$filter)[1]
    ),
    domain = list(x = c(0, 1),
                  y = c(0, 1)),
    orientation = "h",
    valuesuffix = " Students",
    valueformat = ",0f",
    arrangement = "freeform",
    
    node = list(
      label = ~ Node_label,
      color = ~ Colour,
      pad = 15,
      thickness = 20,
      line = list(color = "black",
                  width = 0.5)
    ),
    link = list(
      source = ~ Source,
      target = ~ Target,
      value = ~ Value,
      color = ~ link_colour
    )
  ) %>% layout(
    title = "Glasgow College flows into GRES classification",
    font = list(size = 10),
    updatemenus = list(list(
      type = 'dropdown',
      active = 0,
      buttons = list(
        list(
          method = 'restyle',
          args = list("transforms[0].value", unique(sankey_nodes$filter)[1]),
          label = unique(sankey_nodes$filter)[1]),
        list(
          method = "restyle",
          args = list("transforms[0].value", unique(sankey_nodes$filter)[2]),
          label = unique(sankey_nodes$filter)[2])
      )
    ))
  )

add_lines(sankey_test)
