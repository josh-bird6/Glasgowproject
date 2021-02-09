library(treemapify)
library(igraph)

#####VISUALISATION 1###############
nodes <- read.csv("node_list.csv")
edges <- read.csv("edge_list.csv")

nodes <- arrange(nodes, id)

network <- graph_from_data_frame(d=edges, vertice = nodes, directed =F)

#to drag and drop and such
tkplot(
  network,
  vertex.label = V(network)$node_name,
  vertex.color = V(network)$node_colour,
  vertex.shape = V(network)$shape,
  vertex.label.color = "black",
  edge.curved = .3
)

coords <- tk_coords(tkp.id = 1)

par(bg="#E2E2E3")

plot(
  network,
  vertex.label = V(network)$node_name,
  vertex.color = V(network)$node_colour,
  vertex.shape = V(network)$shape,
  vertex.label.color = "black",
  edge.curved = .3,
  layout = coords
)

#Creating function
plot1 <- function(x) {
  plot(
    x,
    vertex.label = V(network)$node_name,
    vertex.color = V(network)$node_colour,
    vertex.shape = V(network)$shape,
    vertex.label.color = "black",
    edge.curved = .3,
    layout = coords
  )
} 
       

#####VISUALISATION 2###############
GRES_treemap <- postall1617_GRES_REALEST %>% 
  group_by(Glasgow_Region_Economic_Sector) %>% 
  summarise(total = n()) %>% 
  drop_na() %>% 
  mutate(overall = 70021,
         percent = as.numeric(str_sub(total/overall * 100, 1,4)))

p2 <- ggplot(data=GRES_treemap, aes(area=percent, fill=percent, label=Glasgow_Region_Economic_Sector, subgroup = paste0(percent, '%'))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place="centre", reflow = T) +
  geom_treemap_subgroup_text(colour= "white", fontface="italic", size = 12, place="topleft") +
  labs(title = "OVERALL STUDENT PROPORTIONS AT GLASGOW COLLEGES\n BY GRES CLASSIFICATION", caption = "Note: All courses at SCQF levels 1-3 are included in 'Access and Support Learning'") +
  guides(fill=F) +
  theme(plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
        plot.title = element_text(colour = "#4292c6", face = "bold", size = 25, hjust = .5)) 
  

#####VISUALISATION 3_1###############
College_provision <- postall1617_GRES_REALEST %>% 
  group_by(Glasgow_Region_Economic_Sector, college_name) %>% 
  summarise(total = n()) %>% 
  drop_na()

p3_1 <- ggplot(data = College_provision, aes(x=Glasgow_Region_Economic_Sector, y=total)) +
  geom_col(fill = "#4292c6") +
  coord_flip()+
  facet_wrap(~college_name, nrow = 3) +
  scale_y_continuous(label = scales::comma) +
  labs(title = "NUMBER OF STUDENTS BY GLASGOW \nCOLLEGE AND GRES CLASSIFICATION", x = "", y="") +
  theme(strip.background = element_rect(fill = "yellow"),
        plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
        panel.background = element_rect(fill = "#E2E2E3"),
        plot.title = element_text(colour = "#4292c6", face = "bold", size = 15, hjust = .5)) +
  geom_text(aes(label = scales::comma(total)), hjust = 1) 

#####VISUALISATION 3_2###############
College_totals <- postall1617_GRES_REALEST %>% 
  group_by(college_name) %>% 
  summarise(total = n()) %>% 
  drop_na() %>% 
  mutate(totals = 63444,
         percent = as.numeric(str_sub(total / totals * 100, 1,4)),
         pos = c(77, 41, 17))

          
p3_2 <- ggplot(data = College_totals, aes(x=2, y = percent, fill = college_name)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percent, '%'), y=pos), size = 10) +
  xlim(0.5, 2.5) +
  coord_polar(theta = "y") +
  labs(x=NULL, y=NULL, fill = "", title = "PROPORTIONAL STUDENT POPULATIONS \nAT GLASGOW COLLEGES(n=63,444)") +
  scale_fill_manual(values = c("#084594", "#4292c6", "#9ecae1")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(colour = "#4292c6", face = "bold", size = 15, hjust = .5),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background = element_rect(fill = "#E2E2E3"),
        plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"))
