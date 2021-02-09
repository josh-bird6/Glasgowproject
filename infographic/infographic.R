#INFOGRAPHIC?!

library(grid)
library(extrafont)

#importing fonts
font_import()
fonts()
loadfonts()


#creating function for Viewport layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)



#Creating infographic

##PDF Output
pdf(
  "M:/Performance Measurement & Analysis/Josh/Glasgow project/infographic.pdf",
  width = 12,
  height = 28
)

##Creating layout
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("INFOGRAPHIC", y = unit(1, "npc"), x = unit(0.5, "npc"), vjust = 1, hjust = .5, gp = gpar(fontfamily = "Impact", col = "#A9A8A7", cex = 12, alpha = 0.3))
grid.text(
  "An Introduction to the Glasgow \nRegional Economic Sector Classification",
  y = unit(0.94, "npc"),
  gp = gpar(col = "#4292c6", cex = 3.3)
)
##Visualisations
par(bg="#E2E2E3")
# print(plot1(network), vp = vplayout(1, 1:4))
print(p2, vp = vplayout(2, 1:4))
print(p3_1, vp = vplayout(3, 1:2))
print(p3_2, vp = vplayout(3, 3:4))
print(p4, vp = vplayout(4, 1:4))
grid.rect(
  gp = gpar(fill = "#4292c6", col = "#4292c6"),
  x = unit(0.5, "npc"),
  y = unit(0.82, "npc"),
  width = unit(1, "npc"),
  height = unit(0.1, "npc")
)
grid.text(
  "INTRODUCTION",
  vjust = 0,
  hjust = 0,
  x = unit(0.01, "npc"),
  y = unit(0.86, "npc"),
  gp = gpar(col = "yellow", cex = 1.2)
)
grid.text(
  paste(
    "The Glasgow College Regional Board (GCRB) is a joint partnership comprised of representatives of Glasgow Clyde COllege, Glasgow Kelvin College and City \nof Glasgow College. GCRB is currently assessing differing aspects of regional skills training provision across Glasgow with the aim of ensuring that supply is meeting labour \nforce demands.",
    sep = "\n"
  ),
  vjust = 0,
  hjust = 0,
  x = unit(0.01, "npc"),
  y = unit(0.83, "npc"),
  gp = gpar(col = "yellow", cex = 0.8)
)
grid.text(
  paste(
    "A key aim of regional curriculum planning is to ensure that the combined regional offer continues to meet identified economic and social needs. To aid evaluation, \nthe curriculum has been grouped into six ‘sectors’, most related to employment areas, with one related to access and supported learning.",
    sep = "\n"
  ),
  vjust = 0,
  hjust = 0,
  x = unit(0.01, "npc"),
  y = unit(0.81, "npc"),
  gp = gpar(col = "yellow", cex = 0.8)
)
grid.text(
  paste(
    "A preliminary look at this Glasgow Regional Economic Sector classification can be found in the graphs below",
    sep = "\n"
  ),
  vjust = 0,
  hjust = 0,
  x = unit(0.01, "npc"),
  y = unit(0.8, "npc"),
  gp = gpar(col = "yellow", cex = 0.8)
)
dev.off()
