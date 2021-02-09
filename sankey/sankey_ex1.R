library(plotly)

#Transform for CLD comparison - ALL QUALIFIERS
glasgow_colleges_quals <- postall1617_GRES_REALEST %>%
  filter(
    mode %in% c(
      "Short full-time",
      "Full Time",
      "Part-time but previously met full-time criteria"
    ) &
      outcome %in% c(
        "Completed programme/course student assessed and successful",
        "Student completed first year of an HND but has chosen to leave with an HNC"
      ) &
      !studcat %in% c(
        "School Based S3",
        "School Based S4",
        "School Based S5",
        "School Based S6",
        "school based S1",
        "school based S2",
        "Primary school pupil",
        "P1-P7. S1-S6. but not a school link programme"
      ))
# %>%
#   group_by(college_name, Glasgow_Region_Economic_Sector) %>%
#   summarise(total = n()) %>%
#   drop_na()

##################################################
#BRINGING IN CLD DATASET TO MATCH FOR DESTINATIONS
##################################################
CLD <- read.csv("CLD.csv")

#Matching
glasgow_colleges_quals$CLD_class <- CLD$primary_classification_real[match(glasgow_colleges_quals$uniqid, CLD$uniqid_trim)]

########################################
#FORMATTING THE HARD WAY
sankey_plot <- plot_ly(
  type = "sankey",
  orientation = "h",
  valuesuffix = " Students",
  valueformat = ".0f",
  
  
  node = list(
    label = c(
      "Glasgow Clyde College",
      "Glasgow Kelvin College",
      "City of Glasgow College",
      "Access and Support Learning",
      "Business Services",
      "Creative Industries",
      "Health, Care and Education",
      "STEM",
      "Tourism, Hospitality and Leisure",
      "Qualifier",
      "Non-qualifier",
      "Not employed but looking for employment/study/training",
      "Permanently unable to work/retired",
      "Studying FT",
      "Studying PT",
      "Travelling",
      "Temporarily sick/unable to work",
      "Unconfirmed",
      "Unemployed and looking for work",
      "Working FT",
      "Working PT"
      
      
    ),
    color = c(
      "blue",
      "blue",
      "blue",
      "yellow",
      "yellow",
      "yellow",
      "yellow",
      "yellow",
      "yellow",
      "green",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red",
      "red"
      
    ),
    pad = 15,
    thickness = 20,
    line = list(color = "black",
                width = 0.5)
  ),
  
  link = list(
    source = c(
      0,
      0,
      0,
      0,
      0,
      0,
      1,
      1,
      1,
      1,
      1,
      1,
      2,
      2,
      2,
      2,
      2,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9,
      9
      
    ),
    target = c(
      3,
      4,
      5,
      6,
      7,
      8,
      3,
      4,
      5,
      6,
      7,
      8,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      9,
      9,
      9,
      9,
      9,
      10,
      10,
      10,
      10,
      10,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20
      
    ),
    value =  c(
      5405,
      1997,
      2129,
      3165,
      6763,
      1982,
      9273,
      1268,
      1062,
      1174,
      5002,
      1221,
      4592,
      4750,
      3071,
      2203,
      9951,
      5013,
      559,
      1797,
      1912,
      1436,
      3261,
      2260,
      18711,
      6218,
      4350,
      5106,
      18455,
      5956,
      9,
      2,
      7285,
      188,
      20,
      49,
      1169,
      174,
      1549,
      442
    )
  )
) %>%
  layout(title = "Glasgow College flows into GRES classification",
         font = list(size = 10))

chart_link = api_create(sankey_plot, filename = "GCRB-sankey")
