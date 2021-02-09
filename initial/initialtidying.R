library(tidyverse)
library(scales)

#Loading dataset
postall1617 <- read.csv("POSTALL1617.csv") 

#Adding Glasgow Region Economic Sector variable
postall1617_GRES <- postall1617 %>% 
  mutate(Glasgow_Region_Economic_Sector = ifelse(sclass1 %in% "Business/Finance(general)", "Business Services",
                                          ifelse(sclass1 %in% "Management (general)", "Business Services",
                                          ifelse(sclass1 %in% "Public Administration", "Business Services",
                                          ifelse(sclass1 %in% "International Business Studies / Briefing", "Business Services",
                                          ifelse(sclass1 %in% "Enterprises", "Business Services",
                                          ifelse(sclass1 %in% "Management Skills", "Business Services",
                                          ifelse(sclass1 %in% "Management Planning and Control Systems", "Business Services",
                                          ifelse(sclass1 %in% "Human Resources Management", "Business Services",
                                          ifelse(sclass1 %in% "Financial Management/Accounting", "Business Services",
                                          ifelse(sclass1 %in% "Financial Services", "Business Services",
                                          ifelse(sclass1 %in% "Office Skills", "Business Services",
                                          ifelse(sclass1 %in% "Typing/Shorthand/Secretarial Skills", "Business Services",
                                          ifelse(sclass1 %in% "Marketing/PR", "Business Services",
                                          ifelse(sclass1 %in% "Retailing/Wholesaling/Distributive Trades", "Business Services",
                                          ifelse(sclass1 %in% "Retailing:/Distribution:Specific Types", "Business Services",
                                          ifelse(sclass1 %in% "Sales Work", "Business Services",
                                          ifelse(sclass1 %in% "Physical Distribution", "Business Services",
                                          ifelse(sclass1 %in% "IT: Computer Science/Programming/Systems", "Business Services",
                                          ifelse(sclass1 %in% "IT: Computer use", "Business Services",
                                          ifelse(sclass1 %in% "Using Software and Operating Systems", "Business Services",
                                          ifelse(sclass1 %in% "Text/ Graphics?multimedia Presentation software", "Business Services",
                                          ifelse(sclass1 %in% "Software for Specific Applications/Industries", "Business Services",
                                          ifelse(sclass1 %in% "Information Work/Information Use", "Business Services",
                                          ifelse(sclass1 %in% "Information Systems Management", "Business Services",
                                          ifelse(sclass1 %in% "Humanities/ General Studies/Combined Studies", "Business Services",
                                          ifelse(sclass1 %in% "History", "Business Services",
                                          ifelse(sclass1 %in% "Archaeology", "Business Services",
                                          ifelse(sclass1 %in% "Religious Studies", "Business Services",
                                          ifelse(sclass1 %in% "Philosophy", "Business Services",
                                          ifelse(sclass1 %in% "Government/Politics", "Business Services",
                                          ifelse(sclass1 %in% "Economics", "Business Services",
                                          ifelse(sclass1 %in% "Law", "Business Services",
                                          ifelse(sclass1 %in% "Social Sciences general/Combined", "Business Services",
                                          ifelse(sclass1 %in% "Social Studies", "Business Services",
                                          ifelse(sclass1 %in% "Culture/Gender/Folklore", "Business Services", 
                                          ifelse(sclass1 %in% "Cultural/Area/Social/Diaspora Studies", "Business Services",
                                          ifelse(sclass1 %in% "Production/Operations Management", "Business Services",                      
                                          ifelse(sclass1 %in% "Purchasing/Procurement and Sourcing", "Business Services",                 
                                          ifelse(sclass1 %in% "Quality and Reliability Management", "Business Services",                   
                                          ifelse(sclass1 %in% "Self Development", "Access and Support Learning",
                                          ifelse(sclass1 %in% "Career Change/Access", "Access and Support Learning",
                                          ifelse(sclass1 %in% "Continuing Education (basic skills)", "Access and Support Learning",
                                          ifelse(sclass1 %in% "Disabled People: Skills/Facilities", "Access and Support Learning", 
                                          ifelse(sclass1 %in% "Languages", "Access and Support Learning",
                                          ifelse(sclass1 %in% "Linguistic Studies", "Access and Support Learning", 
                                                 "")))))))))))))))))))))))))))))))))))))))))))))) 

#Because you can only do 50 nested ifelse statements, we have to break it up
#Creating first of three 'real' datasets, filtered for populated GRES variable
postall1617_GRES_real1 <- postall1617_GRES %>% 
  filter(Glasgow_Region_Economic_Sector != "")

#Second ifelse iteration
postall1617_GRES_filtered1 <- postall1617_GRES %>% 
  filter(Glasgow_Region_Economic_Sector == "") %>% 
  mutate(Glasgow_Region_Economic_Sector = ifelse(sclass1 %in% "Art Studies/Fine Arts", "Creative Industries",
                                          ifelse(sclass1 %in% "Art Techniques/Practice", "Creative Industries",
                                          ifelse(sclass1 %in% "Design (non/industrial)", "Creative Industries",
                                          ifelse(sclass1 %in% "Crafts:Leisure/General", "Creative Industries",
                                          ifelse(sclass1 %in% "Decorative Leisure Crafts", "Creative Industries",
                                          ifelse(sclass1 %in% "Decorative Metal Crafts/Jewellery", "Creative Industries",
                                          ifelse(sclass1 %in% "Fashion/Textiles/Clothing (craft)", "Creative Industries",
                                          ifelse(sclass1 %in% "Fabric Crafts/Soft Furnishings", "Creative Industries",
                                          ifelse(sclass1 %in% "Glass/Ceramics/Stone Crafts", "Creative Industries", 
                                          ifelse(sclass1 %in% "Communication Skills", "Creative Industries",
                                          ifelse(sclass1 %in% "Communication/Media (general)", "Creative Industries",
                                          ifelse(sclass1 %in% "Writing (authorship)", "Creative Industries",
                                          ifelse(sclass1 %in% "Journalism", "Creative Industries",
                                          ifelse(sclass1 %in% "Photography", "Creative Industries",
                                          ifelse(sclass1 %in% "Film/Video Production", "Creative Industries",
                                          ifelse(sclass1 %in% "Audio and Visual Media", "Creative Industries",
                                          ifelse(sclass1 %in% "Print and Publishing", "Creative Industries",
                                          ifelse(sclass1 %in% "Performing Arts (general)", "Creative Industries",
                                          ifelse(sclass1 %in% "Dance", "Creative Industries",
                                          ifelse(sclass1 %in% "Theatre and Dramatic Arts", "Creative Industries",
                                          ifelse(sclass1 %in% "Theatre Production", "Creative Industries",
                                          ifelse(sclass1 %in% "Music History/ Theory", "Creative Industries",
                                          ifelse(sclass1 %in% "Music of Specific Kinds/Cultures", "Creative Industries",
                                          ifelse(sclass1 %in% "Music Performance", "Creative Industries",
                                          ifelse(sclass1 %in% "Musical Instrument Technology", "Creative Industries",
                                          ifelse(sclass1 %in% "Interior Design/Fitting/Decoration", "Creative Industries",
                                          ifelse(sclass1 %in% "Glass/Ceramics/Stone Crafts", "Creative Industries", 
                                          ifelse(sclass1 %in% "Education Theory/Learning Issues", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Teaching/Training", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Teaching/Training: specific subjects", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Education/School Organisation", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Training/ Vocational Qualifications", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Careers/Education Guidance Work", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Parenting/Carers", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Crisis/Illness/Self Help", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Health Care Management/Health Studies", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Nursing", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Semi-medical/Physical/Psycho/Therapies", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Psychology", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Occupational Health and Safety", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Social care/Social Work Skills", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Family/Community Work", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Crisis Support/Counselling", "Health, Care and Education",
                                          ifelse(sclass1 %in% "Child Care Services", "Health, Care and Education",
                                                 ""))))))))))))))))))))))))))))))))))))))))))))) 

#Second filtering iteration
postall1617_GRES_real2 <-  postall1617_GRES_filtered1 %>%
  filter(Glasgow_Region_Economic_Sector != "")

#Final iteration
postall1617_GRES_real3 <- postall1617_GRES_filtered1 %>% 
  filter(Glasgow_Region_Economic_Sector == "") %>% 
  mutate(Glasgow_Region_Economic_Sector =ifelse(sclass1 %in% "Personal Health/Fitness/Appearance", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Therapeutic Personal Care", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Hair/Personal Care Services", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Sports Studies/Combined Sports", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Water Sports", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Athletics Gymnastics and Combat Sports", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Ball and Related Games", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Hospitality/Catering (general)", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Food/Drink Services", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Catering Services", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Hospitality Services", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Home Economics", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Baking/Dairy/Food and Drink Processing", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Cookery", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Food Sciences/Technology", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Tourism/Travel", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Leisure/Sports Facilities Work", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Arts/Culture/Heritage Administration", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Complementary Medicine", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Cleansing", "Tourism, Hospitality and Leisure",
                                          ifelse(sclass1 %in% "Food/Drink/Tobacco (industrial)", "Tourism, Hospitality and Leisure", 
                                                 "STEM"))))))))))))))))))))))

#Concatenation and filtering for Glasgow college
postall1617_GRES_REALEST <-
  do.call(
    "rbind",
    list(
      postall1617_GRES_real1,
      postall1617_GRES_real2,
      postall1617_GRES_real3
    )
  ) %>% filter(
    college_name == "City of Glasgow College" |
    college_name == "Glasgow Kelvin College" |
    college_name == "Glasgow Clyde College") 

#######################################################

#SCQF level

#Ranking factors for scqf_lev
postall1617_GRES_REALEST$SCQF_order <-
  ifelse(
    postall1617_GRES_REALEST$scqf_lev %in% "National 1",
    1,
    ifelse(
      postall1617_GRES_REALEST$scqf_lev %in% "National 2",
      2,
      ifelse(
        postall1617_GRES_REALEST$scqf_lev %in% "National 3",
        3,
        ifelse(
          postall1617_GRES_REALEST$scqf_lev %in% "National 4 SVQ 1",
          4,
          ifelse(
            postall1617_GRES_REALEST$scqf_lev %in% "National 5 SVQ 2",
            5,
            ifelse(
              postall1617_GRES_REALEST$scqf_lev %in% "Higher SVQ 3",
              6,
              ifelse(
                postall1617_GRES_REALEST$scqf_lev %in% "Advanced Higher HNC. CertHE. SVQ 3",
                7,
                ifelse(
                  postall1617_GRES_REALEST$scqf_lev %in% "HND Dip HE SVQ 4",
                  8,
                  ifelse(
                    postall1617_GRES_REALEST$scqf_lev %in% "Ordinary Degree Graduate Diploma/Certificate. SVQ 4",
                    9,
                    ifelse(
                      postall1617_GRES_REALEST$scqf_lev %in% "Honours Degree Graduate Diploma/Certificate",
                      10,
                      11
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

#Switching everything at SCQF levels 1-3 to Access and Support Learning
postall1617_GRES_REALEST$Glasgow_Region_Economic_Sector <-
  ifelse(
    postall1617_GRES_REALEST$scqf_lev %in% "National 1",
    "Access and Support Learning",
    ifelse(
      postall1617_GRES_REALEST$scqf_lev %in% "National 2",
      "Access and Support Learning",
      ifelse(
        postall1617_GRES_REALEST$scqf_lev %in% "National 3",
        "Access and Support Learning",
        postall1617_GRES_REALEST$Glasgow_Region_Economic_Sector
      )
    )
  )
