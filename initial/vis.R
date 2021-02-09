#Some initial analysis
glasgow_colleges_overall <- postall1617_GRES_REALEST %>% 
  group_by(college_name, Glasgow_Region_Economic_Sector) %>% 
  summarise(total = n()) %>% 
  drop_na()

p1 <- ggplot(data = glasgow_colleges,
       aes(x = Glasgow_Region_Economic_Sector, y = total, fill = college_name)) +
  geom_col(position = "dodge") +
  coord_flip() +
  # facet_wrap(~adv, nrow = 2) +
  labs(title = "FIGURE 1: Students at Glasgow Colleges by GRES Classification, 2016-17", x =
         "GRES Classification", y = "Total") +
  scale_fill_manual(
    labels = c(
      "City of Glasgow College\n(n=29,580)",
      "Glasgow Clyde College\n(n=21,441)",
      "Glasgow Kelvin College\n(n=19,000)"),
    values = c("#084594", "#4292c6", "#9ecae1")) +
  guides(fill=guide_legend("College Name")) +
  scale_y_continuous(label = scales::comma) +
  geom_text(aes(label = scales::comma(total)), position = position_dodge(width = .9), hjust = -.1)


#######################################################

#Analysis
glasgow_colleges_scqf <- postall1617_GRES_REALEST %>% 
  group_by(scqf_lev, SCQF_order, Glasgow_Region_Economic_Sector, credits) %>% 
  summarise(total = n()) %>% 
  drop_na() %>% 
  mutate(credit_total = as.numeric(credits * total),
         credits_total = sum(credit_total),
         totals = sum(total)) %>% 
  select(-credits, -total, -credit_total) %>% 
  distinct(totals, credits_total) %>% 
  arrange(desc(SCQF_order))
  
#RAW STUDENT TOTALS
p4 <-
  ggplot(data = glasgow_colleges_scqf, aes(x = reorder(scqf_lev, SCQF_order), y =
                                             totals)) +
  geom_col(fill = "#4292c6") +
  facet_wrap( ~ Glasgow_Region_Economic_Sector) +
  scale_y_continuous(label = scales::comma) +
  labs(
    x = "SCQF Level",
    y = "Total",
    title = "STUDENT TOTALS BY GRES CLASSIFICATION AND SCQF LEVEL",
    caption = "Note: All courses at SCQF levels 1−3 are included in 'Access and Support Learning'"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "yellow"),
    plot.background = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
    panel.background = element_rect(fill = "#E2E2E3"),
    plot.title = element_text(
      colour = "#4292c6",
      face = "bold",
      size = 15,
      hjust = .5
    )
  ) +
  geom_text(aes(label = scales::comma(totals)), vjust = -.2)
  



#CREDIT TOTALS
p3 <- ggplot(data = glasgow_colleges_scqf, aes(x=reorder(scqf_lev,SCQF_order), y=credits_total)) +
  geom_col(fill = "#4292c6") +
  facet_wrap(~Glasgow_Region_Economic_Sector) +
  scale_y_continuous(label = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "yellow")) +
  geom_text(aes(label = scales::comma(credits_total)), vjust = -.1, size = 4) +
  labs(x = "SCQF Level", y="Credit total", title = "FIGURE 3: Credit totals at Glasgow College by GRES Classification and SCQF level")

##############################################
