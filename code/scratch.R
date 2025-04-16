#Do some final wrangling of the data strictly for plotting purposes
plot_df <- df %>% 
  mutate(age_num = as.numeric(str_sub(value, 1, 2)),
         value = str_wrap(value, 15)) %>%
  #Limit ages from 20 to 65
  filter((age_num >= 20 & age_num <= 60) | is.na(age_num)) %>% 
  #Pivot to calculate proportions from each source
  rename(`County Employees` = emp,
         `Total Population` = pop) %>% 
  pivot_longer(cols = c(`County Employees`, `Total Population`),
               names_to = 'Group',
               values_to = 'count') %>% 
  group_by(metric, Group) %>% 
  mutate(p = count / sum(count), 
         p_label = paste0(round(p * 100), "%")) %>% 
  #Create the index, the ratio of proportions for employees vs total.
  #Limit to groups of at least 5& of the total population
  group_by(metric, value) %>% 
  mutate(index = ifelse(lead(p) > 0.05, round(p / lead(p), 2), NA)) %>% 
  ungroup()


#Build the plot
demo_compare_plot <- plot_df %>% 
  ggplot(aes(value, p, fill = Group)) + 
  geom_col(position = position_dodge()) + 
  #Labels for each proportion.... too busy
  # geom_text(aes(y = 0, label = p_label, group = Group), 
  #           position = position_dodge(width = 0.9),
  #           vjust = -1.5)+
  #Labels with the index
  geom_text(aes(label = index), fontface = 'bold') + 
  facet_wrap(vars(metric), 
             ncol = 1,
             scales = 'free') + 
  theme_ju() + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_brewer(palette = 'Set1') + 
  labs(title = 'Demographic Comparison: Employees vs Population',
       subtitle = 'Labels show the ratio between employee vs population proportions',
       x = NULL,
       y = '%of Total')


#Caption the plot
demo_compare_plot <- caption_plot(demo_compare_plot,
             "Source: ACS 5 year survey 2023",
             "Jordan Upton")


