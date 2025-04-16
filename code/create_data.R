#Create the data sets to be used in this analysis.

library(tidyverse)
library(tidycensus)
library(googlesheets4)



# Comparing demos of employees vs county ----------------------------------


#Load the Montco employee data from google sheets.
emp <- read_sheet("https://docs.google.com/spreadsheets/d/1wg1dWlz76Y63J5cQL6C43gsbGrNQXsjl7TMa1ht-P1c/edit") %>% 
  #Take only the first two columns and rename them
  select(value = 1,
         count = 2)

#This data is far from tidy so we need to do some initial cleanup
emp <- emp %>% 
  #Age
  filter(row_number() >= 3,
         row_number() <= 19) %>% 
  mutate(metric = 'Age',
         count = replace_na(count, 0)) %>% 
  #Gender
  bind_rows({
    emp %>% 
      filter(row_number() %in% c(1, 26, 51)) %>% 
      mutate(value = str_remove_all(value, ' population')) %>% 
      pivot_wider(names_from = value,
                  values_from = count) %>% 
      mutate(Unspecified = Total - Male - Female) %>% 
      select(-Total) %>% 
      pivot_longer(everything(),
                   names_to = 'value',
                   values_to = 'count') %>% 
      mutate(metric = 'Gender')
  }) %>% 
  #Race
  bind_rows({
    emp %>% 
      filter(row_number() >= 83,
             row_number() <= 90) %>% 
      mutate(metric = 'Race')
  })



#Pulling data from the ACS requires an API key, which can be requested here:
#http://api.census.gov/data/key_signup.html
#census_api_key("XXX", install = TRUE)

#What are the variable names in the ACS for race, sex and age?
acs_variables <- load_variables(year = 2023, dataset = 'acs1') %>% 
  filter(concept %in% c("Race",
                        "Sex by Age"))

#Request age, gender and race data for Montco from the ACS
montco <- get_acs(
  geography = "county",
  variables = acs_variables$name,
  year = 2023,
  state = 'PA',
  county = 'Montgomery'
) %>% 
  left_join(acs_variables, by = c('variable' = 'name'))


#Tidy the ACS data
montco <- montco %>% 
  #Gender
  filter(label %in% c('Estimate!!Total:!!Male:',
                      'Estimate!!Total:!!Female:')) %>% 
  transmute(
    metric = 'Gender',
    value = str_extract(label, '(?<=!!Total:!!)([^:]+)'),
    count = estimate
  ) %>%
  #Age
  bind_rows({
    montco %>% 
      filter(concept == 'Sex by Age',
             str_count(label, '!!') == 3) %>% 
      transmute(
        metric = 'Age',
        value = str_extract(label, '[^!]*$'),
        count = estimate
      ) %>% 
      #Summarise by age group
      group_by(metric, value) %>% 
      summarise(count = sum(count))
  }) %>% 
  #Race
  bind_rows({
    montco %>% 
      filter(concept == 'Race',
             !is.na(moe)) %>% 
      transmute(
        metric = 'Race',
        value = str_extract(label, '[^!]*$'),
        count = estimate
      )
    
  })


#Create Age groupings to match the employee data
montco <- montco %>% 
  bind_rows({
    montco %>% 
      filter(metric == 'Age',
             value %in% c('15 to 17 years',
                          '18 and 19 years',
                          '20 years',
                          '21 years',
                          '22 to 24 years',
                          '60 and 61 years',
                          '62 to 64 years',
                          '65 and 66 years',
                          '67 to 69 years')) %>% 
      group_by(metric,
               value = c(rep('15 to 19 years', 2),
                         rep('20 to 24 years', 3),
                         rep('60 to 64 years', 2),
                         rep('65 to 69 years', 2))) %>% 
      summarise(count = sum(count))
  })


#Relabel the races
montco <- montco %>% 
  mutate(value = if_else(metric == 'Race',
                         str_remove_all(value, " alone|:"),
                         value))



#Now we can finally join the employee and population data together
df <- emp %>% 
  rename(emp = count) %>% 
  inner_join(montco, by = c('metric', 'value')) %>% 
  rename(pop = count)

saveRDS(df, 'data/combined_demos.rds')




# County demos over time --------------------------------------------------

#We'll assume that gender/age distributions within the county have remained 
#relatively steady over time.  Let's focus on any changes in the racial makeup
#of the county over the last few decades.

#Ping the census api for 1-year ACS data as far back as we can
race_trend <- tibble()

#Need to exclude 2020
for(year_i in seq(2005, 2023, 1)[-16]){
  race_trend <- race_trend %>% 
    bind_rows({
      get_acs(
        geography = "county",
        variables = paste0("B02001_00", 2:8),
        year = year_i,
        state = 'PA',
        county = 'Montgomery',
        survey = 'acs1'
      ) %>% 
        mutate(year = year_i)
    })
}

race_trend <- race_trend %>% 
  left_join(acs_variables, by = c('variable' = 'name')) %>% 
  mutate(race = str_extract(label, '[^!]*$'), 
         race = str_remove_all(race, ' alone|:')) %>% 
  #Get a proportion for every year
  group_by(year) %>% 
  mutate(p = estimate / sum(estimate)) %>% 
  ungroup()


saveRDS(race_trend, 'data/race_trend.rds')

