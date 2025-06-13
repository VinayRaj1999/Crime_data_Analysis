library(tidyverse)
library(janitor)

"17_crime_by_place_of_occurrence_2001_2012.csv" %>% 
  read_csv() %>% 
  clean_names()->df1
colnames(df1) %>% 
  length()


### 2. Year range in the dataset
df1 %>% 
  pull(year) %>% 
  range(na.rm = T)

### 3. Columns related to crimes in banks
df1 %>% 
  colnames() %>% 
  str_subset("banks_")

### 4. Columns representing total crime counts
df1 %>% 
  colnames() %>% 
  str_subset("total_")

### 5. Total dacoity cases reported
df1 %>% 
  pull(total_dacoity) %>% 
  length()

### 6. Yearly total theft cases in residential premises
df1 %>% 
  group_by(year) %>% 
  summarise(yearly_res_theft=sum(residential_premises_theft,na.rm=T))

### 7. Average burglary cases in commercial establishments by state
df1 %>% 
  group_by(state_ut) %>% 
  summarise(avr_bur_cases=mean(commercial_establishments_burglary,na.rm=T))


### 8. Year with highest robbery on highways
df1 %>% 
  group_by(year) %>% 
  summarise(highest_rob_Highways=sum(highways_robbery,na.rm = T)) %>% 
  arrange(desc(highest_rob_Highways)) %>% 
  slice(1)


### 9. Year-wise trend of total robbery cases
df1 %>% 
  group_by(year) %>% 
  summarise(total_rob=sum(total_robbery,na.rm = T)) %>% 
  ggplot(aes(x=year,y=total_rob))+
  geom_col(fill="orange")+
  geom_text(aes(label = total_rob),vjust=-0.3,size = 3)+
  scale_x_continuous(breaks = c(2001:2012))+
  theme_minimal()

### 10. Theft across all locations in 2010
df1 %>% 
  filter(year==2010) %>% 
  select(ends_with("theft")) %>% 
  summarise_all(sum,na.rm=T) %>% 
pivot_longer(everything(),names_to = "location",values_to = "cases") %>% 
mutate(location=str_remove(location,"_theft")) %>% 
  ggplot(aes(x = reorder(location, cases), y = cases)) +   
  geom_col(fill = "steelblue") + 
  coord_flip() + 
  labs(title = "Theft Cases by Location - 2010", x = NULL, y = "Cases") + 
  theme_minimal()

### 11. Horizontal bar plot for dacoity cases
df1 %>% 
  group_by(state_ut) %>% 
  filter(!state_ut %in% c("TOTAL (STATES)", "TOTAL (ALL-INDIA)")) %>%
  summarise(total_dacoity=sum(total_dacoity,na.rm = T)) %>% 
  mutate(state_ut=fct_reorder(state_ut,total_dacoity)) %>% 
  ggplot(aes(x=state_ut,y=total_dacoity))+
geom_col(fill="tomato")+
coord_flip()+
  theme_minimal()


### 12. Top 5 states in residential burglary
df1 %>% 
  group_by(state_ut) %>% 
  filter(!state_ut %in% c("TOTAL (STATES)", "TOTAL (ALL-INDIA)")) %>% 
  summarise(top_5_states=sum(residential_premises_burglary,na.rm = T)) %>% 
  arrange(desc(top_5_states)) %>% 
  slice(1:5)


### 13. Combine all theft columns per row
df1 %>% 
  mutate(total_theft_all = rowSums(select(., ends_with("_theft")), na.rm = TRUE)) %>% 
view()         
