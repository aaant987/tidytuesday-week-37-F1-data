
library(tidyverse)
library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))
library(ragg)

circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')


df1 <- races %>% 
  group_by(year, round) %>%
  count(round) %>%
  distinct(round,.keep_all = T) %>%
  spread(key = "year", value = n) %>%
  replace(is.na(.), 0) %>%
  select_if(is.numeric) %>%
  map_dbl(sum) %>%
  as_tibble_row() %>%
  gather() %>% 
  rename(year = key,
         n_races = value) %>% 
  filter(year != "round") %>% 
  mutate(year = as.numeric(year))


df2 <- results %>% 
  left_join(races, by = "raceId") %>% 
  left_join(status, by = "statusId") %>% 
  group_by(year, status) %>% 
  count(status) %>% 
  rename(n_incident = n) 

df <- df1 %>% 
  left_join(df2, by = "year")  %>% 
  mutate(incident_by_races_year = n_incident/n_races)

df %>% 
  count(status) %>% 
  arrange(-n) %>% 
  print(n = 135)

df %>% 
  filter(status %in% c("+1 Lap",
                       "Accident",
                       "Engine",
                       "Turbo")) %>% 
  ggplot(aes(year, status)) +
  geom_tile(aes(fill = incident_by_races_year)) + 
  geom_text(aes(label = round(incident_by_races_year,1)), size = 2)

df %>% 
  count(status) %>% 
  arrange(-n) %>% 
  print(n = 135)

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "#737BA2"),
                  panel.background = element_rect(fill = "#737BA2"),
                  panel.grid = element_blank(),
                  plot.title = element_text(family = "Bahnschrift", size = 16, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 



df %>% 
  filter(status %in% c("+1 Lap",
                       "+2 Lap",
                       "Engine",
                       "Gearbox",
                       "Suspension",
                       "Accident",
                       "Brakes",
                       "Clutch",
                       "Electrical",
                       "Spun off",
                       "Transmission",
                       "Disqualified",
                       "Collision",
                       "Oil leak",
                       "Withdrew",
                       "Wheel",
                       "Overheating",
                       "Throttle",
                       "Fuel system",
                       "Ignition",
                       "Halfshaft",
                       "Oil pressure",
                       "Fuel pump",
                       "Differential",
                       "Steering",
                       "Fuel leak",
                       "Radiator",
                       "Tyre",
                       "Handling",
                       "Hydraulics",
                       "Wheel bearing",
                       "Physical",
                       "Puncture",
                       "Water leak",
                       "Chassis",
                       "Exhaust",
                       "Injection",
                       "Turbo",
                       "Alternator",
                       "Fuel pressure",
                       "Heat Shield fire",
                       "Mechanical",
                       "Axle",
                       "Electronics")) %>% 
  ggplot(aes(year, incident_by_races_year)) +
  geom_col(width = 2, fill = "blue") +
  #stat_smooth(se = F) +
  facet_wrap(~status, scales ="free_y") +
  labs(
    title = "Evolution type of incident per season",
    subtitle = "*Incident = number of incidents / number of races per season",
    caption = "#Tidytuesday week 37|Data: Ergats API|@dataR_amateur",
    x = "", 
    y = "Incident per season") +
  my_theme


ggsave("tidytuesdayweek37.png", width = 15.5, height = 9, device = agg_png, dpi = 500)