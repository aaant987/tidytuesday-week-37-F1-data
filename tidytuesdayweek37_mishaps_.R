
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



# pilotos --------------------------

# carreras disputadas por piloto

data1 <- races %>% 
  left_join(results, by = "raceId") %>% 
  left_join(drivers, by = "driverId") %>% 
  left_join(status, by = "statusId") %>% 
  group_by(raceId, driverRef) %>% 
  count(raceId) %>%   
  distinct(raceId,.keep_all = T) %>%
  spread(key = "driverRef", value = n) %>%
  replace(is.na(.), 0) %>%
  select_if(is.numeric) %>%
  map_dbl(sum) %>%
  as_tibble_row() %>%
  gather() %>% 
  rename(driverRef = key,
         n_races = value) %>% 
  filter(driverRef != c("raceId")) %>% 
  arrange(-n_races) 


# incidentes del piloto


data2 <- results %>% 
  left_join(drivers, by = "driverId") %>% 
  left_join(status, by = "statusId") %>% 
  group_by(driverRef, status) %>% 
  count(status) %>% 
  rename(n_incident = n) %>% 
  arrange(-n_incident) 


data <- data1 %>% 
  left_join(data2, by = "driverRef") %>% 
  mutate(incident_ratio = (n_incident/n_races)*100) %>% 
  #filter(driverRef %in% c("alonso", "hamilton", "raikkonen", "michael_schumacher",
                          #"massa", "button", "barrichello"))
  filter(n_races > 130, status %in% c("Accident",
                                      "Brakes",
                                      "Collision",
                                      "Disqualifilied",
                                      "Electrical",
                                      "Engine",
                                      "Gearbox",
                                      "Hydraulics",
                                      "Spun off",
                                      "Suspension",
                                      "Throttle",
                                      "Transmission",
                                      "Turbo",
                                      "Tyre",
                                      "Wheel",
                                      "Withdrew"))
                                      



library("plyr")
library("reshape2")
library("scales")
data.s <- ddply(data, .(status), transform,
               rescale = scale(incident_ratio))


# cuántas veces le ha occurrido un determinado incidente durante una carrera a los mejores pilotos de la historia de la F1


my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "#D0D3D4"),
                  panel.background = element_rect(fill = "#D0D3D4"),
                  panel.grid = element_blank(),
                  plot.title = element_text(family = "Bahnschrift", size = 16, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  axis.text.x = element_text(hjust=0.5, size = 12, color = "#B03A2E"),
                  axis.text.y = element_text(hjust=0.5, size = 12, color = "#B03A2E")) 



data.s %>% 
  ggplot(aes(reorder(driverRef, n_races), status)) +
  geom_tile(aes(fill = rescale)) + 
  scale_fill_gradient(low = "green", high = "red") +
  geom_text(aes(label = round(incident_ratio,1)), size = 3.6, family = "Bahnschrift", color = "black") +
  labs(
    title = "Drivers with more mishaps",
    subtitle = "Mishap = (number of mishap / number of races in his career)*100\nDrivers with >130 races in F1",
    caption = "#Tidytuesday week 37|Data: Ergats API|@dataR_amateur",
    x = "", 
    y = "") +
  my_theme + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_flip()

ggsave("tidytuesdayweek37_mishaps.png", width = 15.5, height = 9, device = agg_png, dpi = 500)





