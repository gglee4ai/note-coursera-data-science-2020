# common
library(tidyverse)
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")


# 1
total_emissions <-
  nei %>% 
  group_by(year) %>% 
  summarize(total = sum(Emissions))

png("plot1.png")
with(total_emissions, 
     barplot(total / 1e3, names.arg = year, 
             ylab = "Total emissions, kilo-tons", 
             main = "PM2.5 from US", 
             ylim = c(0, 8000)))
dev.off()


# 2
baltimore <- 
  nei %>% 
  filter(fips == "24510")

baltimore_total <-
  baltimore %>% 
  group_by(year) %>% 
  summarize(total = sum(Emissions))

png("plot2.png")
with(baltimore_total, 
     barplot(total / 1e3, names.arg = year, 
             ylab = "Total emissions, kilo-tons", 
             main = "Total PM2.5 from Baltimore city", 
             ylim = c(0, 4)))
dev.off()


# 3
nei %>% 
  filter(fips == "24510") %>% 
  group_by(type, year) %>% 
  summarize(sum = sum(Emissions)) %>% 
  ggplot(aes(year, sum, color = type)) +
  geom_point() +
  geom_line() +
  labs(x = "Year",
    y = "Total emissions, tons",
    title = "PM2.5 from Baltimore City"
  ) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
ggsave("plot3.png")


# 4
scc_coal_combustion <-
  scc %>% 
  filter(
    str_detect(SCC.Level.One, regex('combustion', ignore_case = T)),
    str_detect(SCC.Level.Three, regex('coal', ignore_case = T))
  ) %>% 
  pull(SCC)

nei %>% 
  filter(SCC %in% scc_coal_combustion) %>% 
  group_by(year) %>% 
  summarize(total = sum(Emissions)) %>% 
  ggplot(aes(year, total / 1e3)) +
  geom_point() +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_line() +
  ylim(0, 600) +
  labs(x = "Year", y = "Total emission, kilo-tons", 
       title = "PM2.5 from US",
       subtitle = "Coal combustion-related sources") +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
ggsave("plot4.png")



# 5
scc_motor_vehicle <-
  scc %>% 
  filter(
    EI.Sector %in% c(
      "Mobile - On-Road Diesel Heavy Duty Vehicles", 
      "Mobile - On-Road Diesel Light Duty Vehicles", 
      "Mobile - On-Road Gasoline Heavy Duty Vehicles", 
      "Mobile - On-Road Gasoline Light Duty Vehicles"
    )
  ) %>% 
  pull(SCC)

nei %>% 
  filter(fips == "24510") %>% 
  filter(SCC %in% scc_motor_vehicle) %>% 
  group_by(year) %>% 
  summarize(total = sum(Emissions)) %>% 
  ggplot(aes(year, total)) +
  geom_point() +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_line() +
  ylim(0, 400) +
  labs(
    x = "Year", y = "Total emission, tons", 
    title = "PM2.5 from Baltimore city",
    subtitle = "Motor vehicle sources"
  ) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
ggsave("plot5.png")


# 6
scc_motor_vehicle <-
  scc %>% 
  filter(
    EI.Sector %in% c(
      "Mobile - On-Road Diesel Heavy Duty Vehicles", 
      "Mobile - On-Road Diesel Light Duty Vehicles", 
      "Mobile - On-Road Gasoline Heavy Duty Vehicles", 
      "Mobile - On-Road Gasoline Light Duty Vehicles"
    )
  ) %>% 
  pull(SCC)

nei %>% 
  filter(
    fips %in% c("24510", "06037"),
    SCC %in% motor_vehicle_scc$SCC
  ) %>% 
  mutate(city = ifelse(fips == "24510", "Baltimore", "LA")) %>% 
  group_by(city, year) %>% 
  summarize(total = sum(Emissions)) %>% 
  ggplot(aes(year, total, color = city)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year", y = "Total emission, tons", 
    title = "PM2.5 from Baltimore and LA",
    subtitle = "Motor vehicle sources"
  ) +
  ylim(0, 5000) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))
ggsave("plot6.png")




