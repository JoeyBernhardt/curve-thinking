

library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)

edwards_raw <- read_xlsx("data-raw/Edwards-2016-tableA3.xlsx", skip = 2) %>% 
	clean_names()


edwards_raw %>% 
	ggplot(aes(x = temperature, y = growth_rate)) + geom_point()



edwards_raw %>% 
	# filter(species == "Thalassiosira allenii") %>% 
	ggplot(aes(x = temperature, y = growth_rate, color = irradiance, group = irradiance)) + geom_point() + geom_line() +
	facet_wrap(~ species, scales = "free") + ylab("Growth rate (per day)") + xlab("Temperature (°C)")  + scale_color_viridis_c()
ggsave("figures/edwards-temp-irradiance.pdf", width = 16, height = 14)

more_than_1 <- edwards_raw %>% 
	group_by(species, temperature, irradiance) %>% 
	distinct() %>% 
	tally() %>% 
	filter(n> 1)

write_csv(more_than_1, "data-processed/edwards-multi-entries-per-species.csv")


edwards_raw %>% 
	filter(species == "Fragilaria bidens") %>% 
	ggplot(aes(x = temperature, y = growth_rate, color = irradiance, group = irradiance)) + geom_point() +
	facet_wrap( ~ irradiance, scales = "free") + geom_smooth(aes(color = irradiance)) +
	ylab("Growth rate (per day)") + xlab("Temperature (°C)")  + scale_color_viridis_c()
ggsave("figures/fragilaria_tpc.pdf", width = 10, height = 8)

edwards_raw %>% 
	filter(species == "Cryptomonas sp. 1") %>% 
	ggplot(aes(x = temperature, y = growth_rate, color = irradiance, group = irradiance)) + geom_point() +
	facet_wrap( ~ irradiance, scales = "free") + geom_smooth(aes(color = irradiance)) +
	ylab("Growth rate (per day)") + xlab("Temperature (°C)")  + scale_color_viridis_c()
ggsave("figures/cryptomonas_tpc.pdf", width = 10, height = 8)


fragilaria <- edwards_raw %>% 
	filter(species == "Fragilaria bidens") %>% 
	group_by(temperature, irradiance) %>% 
	summarise(mean_growth = mean(growth_rate)) %>% 
	ungroup()

ggplot(data = fragilaria, aes(x=temperature, y=irradiance, fill=mean_growth)) + 
	geom_tile() + scale_fill_viridis_c()



edwards_raw %>% 
	# filter(species == "Fragilaria bidens") %>% 
	ggplot(aes(x=temperature, y=irradiance, color=growth_rate)) + 
	geom_point(size = 4, alpha = 0.7) + scale_color_viridis_c() +
	facet_wrap( ~ species, scales = "free") +ylab("Irradiance") + xlab("Temperature (°C)")
ggsave("figures/edwards-temp-irradiance-heatmap.pdf", width = 16, height = 14)
