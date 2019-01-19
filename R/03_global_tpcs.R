

library(tidyverse)
library(cowplot)
library(vegan)
library(ggbiplot)


temps_growth <- read_csv("data-raw/thomas-tpcs.csv") %>% 
	distinct(isolate.code, temp, growth_rate) 
curve_data <- read_csv("data-raw/thomas3.csv")


tpcs_all <- left_join(temps_growth, curve_data, by = "isolate.code") %>% 
	mutate(abs_lat = abs(latitude))


ggplot() +
	geom_hline(yintercept = 0, color = "grey")+
	geom_line(data = tpcs_all,
			  aes(x = temp, y = growth_rate, group = isolate.code, color = abs_lat)) +
	ylim(0, 2) + ylab("Growth rate (/day)") + xlab("Temperature (°C)") +scale_color_viridis_c()
ggsave("figures/global_tpcs_lat.pdf", width = 8, height = 5)



growth_16 <- temps_growth %>% 
	filter(temp %in% c(1, 5, 10, 15, 20, 25, 30, 35, 40)) %>% 
	distinct(isolate.code, temp, growth_rate) %>% 
	spread(key = temp, value = growth_rate, 2:3) %>% 
	select(-isolate.code)

tpc_temps <- unique(temps_growth$temp)
tpc_temps <- c(1, 5, 10, 15, 20, 25, 30, 35, 40)

cov_mat <- cov(growth_16)
pca_res <- prcomp(cov_mat, center = TRUE,scale. = TRUE)
summary(pca_res)
ggbiplot(pca_res, labels= tpc_temps)
ggsave("figures/PCA-non-acclimated-biplot.pdf", width = 6, height = 6)

pca16 <- rda(growth_16)


loadings16 <- scores(pca_res,choices=c(1,2))
summary(eigenvals(pca_res))

# tpc_temps <- c(1:40)
# tpc_temps <- c(10, 16, 22, 28, 34, 40)
# tpc_temps <- unique(growth_16$temperature)

pcs16 <- as_data_frame((loadings16)) %>% 
	select(PC1)
pc1_16 <- pcs16 %>% 
	mutate(temperature = tpc_temps) 
pc1_16 %>% 
	ggplot(aes(x = temperature, y = PC1)) + geom_point() +
	xlim(0, 40) + 
	# geom_smooth() + 
	geom_hline(yintercept = 0) +
	xlab("Temperature (°C)") + geom_line() +
	ylab("PC1 loadings")

ggsave("figures/PC1-global-phytos.pdf", width = 6, height = 4)


pcs162 <- as_data_frame((loadings16)) %>% 
	select(PC2)
pc2_16 <- pcs162 %>% 
	mutate(temperature = tpc_temps) 
pc2_16 %>% 
	ggplot(aes(x = temperature, y = PC2)) + geom_point() +
	xlim(0, 40) + 
	# geom_smooth() + 
	geom_hline(yintercept = 0) +
	xlab("Temperature (°C)") + geom_line() +
	ylab("PC2 loadings")

ggsave("figures/PC2-global-phytos.pdf", width = 6, height = 4)
