

library(tidyverse)
library(cowplot)
library(vegan)


monod <-  read_csv("data-raw/odonnell-gcb-2018/ODonnell_etal_2018_Monod_gr.rates_T.pseudonana_0318.csv")
growth_temp <- read_csv("data-raw/odonnell-gcb-2018/ODonnell_etal_2018_temp_gr.rate_data_T.pseudonana_0318.csv")




growth_temp %>% 
	ggplot(aes(x = assay.temp, y = gr.rate, color  = factor(evol.temp))) +geom_point()

### start with the 16C selection line
growth_16 <- growth_temp %>% 
	filter(evol.temp == 16) %>% 
	group_by(assay.temp, strain) %>% 
	summarise(mean_growth = mean(gr.rate)) %>% 
	select(strain, assay.temp, mean_growth) %>% 
	spread(key = assay.temp, value = mean_growth, 2:3) %>% 
	select(-strain)


tpc_temps <- unique(growth_temp$assay.temp)

pca16 <- rda(growth_16, scale=TRUE)

loadings16 <- scores(pca16,choices=c(1,2))
summary(eigenvals(pca16))


pcs16 <- as_data_frame((loadings16[[1]]))
pc1_16 <- pcs16 %>% 
	mutate(temperature = tpc_temps) 
pc1_16 %>% 
	ggplot(aes(x = temperature, y = PC1)) + geom_point() +
	xlim(0, 40) + 
	geom_smooth() + 
	geom_hline(yintercept = 0) +
	xlab("Temperature (°C)") + geom_line()
ggsave("JB_figures/evol_16_PC1.pdf", width = 6, height = 4)

### now with 31C selection line
growth_31 <- growth_temp %>% 
	filter(evol.temp == 31) %>% 
	group_by(assay.temp, strain) %>% 
	summarise(mean_growth = mean(gr.rate)) %>% 
	select(strain, assay.temp, mean_growth) %>% 
	spread(key = assay.temp, value = mean_growth, 2:3) %>% 
	select(-strain)


tpc_temps <- unique(growth_temp$assay.temp)

pca31 <- rda(growth_31, scale=TRUE)

loadings31 <- scores(pca31,choices=c(1,2))


pcs31 <- as_data_frame((loadings31[[1]]))
pc1_31 <- pcs31 %>% 
	mutate(temperature = tpc_temps) 
pc1_31 %>% 
	ggplot(aes(x = temperature, y = PC1)) + geom_point() +
	xlim(0, 40) + geom_smooth() + geom_hline(yintercept = 0) +
	xlab("Temperature (°C)") + geom_line()


m2 <- monod %>% 
	unite(strain, evol.temp, evol.rep, remove = FALSE)


m2 %>% 
	# filter(strain == "16_1") %>% 
	ggplot(aes(x = conc.N, y = gr.rate, color = factor(assay.temp))) + geom_point() +
	facet_wrap(~strain + assay.temp)


monod16 <- m2 %>% 
	filter(evol.temp == 16, assay.temp == 16) %>%
	group_by(conc.N, strain) %>% 
	summarise(mean_growth = mean(gr.rate)) %>% 
	select(strain, conc.N, mean_growth) %>% 
	spread(key = conc.N, value = mean_growth, 2:3) %>%
	select(-strain)


monod_ns <- unique(m2$conc.N)

pca_monod16 <- rda(monod16, scale=TRUE)

loadings16_monod <- scores(pca_monod16,choices=c(1,2))


pcs16_monod <- as_data_frame((loadings16_monod[[1]]))
pc1_16_monod <- pcs16_monod %>% 
	mutate(n_conc = monod_ns) 
pc1_16_monod %>% 
	ggplot(aes(x = n_conc, y = PC1)) + geom_point() +
	# xlim(0, 40) + 
	# geom_smooth() + 
	geom_hline(yintercept = 0) +
	xlab("Nitrate concentration") + geom_line()
summary(eigenvals(pca_monod16))
