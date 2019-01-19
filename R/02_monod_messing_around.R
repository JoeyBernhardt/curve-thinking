

library(tidyverse)
library(cowplot)
library(vegan)
library(janitor)


monod <-  read_csv("data-raw/odonnell-gcb-2018/ODonnell_etal_2018_Monod_gr.rates_T.pseudonana_0318.csv")
growth_temp <- read_csv("data-raw/odonnell-gcb-2018/ODonnell_etal_2018_temp_gr.rate_data_T.pseudonana_0318.csv")




growth_temp %>% 
	ggplot(aes(x = assay.temp, y = gr.rate, color  = factor(evol.temp))) +geom_point()

### start with the 16C selection line
growth_16 <- growth_temp %>% 
	filter(evol.temp == 16) %>% 
	mutate(assay.temp = round(assay.temp, digits = 2)) %>% 
	group_by(assay.temp, strain) %>% 
	summarise(mean_growth = mean(gr.rate)) %>% 
	select(strain, assay.temp, mean_growth) %>% 
	spread(key = assay.temp, value = mean_growth, 2:3) %>% 
	select(-strain)

corr_mat <- cor(growth_16,method="s")
library(corrplot)
corrplot(corr_mat)

library(reshape2)
cormat <- corr_mat
melted_cormat <- melt(corr_mat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
	geom_tile(size = 4)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
	geom_tile(color = "white", size = 0.005)+
	scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
						 midpoint = 0, limit = c(-1,1), space = "Lab", 
						 name="Pearson\nCorrelation") +
	coord_fixed() + ylab("Temperature (°C)") + xlab("Temperature (°C)")
ggsave("figures/correlation_plot.pdf", width = 6, height = 4)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
	cormat[upper.tri(cormat)] <- NA
	return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
	cormat[lower.tri(cormat)]<- NA
	return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
	geom_tile(color = "white")+
	scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
						 midpoint = 0, limit = c(-1,1), space = "Lab", 
						 name="Pearson\nCorrelation") +
	theme_minimal()+ 
	theme(axis.text.x = element_text(angle = 45, vjust = 1, 
									 size = 12, hjust = 1))+
	coord_fixed()

ggheatmap + 
	geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
	theme(
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		panel.grid.major = element_blank(),
		panel.border = element_blank(),
		panel.background = element_blank(),
		axis.ticks = element_blank(),
		legend.justification = c(1, 0),
		legend.position = c(0.6, 0.7),
		legend.direction = "horizontal")+
	guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
								 title.position = "top", title.hjust = 0.5))

ggsave("figures/correlation_plot.pdf", width = 6, height = 4)

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


m2 <- monod %>% 
	clean_names() %>% 
	unite(col = strain, evol_temp, evol_rep) %>% 
	mutate(experiment = "monod")
growth2 <- growth_temp %>% 
	clean_names() %>% 
	mutate(conc_n = 882) %>% 
	mutate(experiment = "tpc")

all_data <- bind_rows(m2, growth2) %>% 
	select(strain, assay_temp, conc_n, gr_rate, experiment, evol_temp)

all_data %>% 
	filter(gr_rate >0) %>% 
	# filter(experiment == "tpc") %>% 
	ggplot(aes(x = assay_temp, y = conc_n, color = gr_rate)) + geom_point(size = 3) +
	scale_color_viridis_c() 


all_data %>% 
	filter(gr_rate >0) %>% 
	filter(experiment == "tpc") %>% 
	ggplot(aes(x = assay_temp, y = gr_rate, color = factor(evol_temp))) + geom_point()
