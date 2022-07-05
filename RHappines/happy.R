#### Carregar os pacotes
packages = c('sf', 'tmap', 'tidyverse','plotly','ggthemes','heatmaply','RColorBrewer')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}


# Ler o conjunto de dados
happiness_2020 <- read.csv("2020.csv")
str(happiness_2020)

# Lendo o mapa
#unique(happiness_2020$Country.name)
maplocation <-
  read_sf("Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
str(maplocation)

# Fazendo a conjunção do mapa com o conjunto de dados
#unique(maplocation$CNTRY_NAME)
combined_dataset <- left_join(maplocation,happiness_2020 , 
                              by = c("CNTRY_NAME" = "Country.name"))
names(combined_dataset)

# Remover todos os NA
combined_dataset <- combined_dataset[!is.na(combined_dataset$Ladder.score),] %>%
  select(CNTRY_NAME,Regional.indicator,Ladder.score,Logged.GDP.per.capita,Social.support,
         Healthy.life.expectancy,Freedom.to.make.life.choices,
         Generosity,Perceptions.of.corruption)

# Mapa de correlação dos atributos

combined_dataset_corr <- as.data.frame(combined_dataset)
heatmaply_cor(
  cor(combined_dataset_corr[, 3:9]),
  xlab = "Features", 
  ylab = "Features",
  colors = colorRampPalette(brewer.pal(3, "Spectral"))(256),
  k_col = 2, 
  k_row = 2
)

# Gráfico de dispersão score vs. apoio social

p <- ggplot(combined_dataset, 
            aes(x = Ladder.score, y=Social.support, 
                colour =Regional.indicator ,text = paste("country:", CNTRY_NAME))) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_colour_brewer(type = "seq", palette = "Spectral") + 
  scale_size(range = c(2, 12)) +
  scale_x_log10()+
  theme_minimal()+
  labs(x = "Ladder score", y = "Social support",
       caption = "Data source: ToothGrowth")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
fig <- ggplotly(p)
fig

# PIB vs. Expectativa de vida saudável
p <- ggplot(combined_dataset, 
            aes(x = Logged.GDP.per.capita, y=Healthy.life.expectancy, 
                colour =Regional.indicator ,text = paste("country:", CNTRY_NAME))) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_colour_brewer(type = "seq", palette = "Spectral") + 
  scale_size(range = c(2, 12)) +
  scale_x_log10()+
  theme_minimal()+
  labs(x = "Logged GDP per capita", y = "Life expectancy",
       caption = "Data source: ToothGrowth")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

fig <- ggplotly(p)
fig

# 10 países mais felizes
combined_dataset <- combined_dataset %>% arrange(desc(Ladder.score)) 

top20<- head(combined_dataset,10) 

p2 <- ggplot(top20, aes(x= reorder(CNTRY_NAME,-Ladder.score), 
                        y=Ladder.score, fill=Regional.indicator))+
  geom_point( color="#C4961A", size=4, shape=18) +
  geom_segment( aes(x=reorder(CNTRY_NAME,-Ladder.score), 
                    xend=reorder(CNTRY_NAME,-Ladder.score), 
                    y=0, yend=Ladder.score), color="grey") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Dark2")+
  xlab("") +
  ylab("Ladder score")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  ggtitle("Top 10 Countries with high Ladder Scores" )

ggplotly(p2)

# 10 países menos felizes

bottom20<- tail(combined_dataset,10) 

p3 <- ggplot(bottom20, aes(x= reorder(CNTRY_NAME,-Ladder.score),
                           y=Ladder.score, fill=Regional.indicator))+
  geom_point( color="#00AFBB", size=4, shape=18) +
  geom_segment( aes(x=reorder(CNTRY_NAME,-Ladder.score),
                    xend=reorder(CNTRY_NAME,-Ladder.score), 
                    y=0, yend=Ladder.score), color="grey") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2")+
  xlab("Country") +
  ylab("Ladder score")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
  ggtitle("Bottom 10 Countries with low Ladder Scores" )
ggplotly(p3)

# Box plot regiões do globo (score)

p2 <- ggplot(combined_dataset, aes(x=Regional.indicator, y = Ladder.score))+
  geom_boxplot()+
  theme_minimal()+
  geom_violin(aes(fill=Regional.indicator))+
  scale_color_brewer(palette = "Dark2") +
  stat_summary(geom = 'point', fun = 'mean', color='red')+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

ggplotly(p2)

# Box plot regiões do globo (vida saudável)

p2 <- ggplot(combined_dataset, aes(x=Regional.indicator, y = Ladder.score))+
  geom_boxplot()+
  theme_minimal()+
  geom_violin(aes(fill=Regional.indicator))+
  scale_color_brewer(palette = "Dark2") +
  stat_summary(geom = 'point', fun = 'mean', color='red')+ 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

ggplotly(p2)

# Visualizações em mapas

tmap_mode("view")

## tmap mode set to interactive viewing

tm_shape(combined_dataset[combined_dataset$Regional.indicator=="Latin America", ]) +
  tm_fill("Ladder.score", 
          style = "quantile", 
          palette = "Greens") +
  tm_borders(alpha = 0.5) +
  tm_text("CNTRY_NAME", size="CNTRY_NAME")+
  tmap_style("watercolor")

## tmap style set to "watercolor"

## other available styles are: "white", "gray", "natural", "cobalt", "col_blind", "albatross", "beaver", "bw", "classic"

## Text size will be constant in view mode. Set tm_view(text.size.variable = TRUE) to enable variable text sizes.