library(readr)
anime <- read_csv("https://raw.githubusercontent.com/HaochenPan857/678-final/main/Anime_data1.csv")
data1 <- anime[c("Anime_id","Title", "Type", "Producer", "Studio", "Rating", "ScoredBy", "Popularity","Members","Episodes","Source","Aired")]
data = na.omit(data1)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)
library(tidyr)
library(plotly)
library(corrplot)
library(tidyverse)
library(sf)
library(ggpubr)
library(knitr)
library(coefplot)
library(plotrix)
library(cluster)
library(factoextra)
library(tidytext)
library(tm)
library(stm)
library(reshape2)
library(wordcloud)
library(RColorBrewer)
library(MASS)
library(lme4)
library(lattice)


sample_1 = sample_n(data, 100)
ggplot(data = sample_1, mapping = aes(x = Rating, y = Popularity)) +
  geom_point(alpha = 0.5, aes(size = Members , color=Source)) +
  scale_x_discrete(breaks = seq(0, 100, by = 5)) +
  ggtitle("figure2:Relationship between rating and Members/Source") +
  xlab("The rating of animes") + ylab("Popularity") # The less of the Popularity means the anime is more famous


data2 <- data %>%
  mutate(Source = str_replace(Source,"[[0-9]]+(?!-)","Undicided"))
data2 %>% 
  group_by(Source) %>% 
  summarise(mean = mean(Rating, na.rm = T), 
            SD = mean(Rating, na.rm = T), 
            miss = mean(is.na(Rating))) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  print(n = 50)
data2 %>%
  ggplot(aes(Rating)) + 
  geom_density() +
  facet_wrap(~Source)
data2 %>%
  ggplot(aes(Rating)) + 
  geom_density() +
  facet_wrap(~Type)

mu2 <- lmer(Rating ~ 1 + (1 |Type),REML= FALSE,
            data = data2)
summary(mu2)
data2$mu2 <- predict(mu2)
data2 %>% 
  ggplot(aes(Members, mu2, color = Type, group = Type)) + 
  geom_smooth(se = F, method = lm) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  ggtitle("figure4:Prediction for Rating based on Type and Members")+
  labs(x = "", y = "change for Rating", color = "Type")
qqmath(ranef(mu2, condVar = TRUE))


mu3 <- lmer(Rating ~ 1 + Members + (1 |Type),REML= FALSE,
            data = data2)
summary(mu3)
data2$mu3 <- predict(mu3)
data2 %>%
  ggplot(aes(Members, mu3, color = Type, group = Type)) + 
  geom_smooth(se = F, method = lm) +
  theme_bw() +
  ggtitle("figure6:Prediction in MLM model")+
  labs(x = "Members", y = "change for Rating", color = "Type")

mu4 <- lmer(Rating ~ 1 + Members + (1 + Members |Type),REML= FALSE,
            data = data2)
coefs_mu4 <- coef(mu4)
coefs_mu4$Type %>%
  mutate(Type = rownames(coefs_mu4$Type))  %>% 
  ggplot(aes(Members, `(Intercept)`, label = Type)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.15, alpha = 0.5) +
  theme_bw() +
  ggtitle("figure10 :effect of members and rating")+
  labs(x = "Slope", y = "Intercept")

data3 <- data2[c("Anime_id","Rating","Members","Popularity")]
data3 <- sample_n(data3,40)
data3 <- na.omit(data3)
data3 <- data3[!is.na(as.numeric(data3$Rating)),]
data3 <- data3[!is.na(as.numeric(data3$Popularity)),]
data3 <- data3[!is.na(as.numeric(data3$Members)),]
data3 <- data3[!is.na(as.numeric(data3$Anime_id)),]
data3 <- scale(data3)
k <- kmeans(data3, centers = 2, nstart = 40)
fviz_cluster(k, data = data3)

word_data <- anime[c("Genre")]
word_data <- na.omit(word_data)
word_data_df <- tibble(word_data)
word_data_df %>% 
  mutate(Genre_number = row_number()) -> word_data_df
word_data_df <- word_data_df %>%
  unnest_tokens(word, Genre, to_lower = FALSE)%>% anti_join(stop_words)
word_data_df %>% 
  count(word, sort = TRUE)
world_cloud <- wordcloud(words = word_data$Genre,max.words = 150, min.freq = 1, random.order = FALSE,rot.per = 0.35, colors=brewer.pal(8, "Dark2"))