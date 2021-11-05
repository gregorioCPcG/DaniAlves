library(tidyverse)
library(knitr)
library(kableExtra)


jogos <- c(37, 250, 391,33, 73,95, 119)
gols <- c(5,16,21,6,8,10,8)
time <- c("Bahia","Sevilla", "Barcelona", "Juventus", "PSG", "SãoPaulo", "Brasil")
options(scipen = 1000)
media_gols <- gols/jogos
data <-data.frame (jogos, gols, media_gols, time)
data
b5 <- data %>% 
  dplyr::select(time, jogos, gols, media_gols) %>% 
  arrange(desc(media_gols))
b5 %>%
  kbl(caption = "Carreira de Daniel Alves") %>%
  kable_classic(full_width = F, html_font = "Garamond")


# acrescentar as assistências
assistencias <- c(3, 29, 101, 7, 18, 14, 21)
media_assistencias <- assistencias/jogos

data <-data.frame (jogos, gols, media_gols, time, assistencias, media_assistencias)

b6 <- data %>% 
  dplyr::select(time, jogos, gols, media_assistencias) %>% 
  arrange(desc(media_assistencias))
b6 %>%
  kbl(caption = "Carreira de Daniel Alves") %>%
  kable_classic(full_width = F, html_font = "Garamond")


cor(media_assistencias, media_gols)


Particip <- gols + assistencias 
media_particip <- Particip/jogos

data <-data.frame (jogos, gols, media_gols, time, assistencias, media_assistencias, Particip, media_particip)

b7 <- data %>% 
  dplyr::select(time, jogos, gols, media_particip) %>% 
  arrange(desc(media_particip))
b7 %>%
  kbl(caption = "Carreira de Daniel Alves") %>%
  kable_classic(full_width = F, html_font = "Garamond")

plot(jogos, gols)
plot(jogos, assistencias)
plot(gols, assistencias)
plot(jogos, media_particip)


f <- ggplot(data, aes(jogos, media_particip))
f + geom_text(aes(label = time)) +  ylab("Média Gols + Assistências") + scale_x_continuous(limits = c(0, 450)) + scale_y_continuous(limits = c(0.13, 0.45))

j <- ggplot(data, aes(media_gols, media_assistencias))
j + geom_text(aes(label = time)) + scale_x_continuous(limits = c(0, 0.32)) + scale_y_continuous(limits = c(0, 0.32)) +
  ylab("MédiaAssistências") + xlab("Média Gols")


boxplot(media_assistencias)
hist(media_assistencias)
a <- lm(media_particip ~ gols + assistencias + jogos)
summary(a)
