#Laste ned pakker
library(tidyverse)
library(rvest)

#Leser nettsiden og trekker ut tabellen:

webpage <- read_html("https://www.ssb.no/a/histstat/aarbok/ht-0901-bnp.html")
browseURL("https://www.ssb.no/a/histstat/aarbok/ht-0901-bnp.html")


tabell <- html_table(html_nodes(webpage, "table")[[2]])
tabell


#sjekke ut tabellen:
head(tabell)
tail(tabell)
str(tabell)
names(tabell)

#skal bearbeide tabellen, fjerner først manglende observasjoner:
tabell <- tabell %>% drop_na()

tabell

names(tabell) <- c("År", "BNP", "BNP_endring", "BNP_percap", 
                   "BNP_percap_endring")
tabell <- as_tibble(tabell)

tabell

#erstatter mellomromskillene:
tabell <- 
  tabell %>% 
  mutate(BNP=str_replace(BNP, " ", ""), 
         BNP_endring=na_if(BNP_endring, ""),
         BNP_percap_endring=na_if(BNP_percap_endring, ""), 
         BNP_endring=str_replace(BNP_endring, ",", "."),
         BNP_percap_endring=str_replace(BNP_percap_endring, ",", ".")) %>% 
  mutate_if(is.character, as.numeric)
tabell

#Oppgave 1:

tabell %>% 
  ggplot(aes(x=År, y=BNP_percap)) + geom_line(colour="dark blue") + 
  scale_y_continuous(labels = scales::comma) + 
  labs(title = "Bruttonasjonalprodukt - BNP \n (kr per innbygger)",
       x = " ", 
       y = "kr per innbygger") +
  theme_bw() 

#Oppgave 2:

#ser på befolkningsvekst: 
tabell <- tabell %>% 
  mutate(tabell, Befolkning = BNP, BNP_percap)

tabell

tabell <- mutate(tabell, BNP / BNP_percap )

utvikling <- tabell %>%
  select(År, BNP, BNP_endring, BNP_percap, BNP_percap_endring) %>%
  mutate(BNP_percap_utvikling = BNP_percap / mean(BNP_percap_endring, na.rm = TRUE)) 

view(utvikling)

tabell %>% 
  filter(År>=1866) %>% 
  mutate(tiår = År - År%%10) %>% 
  group_by(tiår) %>% 
  mutate(snittBNP=mean(BNP_percap_endring)) %>% 
  ggplot(aes(x=År)) + 
  geom_line(aes(y=BNP_percap_endring), color ="dark green") + 
  geom_step(aes(y=snittBNP), color = "dark red", linetype="dashed") + 
  labs (title = "Prosentvis endring i Bruttonasjonalprodukt percapita - BNP per cap \n (gjennomsnitt per tiår) ", 
        x = " ", 
        y = "prosent") + 
  theme_bw()

#ser på endring i BNP og BNP_percap samtidig som man ser utviklingen av BNP_percap.

#Oppgave 3:
library(PxWebApiData)

variabler1 <- ApiData("https://data.ssb.no/api/v0/no/table/09842", returnMetaFrames = TRUE)
names(variabler1)

tabell1 <- ApiData("https://data.ssb.no/api/v0/no/table/09842", Tid = paste(2012:2020), ContentsCode = "BNP")
tabell1


#gjøre listenes variabel <chr> til numeriske verdier (<int>)
bnp <- tabell1[[1]]
str(bnp)

tabell <- tabell %>% 
  select(BNP, År[1865, 1982])

bnp1 <- select(bnp, år, value)
bnp1 <- bnp1 %>% drop_na()

comb_bnp <- bind_rows(tabell, bnp1)

comb_bnp1 <- bind_rows(tabell, variabler1)

comb_bnp1 <- comb_bnp1 %>% drop_na()
  

 