# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(htmltools)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2)) +
  theme(axis.text.y = element_blank())

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/data_nd/election_2019_nd/" # parent directory for the data

prepoll <- read_csv(paste0(d,"data/20190516_WEB_Pre-poll_Report_FE2019.csv"), skip = 0)

postal <- read_csv(paste0(d,"data/20190516_WEB_Postal_Report_FE2019.csv"), skip = 0)

formal <- read_csv(paste0(d,"data/HouseVotesCountedByDivisionDownload-20499.csv"), skip = 1)

# primary votes by division 

pd19 <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseFirstPrefsByCandidateByVoteTypeDownload-24310.csv", skip = 1)

pd16 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv"), skip = 1)

pd13 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-17496.csv"), skip = 1)

pd10 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-15508.csv"), skip = 1)

pd07 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-13745.csv"), skip = 1)

# primary votes by polling booth

p19 <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-QLD.csv", skip = 1)

p19_nsw <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-NSW.csv", skip = 1)

p19_vic <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-VIC.csv", skip = 1)

p19_sa <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-SA.csv", skip = 1)

p19_tas <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-TAS.csv", skip = 1)

p19_wa <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-WA.csv", skip = 1)

p19_nt <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-NT.csv", skip = 1)

p19_act <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseStateFirstPrefsByPollingPlaceDownload-24310-ACT.csv", skip = 1)

p16 <- read_csv(paste0(d,"data/HouseStateFirstPrefsByPollingPlaceDownload-20499-QLD.csv"), skip = 1)

p13 <- read_csv(paste0(d,"data/HouseStateFirstPrefsByPollingPlaceDownload-17496-QLD.csv"), skip = 1)

p10 <- read_csv(paste0(d,"data/HouseStateFirstPrefsByPollingPlaceDownload-15508-QLD.csv"), skip = 1)

p07 <- read_csv(paste0(d,"data/HouseStateFirstPrefsByPollingPlaceDownload-13745-QLD.csv"), skip = 1)

# tpp by polling place

t19 <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseTppByPollingPlaceDownload-24310.csv", skip = 1)

t16 <- read_csv(paste0(d,"data/HouseTppByPollingPlaceDownload-20499.csv"), skip = 1)

t13 <- read_csv(paste0(d,"data/HouseTppByPollingPlaceDownload-17496.csv"), skip = 1)

t10 <- read_csv(paste0(d,"data/HouseTppByPollingPlaceDownload-15508.csv"), skip = 1)

t07 <- read_csv(paste0(d,"data/HouseTppByPollingPlaceDownload-13745.csv"), skip = 1)

# tpp by division

tpp19_div <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseTppByDivisionDownload-24310.csv", skip = 1)

tpp16_div <- read_csv(paste0(d,"data/HouseTppByDivisionDownload-20499.csv"), skip = 1)

tpp13_div <- read_csv(paste0(d,"data/HouseTppByDivisionDownload-17496.csv"), skip = 1)

tpp10_div <- read_csv(paste0(d,"data/HouseTppByDivisionDownload-15508.csv"), skip = 1)

tpp07_div <- read_csv(paste0(d,"data/HouseTppByDivisionDownload-13745.csv"), skip = 1)

# import polling booth locations 

pb19 <- read_csv(paste0(d,"data/GeneralPollingPlacesDownload-24310.csv"), skip = 1)

pb16 <- read_csv(paste0(d,"data/GeneralPollingPlacesDownload-20499.csv"), skip = 1)

pb13 <- read_csv(paste0(d,"data/GeneralPollingPlacesDownload-17496.csv"), skip = 1)

pb10 <- read_csv(paste0(d,"data/GeneralPollingPlacesDownload-15508.csv"), skip = 1)

pb07 <- read_csv(paste0(d,"data/GeneralPollingPlacesDownload-13745.csv"), skip = 1)

# two candidate preferred

tcp19 <- read_csv("https://tallyroom.aec.gov.au/Downloads/HouseTcpByCandidateByVoteTypeDownload-24310.csv", skip = 1)

tcp16 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-20499.csv"), skip = 1)

tcp13 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-17496.csv"), skip = 1)

tcp10 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-15508.csv"), skip = 1)

tcp07 <- read_csv(paste0(d,"data/HouseFirstPrefsByCandidateByVoteTypeDownload-13745.csv"), skip = 1)

# other

party_lab <- read_csv(paste0(d,"data/party_lab.csv"), skip = 0)

nats_prim <- read_csv(paste0(d,"data/natsprimary.csv"), skip = 0)

nats_seats <- read_csv(paste0(d,"data/natsseats.csv"), skip = 0)

# INPUTS ---- 

lnp_nats_seats <- c("Capricornia", "Dawson", "Flynn", "Hinkler", "Kennedy", "Maranoa", "Wide Bay", "Lingiari")

clp_nats_seats <- c("Lingiari")

nats_elec <- c("Calare", "Cowper", "Eden-Monaro", "Gilmore", "Hunter", "Lyne", "New England", "Page", "Parkes", "Richmond", "Riverina", "Whitlam", "Lingiari", "Capricornia", "Dawson", "Flynn", "Hinkler", "Kennedy", "Maranoa", "Wide Bay", "Barker", "Bass", "Braddon", "Lyons", "Gippsland", "Indi", "Mallee", "Nicholls", "Durack", "O'Connor", "Pearce", "Ballarat", "Bendigo", "McEwen", "Murray", "Throsby")

nats_elected_19 <- c("Calare", "Cowper", "Lyne", "New England", "Page", "Parkes", "Riverina",  "Capricornia", "Dawson", "Flynn", "Hinkler", "Maranoa", "Wide Bay", "Gippsland", "Mallee", "Nicholls")

coal_seats <- c("Herbert", "Dawson", "Capricornia", "Flynn")

fed_elec <- readOGR(paste0(d,"data/COM_ELB_region.shx"))

# TIDY ----

# tpp by polling place

t19$year <- "2019"

t16$year <- "2016"

t13$year <- "2013"

t10$year <- "2010"

t07$year <- "2007"

tpp <- bind_rows(t19, t16, t13, t10, t07)

tpp <- tpp %>% 
  rename(state = StateAb,
         div_id = DivisionID,
         div = DivisionNm,
         pp_id = PollingPlaceID,
         pp = PollingPlace, 
         v_lnp = "Liberal/National Coalition Votes",
         p_lnp = "Liberal/National Coalition Percentage",
         v_alp = "Australian Labor Party Votes",
         p_alp = "Australian Labor Party Percentage",
         v_t = TotalVotes,
         swing = Swing,
         year = year
  )

# primary vote by polling place

p19 <- bind_rows(p19, p19_nsw, p19_vic, p19_sa, p19_wa, p19_tas, p19_nt, p19_act)

p19$year <- "2019"

p16$year <- "2016"

p13$year <- "2013"

p10$year <- "2010"

p07$year <- "2007"

p_fed <- bind_rows(p19, p16, p13, p10, p07)

p_fed <- p_fed %>% filter(CandidateID != "999")

p_fed <- p_fed %>% 
  select(-CandidateID, -Surname, -GivenNm, -BallotPosition, -Elected, -HistoricElected, -PartyNm)

p_fed <- p_fed %>% 
  rename(state = StateAb,
         div_id = DivisionID,
         div = DivisionNm,
         pp_id = PollingPlaceID,
         pp = PollingPlace, 
         party = PartyAb,
         v = OrdinaryVotes,
         swing = Swing,
         year = year
  )

pb_fed <- bind_rows(pb19, pb16, pb13, pb10, pb07)

pb_fed <- pb_fed %>% 
  select(PollingPlaceID, Latitude, Longitude) %>% 
  rename(pp_id = PollingPlaceID,
         lat = Latitude,
         lon = Longitude)

p_fed$lat <- pb_fed$lat[match(p_fed$pp_id, pb_fed$pp_id)]

p_fed$lon <- pb_fed$lon[match(p_fed$pp_id, pb_fed$pp_id)]

# primary by division 

pd19$year <- "2019"

pd16$year <- "2016"

pd13$year <- "2013"

pd10$year <- "2010"

pd07$year <- "2007"

pd <- bind_rows(pd19, pd16, pd13, pd10, pd07)

pd <- pd %>% 
  select(year, StateAb, DivisionNm, PartyAb, OrdinaryVotes, AbsentVotes, ProvisionalVotes, PrePollVotes, PostalVotes, TotalVotes, Swing) %>% 
  rename(state = "StateAb",
         div = DivisionNm, 
         party = PartyAb,
         o_v = OrdinaryVotes,
         a_v = AbsentVotes, 
         prov_v = ProvisionalVotes,
         pre_v = PrePollVotes,
         post_v = PostalVotes,
         tot_v = TotalVotes, 
         swing = Swing)

# tpp by division

tpp19_div$year <- "2019"

tpp16_div$year <- "2016"

tpp13_div$year <- "2013"

tpp10_div$year <- "2010"

tpp07_div$year <- "2007"

tpp_div <- bind_rows(tpp19_div, tpp16_div, tpp13_div, tpp10_div, tpp07_div)

tpp_div <- tpp_div  %>% 
  select(year, StateAb, DivisionNm, `Liberal/National Coalition Votes`, `Liberal/National Coalition Percentage`, `Australian Labor Party Votes`, `Australian Labor Party Percentage`, TotalVotes, Swing) %>% 
  rename(state = "StateAb",
         div = DivisionNm, 
         v_lnp = `Liberal/National Coalition Votes`,
         p_lnp = `Liberal/National Coalition Percentage`,
         v_alp = `Australian Labor Party Votes`,
         p_alp = `Australian Labor Party Percentage`,
         v_t = TotalVotes, 
         swing = Swing)

# tcp by division

tcp19$year <- "2019"

tcp16$year <- "2016"

tcp13$year <- "2013"

tcp10$year <- "2010"

tcp07$year <- "2007"

tcp <- bind_rows(tcp19, tcp16, tcp13, tcp10, tcp07)

tcp <- tcp  %>% 
  select(year, StateAb, DivisionNm, PartyAb, TotalVotes, Swing) %>% 
  rename(state = "StateAb",
         div = DivisionNm,
         party = PartyAb,
         v_t = TotalVotes, 
         swing = Swing)

# tcp[tcp$div == "New England" & tcp$party == "NP" & tcp$year == "2019", ]$swing <- 13.82

# tcp[tcp$div == "New England" & tcp$party == "IND" & tcp$year == "2019", ]$swing <- -13.82

#tcp_n <- tcp

#tcp_n$party_n <- ifelse(tcp_n$party == "NP", "NAT", ifelse((tcp_n$div %in% lnp_nats_seats & tcp_n$party %in% c("LNP", "LNQ")), "NAT", ifelse((tcp_n$div %in% clp_nats_seats & tcp_n$party == "CLP"), "NAT", tcp_n$party)))

#tcp_n$party_n <- ifelse(tcp_n$party_n %in% c("LNP", "LNQ", "CLP"), "LP", tcp_n$party_n)

tcp_lnp <- tcp %>% 
  filter(party %in% c("LP", "LNP", "CLP", "NP")) %>% 
  select(div, swing)

tcp <- tcp %>% 
  spread(party, v_t) %>% 
  group_by(year, state, div) %>% 
  summarise(v_lnp = sum(c(CLP, LNP, LP, NP), na.rm = T),
            v_alp = sum(c(ALP, GRN, IND, KAP, ON), na.rm = T),
            v_t = v_alp + v_lnp,
            p_lnp = v_lnp / v_t * 100,
            p_alp = v_alp / v_t * 100)

tcp <- left_join(tcp, tcp_lnp, by = "div")

#tcp_by_party <- tcp_n %>% 
 # group_by(year, party_n) %>% 
  #summarise(v = sum(v_t)) %>% 
  #ungroup() %>% 
  #mutate(p = v / sum(v))

# TRANSFORM ---- 


  
# nats analysis

p_fed$party_n <- ifelse(p_fed$party == "NP", "NAT", ifelse((p_fed$div %in% lnp_nats_seats & p_fed$party %in% c("LNP", "LNQ")), "NAT", ifelse((p_fed$div %in% clp_nats_seats & p_fed$party == "CLP"), "NAT", p_fed$party)))

pd$party_n <- ifelse(pd$party == "NP", "NAT", ifelse((pd$div %in% lnp_nats_seats & pd$party %in% c("LNP", "LNQ")), "NAT", ifelse((pd$div %in% clp_nats_seats & pd$party == "CLP"), "NAT", pd$party)))

pd$party_n <- ifelse(pd$party_n %in% c("LNP", "LNQ", "CLP"), "LP", pd$party_n)

pd <- left_join(pd, party_lab, by = "party_n")

pd$lab_coal <- ifelse((pd$div %in% coal_seats & pd$lab %in% c("Nationals", "Liberal")), "LNP", pd$lab)

pd <- pd %>% 
  filter(!is.na(party)) %>% 
  group_by(year, div) %>% 
  mutate(p = tot_v / sum(tot_v))

pd$nats_elected <- ifelse((pd$party_n == "NAT" & pd$year == "2019" & pd$div %in% nats_elected_19), "elected", "not_elected") 

tcp$nats_elected <- ifelse((tcp$year == "2019" & tcp$div %in% nats_elected_19), "elected", "not_elected") 

tpp_div$nats_elected <- ifelse((tpp_div$div %in% nats_elected_19), "elected", "not_elected") 

p_by_pp_nat <- p_fed %>% 
  group_by(div_id, div, pp_id, pp, party_n, year, lat, lon) %>% 
  summarise(v = sum(v)) %>% 
  group_by(year, pp) %>% 
  mutate(p = v / sum(v)) 

pd_by_party <- pd %>% 
  filter(!is.na(party_n)) %>% 
  group_by(party_n, lab, year) %>% 
  summarise(v = sum(tot_v)) %>% 
  group_by(year) %>% 
  mutate(p = v / sum(v)) %>% 
  arrange(party_n, year) %>% 
  ungroup() %>% 
  group_by(party_n, lab) %>% 
  mutate(swing = p - lag(p, 1))

pd_by_party$swing[is.na(pd_by_party$swing)] <- pd_by_party$p[is.na(pd_by_party$swing)]

pd_by_party$swing[pd_by_party$year == "2007"] <- 0

coalition <- pd_by_party %>% 
  filter(party_n %in% c("LP", "NAT") & year  > 2010) %>% 
  group_by(party_n) %>% 
  mutate(index = p / first(p) * 100)

nats_prim <- nats_prim %>% 
  select(Election, NP)

nats_prim2 <- pd_by_party %>% 
  ungroup() %>% 
  filter(party_n == "NAT" & year > 2007) %>% 
  select(year, p) %>% 
  mutate(p = p * 100) %>% 
  rename(Election = year, NP = p) %>% 
  mutate(Election = as.integer(Election))

nats_prim <- bind_rows(nats_prim, nats_prim2)

nats_seats <- nats_seats %>% 
  select(Election, LP, NP, SLP, SNP) %>% 
  mutate(TNP = NP + SNP) %>% 
  mutate(p_np = (NP + SNP) / (LP + SLP + NP + SNP) * 100)

# primary by polling place

p_by_pp <- p_fed %>% 
  group_by(year, pp_id) %>% 
  mutate(p = v / sum(v))

p_by_v <- p_fed %>% 
  group_by(div_id, div, pp_id, pp, year) %>% 
  summarise(v = sum(v))

p_by_pp$lat <- pb_fed$lat[match(p_by_pp$pp_id, pb_fed$pp_id)]

p_by_pp$lon <- pb_fed$lon[match(p_by_pp$pp_id, pb_fed$pp_id)]

p_by_pp_sp <- p_by_pp

p_by_pp_sp <- p_by_pp_sp[!is.na(p_by_pp_sp$lon), ]

coordinates(p_by_pp_sp) <- ~lon + lat

# primary vote by division

# primary votes

pd$party_lnp <- ifelse(pd$party %in% c("LP", "NP", "CLP", "LNP", "LNQ"), "LNP", pd$party)

prim_by_state <- pd %>% 
  group_by(year, state, party, party_n, lab, lab_coal) %>% 
  summarise(v = sum(tot_v)) %>% 
  ungroup() %>% 
  group_by(year, state) %>%
  mutate(p = v / sum(v))

prim_by_state_lnp <- pd %>% 
  group_by(year, state, party_lnp) %>% 
  summarise(v = sum(tot_v)) %>% 
  ungroup() %>% 
  group_by(year, state) %>%
  mutate(p = v / sum(v))

# tpp by polling place

#tpp_by_div <- tpp %>% 
 # group_by(div_id, div, year) %>% 
  #summarise(v_lnp = sum(v_lnp),
   #         v_t = sum(v_t),
    #        p_lnp = v_lnp / v_t)


tpp$lat <- pb_fed$lat[match(tpp$pp_id, pb_fed$pp_id)]

tpp$lon <- pb_fed$lon[match(tpp$pp_id, pb_fed$pp_id)]

tpp_sp <- tpp

tpp_sp <- tpp_sp[!is.na(tpp_sp$lon), ]

coordinates(tpp_sp) <- ~lon + lat


f_tpp_pp <- function(di) {
  
tpp %>% 
  filter(year == "2019" & div == di & p_lnp != 0) %>% 
  ggplot(aes(x = reorder(pp, p_lnp), y = p_lnp))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
  theme_mc +
  labs(title = paste("LNP TPP in", di), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=1.2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
    ylim(0, 110)
}

p_tpp_pp <- map(unique(tpp$div), f_tpp_pp)

names(p_tpp_pp) <- unique(tpp$div)

f_tpp_pp_swing <- function(di) {
  
  tpp %>% 
    filter(year == "2019" & div == di & swing != 0) %>% 
    ggplot(aes(x = reorder(pp, swing), y = swing))  + 
    geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
    theme_mc +
    labs(title = paste("LNP TPP swing in", di), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(swing,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=1.2) +
    theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
    ylim(-20, 40)
}

p_tpp_pp_swing <- map(unique(tpp$div), f_tpp_pp_swing)

names(p_tpp_pp_swing) <- unique(tpp$div)

p_tpp_flynn_1 <- tpp %>% 
  filter(year == "2019" & div == "Flynn" & p_lnp != 0 & p_lnp < 63) %>% 
  ggplot(aes(x = reorder(pp, p_lnp), y = p_lnp))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
  theme_mc +
  labs(title = paste("LNP TPP in", "Flynn"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=1.2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 110)

p_tpp_flynn_2 <- tpp %>% 
  filter(year == "2019" & div == "Flynn" & p_lnp != 0 & p_lnp >= 63) %>% 
  ggplot(aes(x = reorder(pp, p_lnp), y = p_lnp))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
  theme_mc +
  labs(title = paste("LNP TPP in", "Flynn"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=1.2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 110)


p_tpp_flynn_1_swing <- tpp %>% 
  filter(year == "2019" & div == "Flynn" & swing != 0 & swing < 7) %>% 
  ggplot(aes(x = reorder(pp, swing), y = swing))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
  theme_mc +
  labs(title = paste("LNP TPP swing in", "Flynn"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=1.2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(-20, 40)

p_tpp_flynn_2_swing <- tpp %>% 
  filter(year == "2019" & div == "Flynn" & swing != 0 & swing >= 7) %>% 
  ggplot(aes(x = reorder(pp, swing), y = swing))  + 
  geom_bar(stat = "identity", color = line_color, fill = line_color, position = position_dodge(width=.6), width = 0.3 ) +
  theme_mc +
  labs(title = paste("LNP TPP swing in", "Flynn"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing,1))), vjust = -1, size=1.2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(-20, 40)

# tpp by division

tpp_by_party <- tpp_div %>% 
  group_by(year) %>% 
  summarise(v_lnp = sum(v_lnp),
            v_t = sum(v_t),
            p_lnp = v_lnp / v_t,
            v_alp = sum(v_alp),
            p_alp = v_alp / v_t) %>% 
  mutate(swing = p_lnp - lag(p_lnp, 1))

tpp_by_state <- tpp_div %>% 
  group_by(year, state) %>% 
  summarise(v_lnp = sum(v_lnp),
            v_t = sum(v_t),
            p_lnp = v_lnp / v_t,
            v_alp = sum(v_alp),
            p_alp = v_alp / v_t) %>% 
  arrange(state, year) %>% 
  ungroup() %>% 
  mutate(swing = p_lnp - lag(p_lnp, 1))

# CQ / NQ COAL ANALYSIS ---- 


# tpp 

tpp_div$coal <- ifelse(tpp_div$div %in% coal_seats, "coal", "non-coal")

tpp_div$qld <- ifelse(tpp_div$state == "QLD", "qld", "non-qld")

tpp_div$coal_type <- ifelse(tpp_div$coal == "coal", "Coal", ifelse((tpp_div$coal == "non-coal" & tpp_div$state == "QLD"), "Qld (non-coal)", "Rest of Aus"))

tpp_coal_div <- tpp_div %>% 
  filter(div %in% coal_seats) %>% 
  filter(year == "2019")
  
tpp_coal <- tpp_div %>% 
  group_by(coal_type, year) %>% 
  summarise(v_lnp = sum(v_lnp),
            v_t = sum(v_t),
            p_lnp = v_lnp / v_t) %>% 
  mutate(swing = p_lnp - lag(p_lnp))

# primary



# PLOTS 

# tpp 

p_tpp_coal <- tpp_coal %>% 
  filter(year == "2019") %>% 
  ggplot(aes(x = reorder(coal_type, p_lnp), y = p_lnp * 100, fill = coal_type))  + 
  geom_bar(stat = "identity") +
  theme_mc +
  labs(title = paste("LNP TPP in different regions"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c("black", "blue", "maroon")) +
  ylim(0,80)

p_tpp_coal_swing <- tpp_coal %>% 
  filter(year == "2019") %>% 
  ggplot(aes(x = reorder(coal_type, swing), y = swing * 100, fill = coal_type))  + 
  geom_bar(stat = "identity") +
  theme_mc +
  labs(title = paste("LNP TPP swing in different regions"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c("black", "blue", "maroon"), labels = c("non-qld" = "Outside QLD", "qld" = "QLD", "coal" = "Coal seats")) +
  ylim(0, 25)

p_tpp_coal_div <- tpp_coal_div %>% 
  filter(year == "2019" & div %in% coal_seats) %>% 
  ggplot(aes(x = reorder(div, p_lnp), y = p_lnp))  + 
  geom_bar(stat = "identity", fill = line_color) +
  theme_mc +
  labs(title = paste("LNP TPP in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=3) + 
  ylim(0, 80)
  
p_tpp_coal_div_swing <- tpp_coal_div %>% 
  filter(year == "2019" & div %in% coal_seats) %>% 
  ggplot(aes(x = reorder(div, swing), y = swing))  + 
  geom_bar(stat = "identity", fill = line_color) +
  theme_mc +
  labs(title = paste("LNP TPP swing in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing,1))), vjust = -1, size=3) + 
  ylim(0, 25)

# tpp 

p_pd_coal_alp <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "ALP") %>% 
  ggplot(aes(x = reorder(div, p), y = p * 100))  + 
  geom_bar(fill = "red", stat = "identity") +
  theme_mc +
  labs(title = paste("Labor primary vote in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 50)

p_pd_coal_lnp <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "LNP") %>% 
  ggplot(aes(x = reorder(div, p), y = p * 100))  + 
  geom_bar(fill = "blue", stat = "identity") +
  theme_mc +
  labs(title = paste("LNP primary vote in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 50)

p_pd_coal_on <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "ON") %>% 
  ggplot(aes(x = reorder(div, p), y = p * 100))  + 
  geom_bar(fill = "orange", stat = "identity") +
  theme_mc +
  labs(title = paste("One Nation primary vote in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 50)

p_pd_coal_kap <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "KAP") %>% 
  ggplot(aes(x = reorder(div, p), y = p * 100))  + 
  geom_bar(fill = "maroon", stat = "identity") +
  theme_mc +
  labs(title = paste("Katter primary vote in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 50)

p_pd_coal_grn <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "GRN") %>% 
  ggplot(aes(x = reorder(div, p), y = p * 100))  + 
  geom_bar(fill = "green", stat = "identity") +
  theme_mc +
  labs(title = paste("Greens primary vote in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 50)



f_pd_coal <- function(di) {

  pd %>% 
  filter(year == "2019" & div == di) %>% 
  top_n(6, p) %>%
  ggplot(aes(x = reorder(lab_coal, -p), y = p * 100, fill = lab_coal))  + 
  geom_bar(stat = "identity") +
  theme_mc +
  labs(title = paste("Primary vote in", di), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c(LNP = "blue", Labor = "red", "One Nation" = "orange", Katter = "maroon", Palmer = "yellow", Greens = "green")) +
  ylim(0, 50) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_pd_coal <- map(coal_seats, f_pd_coal) 

names(p_pd_coal) <- coal_seats

f_pd_coal_swing <- function(di) {
  
  pd %>% 
    filter(year == "2019" & div == di) %>% 
    top_n(6, p) %>%
    ggplot(aes(x = reorder(lab_coal, -p), y = swing, fill = lab_coal))  + 
    geom_bar(stat = "identity") +
    theme_mc +
    labs(title = paste("Primary vote in", di), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(swing, 1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=3) +
    scale_fill_manual(values = c(LNP = "blue", Labor = "red", "One Nation" = "orange", Katter = "maroon", Palmer = "yellow", Greens = "green", FACN = "dark blue")) +
    ylim(-20, 20)
}

p_pd_coal_swing <- map(coal_seats, f_pd_coal_swing) 

names(p_pd_coal_swing) <- coal_seats

p_pd_coal_alp_swing <- pd %>% 
  filter(year == "2019" & div %in% coal_seats & party == "ALP") %>% 
  ggplot(aes(x = reorder(div, p), y = swing))  + 
  geom_bar(fill = "red", stat = "identity") +
  theme_mc +
  labs(title = paste("Labor primary vote swing in coal seats"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=3) +
  ylim(-30, 30)

# NATS ANALYSIS ---- 

# PLOTS ----

# primary votes by party

f_party <- function(yr) {
pd_by_party %>% 
  ungroup() %>% 
  filter(year == yr) %>% 
  top_n(7, p) %>% 
  ggplot(aes(x = reorder(lab, -p), y = p * 100, fill = lab))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  labs(title = paste("Primary votes -", yr), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c(Liberal = "blue", Labor = "red", Greens = "green", Nationals = "dark green", Independent = "grey", Palmer = "yellow", "One Nation" = "orange", Xenophon = "orange", "Family First" = "light blue", CDP = "light blue")) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0,50)
}

p_party <- map(unique(pd_by_party$year), f_party)

names(p_party) <- unique(pd_by_party$year)

# primary vote swing 

f_party_swing <- function(yr) {
pd_by_party %>% 
  ungroup() %>% 
  filter(year == yr) %>% 
  top_n(7, p) %>% 
  ggplot(aes(x = reorder(lab, -p), y = swing * 100, fill = lab))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  labs(title = paste("Primary vote swing -", yr), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing * 100,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=3) +
  scale_fill_manual(values = c(Liberal = "blue", Labor = "red", Greens = "green", Nationals = "dark green", Independent = "grey", Palmer = "yellow", "One Nation" = "orange", Xenophon = "orange", "Family First" = "light blue", CDP = "light blue")) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(-10,10)
}

p_party_swing <- map(unique(pd_by_party$year), f_party_swing)

names(p_party_swing) <- unique(pd_by_party$year)

# primary votes by party

f_party_year <- function(par) {
  
  max_y <- max(pd_by_party[pd_by_party$lab == par, ]$p) * 1.2
  
  pd_by_party %>% 
    ungroup() %>% 
    filter(lab == par) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat="identity", fill = line_color) + 
    theme_mc +
    labs(title = paste("Primary votes -", par), caption = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, max_y * 100)
}

p_party_year <- map(unique(pd_by_party$lab), f_party_year)

names(p_party_year) <- unique(pd_by_party$lab)

# primary votes by state


f_prim_lnp <- function(st) {
p_prim_lnp <- prim_by_state_lnp %>%
  filter(party_lnp == "LNP" & state == st) %>% 
  ggplot(aes(x = year, y = p * 100))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("LNP primary vote -", st), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 65)
}

p_prim_lnp <- map(unique(prim_by_state_lnp$state), f_prim_lnp)

names(p_prim_lnp) <- unique(prim_by_state_lnp$state)

p_tpp_alp <- tpp_by_party %>% 
  ggplot(aes(x = year, y = p_alp * 100, fill = elected))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  labs(title = paste("Two party preferred for Labor"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_alp * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c(alp = "red", lnp = "blue")) +
  ylim(0, 75)

f_tpp_swing <- function(yr) {
  
  tpp_by_state %>% 
    ungroup() %>% 
    filter(year == yr) %>% 
    ggplot(aes(x = reorder(state, -swing), y = swing * 100, fill = state))  +
    geom_bar(stat="identity") + 
    theme_mc +
    labs(title = paste("TPP swing -", yr), caption = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(swing * 100,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=3) +
    scale_fill_manual(values = c(ACT = "blue", VIC = "blue", QLD = "maroon", WA = "yellow", NSW = "light blue", SA = "red", TAS = "dark green", NT = "orange")) +
    ylim(-5, 5)
}

p_tpp_swing <- map(unique(tpp_by_state$year), f_tpp_swing)

names(p_tpp_swing) <- unique(tpp_by_state$year)


# tpp charts

tpp_by_party$elected <- c("alp", "alp", "lnp", "lnp", "lnp")

p_tpp_lnp <- tpp_by_party %>% 
  ggplot(aes(x = year, y = p_lnp * 100, fill = elected))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  labs(title = paste("Two party preferred for LNP"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c(alp = "red", lnp = "blue")) +
  ylim(0, 75)

p_tpp_alp <- tpp_by_party %>% 
  ggplot(aes(x = year, y = p_alp * 100, fill = elected))  + 
  geom_bar(stat="identity") + 
  theme_mc +
  labs(title = paste("Two party preferred for Labor"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_alp * 100,1))), vjust = -1, size=3) +
  scale_fill_manual(values = c(alp = "red", lnp = "blue")) +
  ylim(0, 75)

f_tpp_swing <- function(yr) {
  
  tpp_by_state %>% 
    ungroup() %>% 
    filter(year == yr) %>% 
    ggplot(aes(x = reorder(state, -swing), y = swing * 100, fill = state))  +
    geom_bar(stat="identity") + 
    theme_mc +
    labs(title = paste("TPP swing -", yr), caption = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(swing * 100,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=3) +
    scale_fill_manual(values = c(ACT = "blue", VIC = "blue", QLD = "maroon", WA = "yellow", NSW = "light blue", SA = "red", TAS = "dark green", NT = "orange")) +
    ylim(-5, 5)
}

p_tpp_swing <- map(unique(tpp_by_state$year), f_tpp_swing)

names(p_tpp_swing) <- unique(tpp_by_state$year)


# tpp by state

tpp_by_state$elected <- ifelse(tpp_by_state$p_lnp > 0.5, "lnp", "alp")

f_tpp_state <- function(st) {
  
  tpp_by_state %>% 
    ungroup() %>% 
    filter(state == st) %>% 
    ggplot(aes(x = year, y = p_lnp * 100, fill = elected))  + 
    geom_bar(stat="identity") + 
    theme_mc +
    labs(title = paste("LNP TPP in", st), caption = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p_lnp * 100,1))), vjust = -1, size=3) +
    scale_fill_manual(values = c(alp = "red", lnp = "blue")) +
    ylim(0, 75)
}

p_tpp_state <- map(unique(tpp_by_state$state), f_tpp_state)

names(p_tpp_state) <- unique(tpp_by_state$state)




# coalition, libs v nats

p_coalition <- coalition %>% 
  ggplot(aes(x = year, y = index, color = lab, group = lab)) + 
  geom_line(size = stroke_size) +
  geom_point() +
  theme_mc +
  labs(title = "Coalition primary vote since 2013", subtitle = "2013 = 100", x ="", y = "") +
  theme(legend.position = "bottom", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank(), legend.key.size = unit(3.5, "mm")) +
  scale_color_manual(values = c("blue", "dark green"), labels = c("Liberal", "Nationals"))

# nats seats

p_nats_elec <- pd %>% 
  filter(year == "2019" & party_n == "NAT") %>% 
  ggplot(aes(x = reorder(div, -p), y = p * 100, fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals primary vote"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(0, 75)

p_nats_swing <- pd %>% 
  filter(year == "2019" & party_n == "NAT") %>% 
  ggplot(aes(x = reorder(div, -swing), y = swing , fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals primary vote swing"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing ,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(-50, 30)

# tpp charts 

p_nats_tpp <- tpp_div %>% 
  filter(year == "2019" & div %in% nats_elec & p_lnp > 0 & nats_elected == "elected") %>% 
  ggplot(aes(x = reorder(div, -p_lnp), y = p_lnp, fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals TPP vote"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(0, 100)

p_nats_swing_tpp <- tpp_div %>% 
  filter(year == "2019" & div %in% nats_elec & p_lnp > 0 & nats_elected == "elected") %>% 
  ggplot(aes(x = reorder(div, -swing), y = swing , fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals TPP swing"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing ,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(-10, 20)

# two candidate preferred charts

p_nats_tcp <- tcp %>% 
  filter(year == "2019" & div %in% nats_elec & p_lnp > 0 & nats_elected == "elected") %>% 
  ggplot(aes(x = reorder(div, -p_lnp), y = p_lnp, fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals TCP vote"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(0, 110)

p_nats_swing_tcp <- tcp %>% 
  filter(year == "2019" & div %in% nats_elec & p_lnp > 0 & nats_elected == "elected") %>% 
  ggplot(aes(x = reorder(div, -swing), y = swing , fill = nats_elected))  + 
  geom_bar(stat = "identity", position = position_dodge(width=.6), width = 0.5 ) +
  theme_mc +
  labs(title = paste("Nationals TCP swing"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(swing ,1)), vjust = ifelse(swing >= 0, -1, 1.5)), size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) + 
  theme(axis.ticks = element_blank()) +
  scale_fill_manual(values = c("dark green", "yellow")) +
  ylim(-25, 25)



# historical plots

p_nats_prim <- nats_prim %>% 
  ggplot(aes(x = Election, y = NP, label = round(NP,1))) + 
  geom_line(color = "light green") + 
  geom_point(colour = "grey", size = 1.5) +
  theme_mc +  
  labs(title = "Nationals primary vote", subtitle = "2013 = 100", x ="", y = "") +
  geom_text(hjust=-0.0, vjust=-0.5, size=2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ylim(0,20) + 
  scale_x_continuous(breaks=c(1920, 1930, 1940, 1950,1960,1970,1980,1990,2000,2010)) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  annotate("rect", xmin=1919, xmax=1929, ymin=0, ymax=20, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1929, xmax=1931, ymin=0, ymax=20, alpha=.1,fill="red") + 
  annotate("rect", xmin=1931, xmax=1941, ymin=0, ymax=20, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1941, xmax=1949, ymin=0, ymax=20, alpha=.1,fill="red")  + 
  annotate("rect", xmin=1949, xmax=1972, ymin=0, ymax=20, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1972, xmax=1975, ymin=0, ymax=20, alpha=.1,fill="red") + 
  annotate("rect", xmin=1975, xmax=1983, ymin=0, ymax=20, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1983, xmax=1996, ymin=0, ymax=20, alpha=.1,fill="red")+ 
  annotate("rect", xmin=1996, xmax=2007, ymin=0, ymax=20, alpha=.1,fill="blue")+ 
  annotate("rect", xmin=2007, xmax=2013, ymin=0, ymax=20, alpha=.1,fill="red") + 
  annotate("rect", xmin=2013, xmax=2019, ymin=0, ymax=20, alpha=.1,fill="blue")

p_nats_seats <- nats_seats %>% 
  ggplot(aes(x = Election, y = TNP, label = round(TNP))) + 
  geom_line(color = "light green") + 
  geom_point(colour = "grey", size = 1.5) +
  theme_mc +  
  labs(title = "Number of Nationals seats", x ="", y = "") +
  geom_text(hjust=-0.0, vjust=-0.5, size=2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks=c(1920, 1930, 1940, 1950,1960,1970,1980,1990,2000,2010)) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  annotate("rect", xmin=1919, xmax=1929, ymin=0, ymax=40, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1929, xmax=1931, ymin=0, ymax=40, alpha=.1,fill="red") + 
  annotate("rect", xmin=1931, xmax=1941, ymin=0, ymax=40, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1941, xmax=1949, ymin=0, ymax=40, alpha=.1,fill="red")  + 
  annotate("rect", xmin=1949, xmax=1972, ymin=0, ymax=40, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1972, xmax=1975, ymin=0, ymax=40, alpha=.1,fill="red") + 
  annotate("rect", xmin=1975, xmax=1983, ymin=0, ymax=40, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1983, xmax=1996, ymin=0, ymax=40, alpha=.1,fill="red")+ 
  annotate("rect", xmin=1996, xmax=2007, ymin=0, ymax=40, alpha=.1,fill="blue")+ 
  annotate("rect", xmin=2007, xmax=2013, ymin=0, ymax=40, alpha=.1,fill="red") + 
  annotate("rect", xmin=2013, xmax=2019, ymin=0, ymax=40, alpha=.1,fill="blue") + 
  ylim(0, 40)

p_nats_prop <- nats_seats %>% 
  ggplot(aes(x = Election, y = p_np, label = round(p_np, 1))) + 
  geom_line(color = "light green") + 
  geom_point(colour = "grey", size = 1.5) +
  theme_mc +  
  labs(title = "Proportion of Nationals in Coalition party room", x ="", y = "") +
  geom_text(hjust=-0.0, vjust=-0.5, size = 1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks=c(1920, 1930, 1940, 1950,1960,1970,1980,1990,2000,2010)) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black")) + 
  annotate("rect", xmin=1919, xmax=1929, ymin=0, ymax=50, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1929, xmax=1931, ymin=0, ymax=50, alpha=.1,fill="red") + 
  annotate("rect", xmin=1931, xmax=1941, ymin=0, ymax=50, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1941, xmax=1949, ymin=0, ymax=50, alpha=.1,fill="red")  + 
  annotate("rect", xmin=1949, xmax=1972, ymin=0, ymax=50, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1972, xmax=1975, ymin=0, ymax=50, alpha=.1,fill="red") + 
  annotate("rect", xmin=1975, xmax=1983, ymin=0, ymax=50, alpha=.1,fill="blue") + 
  annotate("rect", xmin=1983, xmax=1996, ymin=0, ymax=50, alpha=.1,fill="red")+ 
  annotate("rect", xmin=1996, xmax=2007, ymin=0, ymax=50, alpha=.1,fill="blue")+ 
  annotate("rect", xmin=2007, xmax=2013, ymin=0, ymax=50, alpha=.1,fill="red") + 
  annotate("rect", xmin=2013, xmax=2019, ymin=0, ymax=50, alpha=.1,fill="blue") + 
  ylim(0, 50)


# PREPOLL ---- 

# TIDY 

prepoll[is.na(prepoll)] <- 0

prepoll$total <- rowSums(prepoll[, 5:18])

prepoll <- prepoll %>% 
  rename(div = m_div_nm,
         state = m_state_ab)

prepoll$div <- tolower(prepoll$div)

# TRANSFORM 

p_poll <- prepoll %>% 
  group_by(state, div) %>% 
  summarise(prepoll = sum(total)) %>% 
  arrange(-prepoll)

# POSTAL ---- 

# TIDY

postal <- postal[, 1:12]

postal[is.na(postal)] <- 0

postal$postal <- rowSums(postal[, 5:12])

postal <- postal %>% 
  rename(div = PVA_Web_1_Party_Div,
         state = State_Cd)

postal$div <- tolower(postal$div)

# TRANSFORM 

early <- left_join(p_poll, postal[, c(2, 13)], by = "div")

early$total <- early$prepoll + early$postal

formal <- formal %>% 
  rename(div = DivisionNm)

formal$div <- tolower(formal$div)

early <- left_join(early, formal[, c(2, 10)], by = "div")

early <- early %>% 
  rename(v16 = TotalVotes)

early$p <- early$total / early$v16 * 100

early <- early %>% 
  arrange(-p)

# PLOTS 

f_early <- function(st) {
  early %>% 
    filter(state == "st") %>% 
    ggplot(aes(x = reorder(div, -p), y = p))  + 
    geom_bar(stat="identity", fill = line_color) + 
    theme_mc +
    labs(title = paste("Proportion of early votes 2019 -", st), caption = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
    theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
    ylim(0, 75)
}

p_early <- map(unique(early$state), f_early)

names(p_early) <- unique(early$state)

p_qld <- early %>% 
  filter(state == "QLD") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "QLD"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_nsw <- early %>% 
  filter(state == "NSW") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "NSW"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  ylim(0, 75)

p_vic <- early %>% 
  filter(state == "VIC") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "QLD"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_tas <- early %>% 
  filter(state == "TAS") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "TAS"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_sa <- early %>% 
  filter(state == "QLD") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "SA"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_wa <- early %>% 
  filter(state == "WA") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "WA"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_nt <- early %>% 
  filter(state == "NT") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "NT"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

p_act <- early %>% 
  filter(state == "ACT") %>% 
  ggplot(aes(x = reorder(div, -p), y = p))  + 
  geom_bar(stat="identity", fill = line_color) + 
  theme_mc +
  labs(title = paste("Proportion of early votes 2019 -", "ACT"), caption = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p,1))), vjust = -1, size=2) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust = 1)) +
  ylim(0, 75)

# MAPS ---- 

tpp$popup_label <- paste0("<b>", tpp$pp, "</b>", "<br/>", "LNP TPP: ", tpp$p_lnp, "%", "<br/>", "TPP Swing: ", tpp$swing, "%", "<br/>", "Votes: ", tpp$v_t)

p_by_pp$popup_label <- paste0("<b>", p_by_pp$pp, "</b>", "<br/>", "LNP Primary: ", round(p_by_pp$p * 100, 1), "%", "<br/>", "Primary Swing: ", round(p_by_pp$swing, 1), "%")

tpp <- tpp %>% 
  mutate(swing_10 = ifelse(swing >= 10, "Above 10%", "Under 10%"))

tpp <- tpp %>% 
  mutate(swing_15 = ifelse(swing >= 15, "Above 15%", "Under 15%"))


# coal swing

coal_map <- fed_elec[fed_elec$Elect_div %in% coal_seats, ]

tpp_coal <- tpp %>% 
  filter(div %in% coal_seats & year == "2019" & p_lnp != 0) %>% 
  drop_na()

pal <- colorBin(c("red", "pink", "light blue", "blue"), domain = tpp_coal$swing, bins = c(-30, -10, 0, 10, 30))

m_coal <- leaflet(data = tpp_coal) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = tpp_coal %>% filter(swing_10 == "Above 10%"), fillOpacity = 1, color = ~pal(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Above 10%") %>% 
  addCircleMarkers(data = tpp_coal %>% filter(swing_10 == "Under 10%"), fillOpacity = 1, color = ~pal(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Under 10%") %>% 
  addPolygons(data = coal_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = coal_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal, values = c(-30, 30), position = "bottomright") %>% 
  addLayersControl(overlayGroups = c("Above 10%", "Under 10%"))

# coal tpp

pal_coal_tpp <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = tpp_coal$p_lnp, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

m_coal_tpp <- leaflet(data = tpp_coal) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_coal_tpp(p_lnp), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = coal_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = coal_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_coal_tpp, values = c(0, 100), position = "bottomright") 

# saveWidget(m_coal_tpp, file="m_coal.html")
  
# qld

qld_map <- fed_elec[fed_elec$State == "QLD", ]

tpp_qld <- tpp %>% 
  filter(state == "QLD" & year == "2019" & p_lnp != 0) %>% 
  drop_na()

pal_qld <- colorBin(c("red", "pink", "light blue", "blue"), domain = tpp_qld$swing, bins = c(-30, -10, 0, 10, 30))

m_qld <- leaflet(data = tpp_qld) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal(swing), radius = 3, stroke = FALSE, popup = ~paste0("<b>", pp, "</b>", "<br/>", "LNP TPP: ", p_lnp, "<br/>", "TPP Swing: ", swing)) %>% 
  addPolygons(data = qld_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = qld_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>%  
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_qld, values = c(-30, 30), position = "bottomright")

# qld tpp

pal_qld_tpp <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = tpp_qld$p_lnp, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

m_qld_tpp <- leaflet(data = tpp_qld) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_qld_tpp(p_lnp), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = qld_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = qld_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_qld_tpp, values = c(0, 100), position = "bottomright") 



# aus

tpp_aus <- tpp %>% 
  filter(year == "2019" & p_lnp != 0) %>% 
  drop_na()

pal_aus <- colorBin(c("red", "#F08080", "pink", "light blue", "blue", "#00304E"), domain = tpp_aus$swing, bins = c(-30, -20, -10, 0, 10, 20, 30))

m_aus <- leaflet(data = tpp_aus) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = tpp_aus %>% filter(swing_10 == "Under 10%"), fillOpacity = 1, color = ~pal_aus(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Under 10%") %>% 
  addCircleMarkers(data = tpp_aus %>% filter(swing_10 == "Above 10%"), fillOpacity = 1, color = ~pal_aus(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Above 10%") %>% 
  addPolygons(data = fed_elec, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = fed_elec$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_aus, values = c(-30, 30), position = "bottomright") %>% 
  addLayersControl(overlayGroups = c("Above 10%", "Under 10%"))

# saveWidget(m_aus, file="m_aus.html", selfcontained = F)

# aus tpp

pal_aus_tpp <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = tpp$p_lnp, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

m_aus_tpp <- leaflet(data = tpp_aus) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_aus_tpp(p_lnp), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = fed_elec, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = fed_elec$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_aus_tpp, values = c(0, 100), position = "bottomright") 

# saveWidget(m_aus_tpp, file="m_aus_tpp.html", selfcontained = F)

# one nation primary

p_fed_on <- p_by_pp %>% 
  filter(year == "2019" & party == "ON") %>% 
  drop_na()

p_fed_on$p <- round(p_fed_on$p * 100, 2)

p_fed_on$popup_label <- paste0("<b>", p_fed_on$pp, "</b>", "<br/>", "One Nation primary: ", p_fed_on$p, "<br/>", "One Nation Swing: ", p_fed_on$swing, "<br/>")

pal_on <- colorBin(c("#ffebcc", "#ffd699", "#ffc266", "#ffad33", "#ff9900", "#cc7a00", "#995c00"), domain = p_fed_on$p, bins = c(0, 5, 10, 15, 20, 25, 30, 50))

m_on <- leaflet(data = p_fed_on) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_on(p), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = fed_elec, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = fed_elec$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "One Nation primary (%)", pal = pal_on, values = c(0, 50), position = "bottomright") 

# saveWidget(m_on, file="m_on.html", selfcontained = F)


# green primary

p_fed_grn <- p_by_pp %>% 
  filter(year == "2019" & party == "GRN") %>% 
  drop_na()

p_fed_grn$p <- round(p_fed_grn$p * 100, 2) 

p_fed_grn$popup_label <- paste0("<b>", p_fed_grn$pp, "</b>", "<br/>", "Greens primary: ", p_fed_grn$p, "<br/>", "Greens Swing: ", p_fed_grn$swing, "<br/>")

pal_grn <- colorBin(c("#d6f5d6", "#adebad", "#5cd65c", "#33cc33", "#29a329", "#1f7a1f"), domain = p_fed_grn$p, bins = c(0, 5, 10, 15, 20, 25, 70))

m_grn <- leaflet(data = p_fed_grn) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_grn(p), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = fed_elec, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = fed_elec$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "Greens primary (%)", pal = pal_grn, values = c(0, 70), position = "bottomright") 

# saveWidget(m_grn, file="m_grn.html", selfcontained = F)

# CAPRICORNIA ----

cap_reg <- read_csv(paste0(d,"data/cap_pp.csv"), skip = 0)

prim_cap <- p_fed %>% 
  filter(div == "Capricornia")

prim_cap <- left_join(prim_cap, cap_reg, by = "pp")

prim_cap_reg <- prim_cap %>%
  group_by(party_n, year, region) %>% 
  summarise(v = sum(v)) %>% 
  group_by(year, region) %>% 
  mutate(p = v / sum(v)) %>% 
  ungroup() %>% 
  arrange(party_n, region, year) %>% 
  group_by(region) %>% 
  mutate(s = p - lag(p, 1))

f_prim_cap_lnp <- function(reg) {
  
  prim_cap_reg %>% 
    filter(party_n == "NAT" & region == reg) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "blue") +
    theme_mc +
    labs(title = paste("LNP votes in", reg, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 60) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_cap_lnp <- map(unique(prim_cap_reg$region), f_prim_cap_lnp) 

names(p_prim_cap_lnp) <- unique(prim_cap_reg$region)

f_prim_cap_alp <- function(reg) {
  
  prim_cap_reg %>% 
    filter(party_n == "ALP" & region == reg) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "red") +
    theme_mc +
    labs(title = paste("Labor votes in", reg, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 70) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_cap_alp <- map(unique(prim_cap_reg$region), f_prim_cap_alp) 

names(p_prim_cap_alp) <- unique(prim_cap_reg$region)

p_prim_cap_on <- prim_cap_reg %>% 
  filter(party_n == "ON" & year == "2019" & region != "Other") %>% 
  ggplot(aes(x = reorder(region, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "orange") +
  theme_mc +
  labs(title = paste("One Nation vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 70) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))

# DAWSON ----

# tidy

daw_reg <- read_csv(paste0(d,"data/daw_pp.csv"), skip = 0)

prim_daw <- p_by_pp %>% 
  filter(div == "Dawson")

prim_daw <- left_join(prim_daw, daw_reg, by = "pp")

prim_daw_19 <- prim_daw %>% 
  filter(year == "2019")

prim_daw_19_lnp <- prim_daw_19 %>% 
  filter(party == "LNP") %>% 
  mutate(p = p * 100)

tpp_daw <- tpp %>% 
  filter(div %in% "Dawson" & p_lnp != 0) %>% 
  drop_na()

tpp_daw <- left_join(tpp_daw, daw_reg, by = "pp")

tpp_daw_19 <- tpp_daw %>% 
  filter(year == "2019" & v_t > 100)



# transform

prim_daw_reg <- prim_daw %>%
  group_by(party_n, year, region) %>% 
  summarise(v = sum(v)) %>% 
  group_by(year, region) %>% 
  mutate(p = v / sum(v)) %>% 
  ungroup() %>% 
  arrange(party_n, region, year) %>% 
  group_by(region) %>% 
  mutate(s = p - lag(p, 1))

prim_daw_subreg <- prim_daw %>%
  group_by(party_n, year, subregion) %>% 
  summarise(v = sum(v)) %>% 
  group_by(year, subregion) %>% 
  mutate(p = v / sum(v)) %>% 
  ungroup() %>% 
  arrange(party_n, subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(s = p - lag(p, 1))

prim_daw_subreg$party_rw <- ifelse(prim_daw_subreg$party_n %in% c("CEC", "FACN", "FFP", "KAP", "ON", "PUP", "UAPP"), "RW", prim_daw_subreg$party_n)

prim_daw_subreg_rw <- prim_daw_subreg %>% 
  group_by(party_rw, subregion, year) %>% 
  summarise(v = sum(v)) %>% 
  group_by(year, subregion) %>% 
  mutate(p = v / sum(v)) %>% 
  ungroup() %>% 
  arrange(party_rw, subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(s = p - lag(p, 1))

tpp_daw_reg <- tpp_daw %>%
  group_by(year, region) %>% 
  summarise(v_lnp = sum(v_lnp),
            v_alp = sum(v_alp),
            v_t = sum(v_t)) %>% 
  group_by(year, region) %>% 
  mutate(p_lnp = v_lnp / v_t,
         p_alp = v_alp / v_t) %>% 
  ungroup() %>% 
  arrange(region, year) %>% 
  group_by(region) %>% 
  mutate(s = p_lnp - lag(p_lnp, 1))

tpp_daw_subreg <- tpp_daw %>%
  group_by(year, subregion) %>% 
  summarise(v_lnp = sum(v_lnp),
            v_alp = sum(v_alp),
            v_t = sum(v_t)) %>% 
  group_by(year, subregion) %>% 
  mutate(p_lnp = v_lnp / v_t,
         p_alp = v_alp / v_t) %>% 
  ungroup() %>% 
  arrange(subregion, year) %>% 
  group_by(subregion) %>% 
  mutate(s = p_lnp - lag(p_lnp, 1))

# maps

# swing tpp

daw_map <- fed_elec[fed_elec$Elect_div %in% "Dawson", ]

pal_daw <- colorBin(c("#ff1414", "#ff4e4e", "#ff8989", "#ffc4c4" ,"#c4c4ff" , "#8989ff" , "#4e4eff", "#1414ff"), domain = tpp_daw$swing, bins = c(-20, -15, -10, -5, 0, 5, 10, 15, 20))

m_daw <- leaflet(data = tpp_daw_19) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = tpp_daw_19, fillOpacity = 1, color = ~pal_daw(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Above 15%") %>% 
  addCircleMarkers(data = tpp_daw_19 %>% filter(swing_15 == "Under 15%"), fillOpacity = 1, color = ~pal_daw(swing), radius = 3, stroke = FALSE, popup = ~popup_label, group = "Under 15%") %>% 
  addPolygons(data = daw_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = daw_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "TPP Swing to LNP (%)", pal = pal_daw, values = c(-30, 30), position = "bottomright") %>% 
  addLayersControl(overlayGroups = c("Above 15%", "Under 15%"))

# swing prim

pal_daw_prim_s <- colorBin(c("#d80000", "#ff1414", "#ff4e4e", "#ff8989", "#ffc4c4" ,"#c4c4ff" , "#8989ff" , "#4e4eff", "#1414ff", "#0000d8"), domain = tpp_daw$swing, bins = c(-50, -20, -15, -10, -5, 0, 5, 10, 15, 20, 50))

m_daw_prim_s <- leaflet(data = prim_daw_19_lnp) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = prim_daw_19_lnp, fillOpacity = 1, color = ~pal_daw_prim_s(swing), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = daw_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = daw_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "Primary Swing to LNP (%)", pal = pal_daw_prim_s, values = c(-50, 50), position = "bottomright")

# prim

pal_daw_prim <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = tpp_coal$p_lnp, bins = c(0, 30, 35, 40, mean(prim_daw_19_lnp$p, na.rm = T), 45, 50, 55, 100))

m_daw_prim <- leaflet(data = prim_daw_19_lnp) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(data = prim_daw_19_lnp, fillOpacity = 1, color = ~pal_daw_prim(p), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = daw_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = daw_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "Primary LNP vote (%)", pal = pal_daw_prim, values = c(0, 100), position = "bottomright")

# tpp

pal_daw_tpp <- colorBin(c("#990000", "#ff0000", "#ff9999", "#ffcccc", "#9999ff", "#ccccff", "#0000ff", "#0000b3"), domain = tpp_coal$p_lnp, bins = c(0, 30, 40, 45, 50, 55, 60, 70, 100))

m_daw_tpp <- leaflet(data = tpp_daw_19) %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(fillOpacity = 1, color = ~pal_daw_tpp(p_lnp), radius = 3, stroke = FALSE, popup = ~popup_label) %>% 
  addPolygons(data = daw_map, color = "#696969", weight = 0.5, opacity = 1, fill = FALSE, label = daw_map$Elect_div, highlight = highlightOptions(weight = 2, color = "black", bringToFront = TRUE)) %>% 
  addLegend(title = "LNP TPP (%)", pal = pal_daw_tpp, values = c(0, 100), position = "bottomright") 

saveWidget(m_daw_tpp, file="m_daw_tpp.html")


# plots

# primary by subregion in 2019

p_prim_daw_subreg <- prim_daw_subreg %>% 
  filter(party_n == "NAT" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "blue") +
  theme_mc +
  labs(title = paste("LNP primary vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 60) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5))

# primary swing by subregion in 2019

p_prim_daw_subreg_s <- prim_daw_subreg %>% 
  filter(party_n == "NAT" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -s), y = s * 100))  + 
  geom_bar(stat = "identity", fill = "blue") +
  theme_mc +
  labs(title = paste("LNP primary vote swing in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(s * 100,1)), vjust = ifelse(s >= 0, -1, 1.5)), size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(-10,10)

# alp primary by subregion in 2019

p_prim_daw_subreg_alp <- prim_daw_subreg %>% 
  filter(party_n == "ALP" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "red") +
  theme_mc +
  labs(title = paste("ALP primary vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 40) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5))

# alp primary swing by subregion in 2019

p_prim_daw_subreg_s_alp <- prim_daw_subreg %>% 
  filter(party_n == "ALP" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -s), y = s * 100))  + 
  geom_bar(stat = "identity", fill = "red") +
  theme_mc +
  labs(title = paste("ALP primary vote swing in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(s * 100,1)), vjust = ifelse(s >= 0, -1, 1.5)), size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(-30,10)

# green primary by subregion in 2019

p_prim_daw_subreg_grn <- prim_daw_subreg %>% 
  filter(party_n == "GRN" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "green") +
  theme_mc +
  labs(title = paste("Greens primary vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 20) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5))

# greens primary swing by subregion in 2019

p_prim_daw_subreg_s_grn <- prim_daw_subreg %>% 
  filter(party_n == "GRN" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -s), y = s * 100))  + 
  geom_bar(stat = "identity", fill = "green") +
  theme_mc +
  labs(title = paste("Greens primary vote swing in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(s * 100,1)), vjust = ifelse(s >= 0, -1, 1.5)), size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(-5,5)

# rw primary by subregion in 2019

p_prim_daw_subreg_rw <- prim_daw_subreg_rw %>% 
  filter(party_rw == "RW" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "orange") +
  theme_mc +
  labs(title = paste("Right wing minor parties primary vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 40) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5))

# rw primary swing by subregion in 2019

p_prim_daw_subreg_s_rw <-  prim_daw_subreg_rw %>% 
  filter(party_rw == "RW" & year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -s), y = s * 100))  + 
  geom_bar(stat = "identity", fill = "orange") +
  theme_mc +
  labs(title = paste("Right wing minor parties primary swing in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(s * 100,1)), vjust = ifelse(s >= 0, -1, 1.5)), size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(0, 30)

# tpp by subregion in 2019

p_tpp_daw_subrg <- tpp_daw_subreg %>% 
  filter(year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -p_lnp), y = p_lnp * 100))  + 
  geom_bar(stat = "identity", fill = "blue") +
  theme_mc +
  labs(title = paste("LNP TPP vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p_lnp * 100,1))), vjust = -1, size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(0, 80)

# tpp swing by subregion in 2019

p_tpp_daw_subrg_s <- tpp_daw_subreg %>% 
  filter(year == "2019" & subregion != "other") %>% 
  ggplot(aes(x = reorder(subregion, -s), y = s * 100))  + 
  geom_bar(stat = "identity", fill = "blue") +
  theme_mc +
  labs(title = paste("LNP TPP vote swing in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(s * 100,1)), vjust = ifelse(s >= 0, -1, 1.5)), size=3) +
  theme(axis.text = element_text(size = 7, vjust = 0.3, hjust = 0.5)) +
  ylim(0,20)

# lnp primary vote by region by year

f_prim_daw_lnp <- function(reg) {
  
  prim_daw_reg %>% 
    filter(party_n == "NAT" & region == reg) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "blue") +
    theme_mc +
    labs(title = paste("LNP votes in", reg, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 60) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_daw_lnp <- map(unique(prim_daw_reg$region), f_prim_daw_lnp) 

names(p_prim_daw_lnp) <- unique(prim_daw_reg$region)

# lnp primary vote by subregion by year

f_prim_daw_lnp_sub <- function(subr) {
  
  prim_daw_subreg %>% 
    filter(party_n == "NAT" & subregion == subr) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "blue") +
    theme_mc +
    labs(title = paste("LNP votes in", subr, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 60) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_daw_lnp_sub <- map(unique(prim_daw_subreg$subregion), f_prim_daw_lnp_sub) 

names(p_prim_daw_lnp_sub) <- unique(prim_daw_subreg$subregion)

# lnp tpp vote by subregion by year

f_tpp_daw_lnp_sub <- function(subr) {
  
  tpp_daw_subreg %>% 
    filter(subregion == subr) %>% 
    ggplot(aes(x = year, y = p_lnp * 100))  + 
    geom_bar(stat = "identity", fill = "blue") +
    theme_mc +
    labs(title = paste("LNP primary vote in", subr, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p_lnp * 100,1))), vjust = -1, size=3) +
    ylim(0, 80) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_tpp_daw_lnp_sub <- map(unique(prim_daw_subreg$subregion), f_tpp_daw_lnp_sub) 

names(p_tpp_daw_lnp_sub) <- unique(prim_daw_subreg$subregion)

# alp primary vote by region by year

f_prim_daw_alp <- function(reg) {
  
  prim_daw_reg %>% 
    filter(party_n == "ALP" & region == reg) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "red") +
    theme_mc +
    labs(title = paste("Labor votes in", reg, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 70) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_daw_alp <- map(unique(prim_daw_reg$region), f_prim_daw_alp) 

names(p_prim_daw_alp) <- unique(prim_daw_reg$region)

# alp primary vote by subregion by year

f_prim_daw_alp_sub <- function(subr) {
  
  prim_daw_subreg %>% 
    filter(party_n == "ALP" & subregion == subr) %>% 
    ggplot(aes(x = year, y = p * 100))  + 
    geom_bar(stat = "identity", fill = "red") +
    theme_mc +
    labs(title = paste("LNP votes in", subr, "region"), subtitle = "", x = "", y = "") +
    theme(panel.grid.major = element_blank()) +
    geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
    ylim(0, 60) +
    theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))
}

p_prim_daw_alp_sub <- map(unique(prim_daw_subreg$subregion), f_prim_daw_alp_sub) 

names(p_prim_daw_alp_sub) <- unique(prim_daw_subreg$subregion)

# one nation primary vote by region in 2019

p_prim_daw_on <- prim_daw_reg %>% 
  filter(party_n == "ON" & year == "2019" & region != "Other") %>% 
  ggplot(aes(x = reorder(region, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "orange") +
  theme_mc +
  labs(title = paste("One Nation vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 70) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))



p_prim_daw_kap <- prim_daw_reg %>% 
  filter(party_n == "KAP" & year == "2019" & region != "Other") %>% 
  ggplot(aes(x = reorder(region, -p), y = p * 100))  + 
  geom_bar(stat = "identity", fill = "maroon") +
  theme_mc +
  labs(title = paste("Katter vote in 2019"), subtitle = "", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(p * 100,1))), vjust = -1, size=3) +
  ylim(0, 70) +
  theme(axis.text = element_text(size = 8, vjust = 0.3, hjust = 0.5))


# EXPORT ---- 

png("images/p_qld.png", width = 6, height = 3, units = "in", res = 300)
p_qld
dev.off() 

png("images/p_nsw.png", width = 6, height = 3, units = "in", res = 300)
p_nsw
dev.off() 

png("images/p_vic.png", width = 6, height = 3, units = "in", res = 300)
p_vic
dev.off() 

png("images/p_tas.png", width = 6, height = 3, units = "in", res = 300)
p_tas
dev.off() 

png("images/p_sa.png", width = 6, height = 3, units = "in", res = 300)
p_sa
dev.off() 

png("images/p_wa.png", width = 6, height = 3, units = "in", res = 300)
p_wa
dev.off() 

png("images/p_nt.png", width = 6, height = 3, units = "in", res = 300)
p_nt
dev.off() 

png("images/p_act.png", width = 6, height = 3, units = "in", res = 300)
p_act
dev.off() 
