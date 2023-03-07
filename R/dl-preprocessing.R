library(tidyverse)
library(readxl)


dl=readxl::read_excel("data/DreamLeague22-23.xlsx", na=c("SOLD"))

teams=dl %>% dplyr::select(2:8) %>% 
  rename(position=1,
         player=2,
         club=3,
         cost=4,
         goals=5,
         sold=7,
         bought=6) %>% 
  mutate(team=NA_character_)

managers=dl %>% dplyr::select(11:12) %>% na.omit() %>% 
  rename(manager=1,
         team=2) %>% 
  filter(team!="TEAM")

teams2=teams
for(i in 1:nrow(teams))
{
  if(teams$player[i] %in% managers$team)
  {
    team=teams$player[i]
  }
  if(teams$position[i] %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"))
  {
    teams2$team[i]=team
  }
}

teams2=teams2 %>% filter(position %in% c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")) %>% 
  mutate(goals=if_else(position=="GOALKEEPER", -abs(as.numeric(goals)), as.numeric(goals)),
         bought=format(as.Date(bought), "%d-%b"),
         sold=format(as.Date(sold), "%d-%b"))

league=managers %>% merge(teams2 %>% group_by(team) %>% 
                            summarise(total=sum(goals)), by="team") %>% 
  merge(teams2 %>% filter(goals>0) %>% group_by(team) %>% 
          summarise(gf=sum(goals)), by="team") %>% 
  merge(teams2 %>% filter(goals<0) %>% group_by(team) %>% 
          summarise(ga=-sum(goals)), by="team") %>% 
  arrange(-total,-gf)

save(teams2, league, file = "dreamleague/Data.RDa")
save(teams2, league, file = "data/Data.RDa")
