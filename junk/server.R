library(shiny)
library(tidyverse)
library(shinyWidgets)
library(lubridate)
library(readxl)
library(ggplot2)
library(ggsankey)
library(htmltools)
library(flextable)
library(htmlwidgets)
library(shinydashboard)

library(googlesheets4)
`%notin%`=Negate(`%in%`)
options(gargle_oauth_cache = ".secrets")
gs4_auth(
  cache = ".secrets",
  email = "sebastiangbate@gmail.com",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"
)

dl=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1dKUl4hpZ0SnqqLoZk5IpJwISKoMj7o0WNoeUoLebc8s/edit#gid=0",
                             na=c("SOLD",""),
                             col_names = T)

managers=tribble(~manager, ~team,
                 "BRYN COOMBE",	"DURHAM DYNAMO",
                 "ROB PICKETT",	"ROBS ROVERS",
                 "JOE HARPER",	"CASEMIGOS",
                 "SEAN HELSBY",	"SHEFSPANYOL",
                 "MIKE ASTLEY","SPORTING HOOF",
                 "MARK WHITTLE",	"RASH IN THE ATTIC",
                 "RICHARD KILPATRICK",	"ERIMUS",
                 "CHRIS DUFFY",	"NUTMEG UTD",
                 "SEB BATE",	"MICHU IN DE GEA BAR",
                 "SAM HARPER",	"ONE AND A HALF A HARPER FC",
                 "TAMIKO JELENJE",	"DREAMVILLE",
                 "CHRIS MCDERMOTT & ADG",	"TIKI TAKA FC",
                 "JAMIE DUGGAN",	"DUGGAN'S DREAMERS",
                 "CHARLIE REED",	"CHARLIE'S ANGELS"
)


league=managers %>% merge(dl %>% group_by(team) %>% 
                            summarise(total=sum(SBgoals)), by="team") %>% 
  merge(dl%>% filter(position != "GOALKEEPER") %>% group_by(team) %>% 
          summarise(gf=sum(SBgoals)), by="team") %>% 
  merge(dl %>% filter(position == "GOALKEEPER") %>% group_by(team) %>% 
          summarise(ga=-sum(SBgoals)), by="team") %>% 
  arrange(-total,-gf)



output$table=renderUI({
  league%>% flextable() %>% 
    set_header_labels(team="Team",
                      manager="Manager",
                      total="Total",
                      gf="For",
                      ga="Against") %>% 
    bg(i=1:2, bg=c("#FFD700", "#C0C0C0")) %>% 
    autofit() %>%
    font(fontname = "Arial", part="all") %>% 
    htmltools_value()
})