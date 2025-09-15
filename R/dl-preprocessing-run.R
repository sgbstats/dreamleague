suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(rvest)
  library(RCurl)
  library(XML)
  library(googlesheets4)
  library(fuzzyjoin)
  library(crayon)
})
a=Sys.time()

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

if(get_os()=="linux"){
  short=""
}else{
  
  short="C:/R/git/dreamleague/"
}

source(paste0(short,"R/dl-preprocessing.R"))
# gs4_auth(
#   email = T
# )

file_d=paste0(short,"data/DreamLeague25-26.xlsx")
dl_d=readxl::read_excel(file_d, na=c("SOLD"), sheet = "Stats", skip=0, col_names = F)%>%
  suppressMessages() %>% 
  dplyr::select(2:8)
managers_d=readxl::read_excel(file_d, na=c("SOLD"), sheet = "Stats")%>%
  suppressMessages() %>% 
  dplyr::select(11:12) %>% na.omit() %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM") 

mod_d=file.info(file_d)$mtime
cat("Didsbury\n")
out_d=dl_process(dl_d, managers_d, "Didsbury")


file_o=paste0(short,"data/DL25-26.xlsx")
dl_o=readxl::read_excel(file_o, na=c(""), sheet = "Stats", skip=0, col_names = F) %>%
  suppressMessages() %>% 
  dplyr::select(1:7)
managers_o=readxl::read_excel(file_o, na=c("SOLD"), sheet = "Table", skip = 4)%>% 
  suppressMessages() %>% 
  dplyr::select(c(2,4))  %>%
  rename(manager=1,
         team=2) %>%
  filter(team!="TEAM")

mod_o=file.info(file_o)$mtime
cat("Original\n")
dl_o=dl_o %>% mutate(`...7`=case_when(`...2`=="JAMES TRAFFORD"~"45529",
                                      `...2`=="NONI MADUEKE"~"45530",
                                      T~`...7`))



out_o=dl_process(dl_o, managers_o,"Original")

dl_d=out_d$scores
dl_o=out_o$scores
weekly_d=out_d$weekly
weekly_o=out_o$weekly
daily_o=out_o$daily
daily_d=out_d$daily
time=list("update_time"=Sys.time(), "mod_d"=mod_d,"mod_o"=mod_o)

cupties <- read.csv(paste(short,"data/cupties.csv")) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  mutate(across(where(is.character), trimws))


dl=rbind.data.frame(out_d$scores %>% mutate(league="didsbury"),
                    out_o$scores %>% mutate(league="original"))
managers=rbind.data.frame(managers_d %>% mutate(league="didsbury"),
                          managers_o %>% mutate(league="original"))
daily=rbind.data.frame(out_d$daily %>% mutate(league="didsbury"),
                       out_o$daily %>% mutate(league="original"))

save(dl,  daily, time, cupties, file=paste0(short,"dreamleague/data.RDa"))

googledrive::drive_auth(
  email = TRUE,
  path = NULL,
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

googledrive::drive_update(media=paste0(short,"dreamleague/data.RDa"),
                          file=googledrive::as_id("108pNlDYjniFZiPU3PG82bIdChZmZGqUh"),)
b=Sys.time()

difftime(b,a, units = "mins")
