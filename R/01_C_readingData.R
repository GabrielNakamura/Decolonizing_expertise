
library(bibliometrix)
library(here)
library(dplyr)
files <- here::here("data", "50_most_cited_LA.txt")
M <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
results <- biblioAnalysis(M, sep = ";")
M_single <- M[which(unlist(lapply(strsplit(x = M$AU, split = ";"), function(x) length(x))) == 1), ]
q_top_LA <- glue::glue_collapse(M_single$UT,  sep = ' OR ')


# papers most cited Canada and USA ----------------------------------------

files_can_usa <- here::here("data", "papers_CAN_USA.txt")
M_can_usa <- convert2df(file = files_can_usa, dbsource = 'wos', format = "plaintext")
results_can_usa <- biblioAnalysis(M_can_usa, sep = ";")
M_single_can_usa <- M_can_usa[which(unlist(lapply(strsplit(x = M_can_usa$AU, split = ";"), function(x) length(x))) == 1), ]
q_top_can_usa <- glue::glue_collapse(M_single_can_usa$UT,  sep = ' OR ')

# citation analysis for single authors from usa and canada
files <- here::here("data", "citation_USA_CAN.txt")
usa_can_citation <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
results_usa_can <- biblioAnalysis(usa_can_citation, sep = ";")
usa_prop_citation <- sort(results_usa_can$Countries/sum(results_can_usa$Countries))


# citations for LA single author articles 

la_citation <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
results_la <- biblioAnalysis(la_citation, sep = ";")
results_la$FirstAffiliation
la_prop_citation <- sort(results$Countries/sum(results$Countries))


# papers top cited EU -----------------------------------------------------

files <- here::here("data", "papers_EU_500.txt")
M_eu <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
results_eu <- biblioAnalysis(M_eu, sep = ";")
M_single_eu <- M_eu[which(unlist(lapply(strsplit(x = M_eu$AU, split = ";"), function(x) length(x))) == 1), ]
q_top_eu <- glue::glue_collapse(M_single_eu$UT,  sep = ' OR ')

# citations for a single author top-cited papers


files <- here::here("data", "citation_EU.txt")
eu_citation <- convert2df(file = files, dbsource = 'wos', format = "plaintext")
results_eu <- biblioAnalysis(eu_citation, sep = ";")
eu_prop_citation <- sort(results_eu$Countries/sum(results_eu$Countries))

df <- merge(stack(usa_prop_citation), stack(eu_prop_citation), by= "ind", all.x = TRUE, all.y = TRUE)
all_prop_citation <- 
df %>% 
  left_join(stack(la_prop_citation), by = "ind")
