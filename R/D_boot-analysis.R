
# reading M matrices ------------------------------------------------------


M_citations_EastAsia <- readRDS(here::here("data", "processed", "M_EastAsia.rds"))
M_citations_EU <- readRDS(here::here("data", "processed", "M_EU.rds"))
M_citations_la <- readRDS(here::here("data", "processed", "M_LA.rds"))
M_citations_MiddleAfrica <- readRDS(here::here("data", "processed", "M_MiddleAfrica.rds"))
M_citations_Sahara <- readRDS(here::here("data", "processed", "M_Sahara.rds"))
M_citations_usa_can <- readRDS(here::here("data", "processed", "M_USA.rds"))

# general setup of bootstrap procedure ------------------------------------

samp <- 1000
iter <- 100
list_samp_eastasia <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_EastAsia), size = samp, replace = FALSE))
list_samp_middleafrica <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_MiddleAfrica), size = samp, replace = FALSE))
list_samp_eu <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_EU), size = samp, replace = FALSE))
list_samp_la <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_la), size = samp, replace = FALSE))
list_samp_sahara <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_Sahara), size = samp, replace = FALSE))
list_samp_usacan <- lapply(seq_along(along.with = 1:100), function(x) sample(1:nrow(M_citations_usa_can), size = samp, replace = FALSE))


# sampling each region ----------------------------------------------------

samp_eastasia <- lapply(list_samp_eastasia, function(x) M_citations_EastAsia[x,])
samp_eu <- lapply(list_samp_eu, function(x) M_citations_EU[x,])
samp_la <- lapply(list_samp_la, function(x) M_citations_la[x,])
samp_middleafrica <- lapply(list_samp_middleafrica, function(x) M_citations_MiddleAfrica[x,])
samp_sahara <- lapply(list_samp_sahara, function(x) M_citations_Sahara[x,])
samp_usacan <- lapply(list_samp_usacan, function(x) M_citations_usa_can[x,])


# biblio analysis with all bootstrap matrices -----------------------------

res_eastasia <- lapply(samp_eastasia, function(x) biblioAnalysis(x, sep = ";")) 
res_eu <- lapply(samp_eu, function(x) biblioAnalysis(x, sep = ";")) 
res_la <- lapply(samp_la, function(x) biblioAnalysis(x, sep = ";")) 
res_middleafrica <- lapply(samp_middleafrica, function(x) biblioAnalysis(x, sep = ";")) 
res_sahara <- lapply(samp_sahara, function(x) biblioAnalysis(x, sep = ";")) 
res_usacan <- lapply(samp_usacan, function(x) biblioAnalysis(x, sep = ";")) 

saveRDS(res_eastasia, here::here("output", "res_boot_eastasia.rds"))
saveRDS(res_eu, here::here("output", "res_boot_eu.rds"))
saveRDS(res_la, here::here("output", "res_boot_la.rds"))
saveRDS(res_middleafrica, here::here("output", "res_boot_middleafrica.rds"))
saveRDS(res_sahara, here::here("output", "res_boot_sahara.rds"))
saveRDS(res_usacan, here::here("output", "res_boot_usacan.rds"))

# summarizing results from bootstrap --------------------------------------

res_eastasia <- readRDS(here::here("output", "res_boot_eastasia.rds"))
res_eu <- readRDS(here::here("output", "res_boot_eu.rds"))
res_la <- readRDS(here::here("output", "res_boot_la.rds"))
res_middleafrica <- readRDS(here::here("output", "res_boot_middleafrica.rds"))
res_sahara <- readRDS(here::here("output", "res_boot_sahara.rds"))
res_usacan <- readRDS(here::here("output", "res_boot_usacan.rds"))


cit_eastasia <- lapply(res_eastasia, function(x) x$Countries)
long_cit_eastasia <- data.frame(count = names(unlist(cit_eastasia)), ncitations = unlist(cit_eastasia))
library(dplyr)
mean_eastasia <- 
long_cit_eastasia %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_eastasia <- as.data.frame(mean_eastasia[order(mean_eastasia$Mean, decreasing = T), ])


cit_eu <- lapply(res_eu, function(x) x$Countries)
long_cit_eu <- data.frame(count = names(unlist(cit_eu)), ncitations = unlist(cit_eu))
mean_eu <- 
  long_cit_eu %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_eu <- as.data.frame(mean_eu[order(mean_eu$Mean, decreasing = T), ])


cit_la <- lapply(res_la, function(x) x$Countries)
long_cit_la <- data.frame(count = names(unlist(cit_la)), ncitations = unlist(cit_la))
mean_la <- 
  long_cit_la %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_la <- as.data.frame(mean_la[order(mean_la$Mean, decreasing = T), ])

cit_middleafrica <- lapply(res_middleafrica, function(x) x$Countries)
long_cit_middleafrica <- data.frame(count = names(unlist(cit_middleafrica)), ncitations = unlist(cit_middleafrica))
mean_middleafrica <- 
  long_cit_middleafrica %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_middleafrica <- as.data.frame(mean_middleafrica[order(mean_middleafrica$Mean, decreasing = T), ])

cit_sahara <- lapply(res_sahara, function(x) x$Countries)
long_cit_sahara <- data.frame(count = names(unlist(cit_sahara)), ncitations = unlist(cit_sahara))
mean_sahara <- 
  long_cit_sahara %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_sahara <-  as.data.frame(mean_sahara[order(mean_sahara$Mean, decreasing = T), ])

cit_usacan <- lapply(res_usacan, function(x) x$Countries)
long_cit_usacan <- data.frame(count = names(unlist(cit_usacan)), ncitations = unlist(cit_usacan))
mean_usacan <- 
  long_cit_usacan %>% 
  group_by(count) %>% 
  summarize(Mean = mean(ncitations, na.rm = T), std = sd(ncitations, na.rm = T))
cit_usacan <- as.data.frame(mean_usacan[order(mean_usacan$Mean, decreasing = T), ])

# joining data frames

all_cit <- rbind(cit_eastasia, cit_eu, cit_la, cit_middleafrica, cit_sahara, cit_usacan)

wb_reg <- 
rep(x = c("East Asia", "Europe", "Latin Am.", "Middle Africa", "Sahara", "USA-Can"), 
    times = c(dim(cit_eastasia)[1], dim(cit_eu)[1], dim(cit_la)[1], dim(cit_middleafrica)[1], dim(cit_sahara)[1], dim(cit_usacan)[1]))

all_cit$region <- wb_reg # adding region


