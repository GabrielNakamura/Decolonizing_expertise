
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


