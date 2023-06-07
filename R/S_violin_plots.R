library(hrbrthemes)
library(ggplot2)
library(bibliometrix)
library(cowplot)


safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

color_map <- c("Latin America" = "#117733", 
               "East Asia" = "#999933",
               "Europe" = "#88CCEE", 
               "Middle Africa" = "#AA4499",
               "USA and Canada" = "#661100", 
               "South Asia" = "#DDCC77",
               "Sahara" = "#888888")


names_top_usacan <- all_cit[which(all_cit$region == "USA-Can"), ][1:10, "count"]
count_top_usacan <- long_cit_usacan[which(is.na(match(long_cit_usacan$count, names_top_usacan)) != TRUE), ]
count_top_usacan_order <- count_top_usacan[order(count_top_usacan$ncitations, decreasing = T), ]
count_top_usacan_order$region.wb <- NA
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_LA), "region.wb"] <- "Latin America"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_EastAsia), "region.wb"] <- "East Asia"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_EU), "region.wb"] <- "Europe"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_MidleEastAfrica), "region.wb"] <- "Middle Africa"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_NA), "region.wb"] <- "USA and Canada"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_SouthAsia), "region.wb"] <- "South Asia"
count_top_usacan_order[count_top_usacan_order$count %in% toupper(names_SubSahara), "region.wb"] <- "Sahara"

viol_usacan <- 
  count_top_usacan_order %>%
  mutate(ordcount = forcats::fct_reorder(count, ncitations)) %>% # Reorder data
  ggplot( aes(x=ordcount, y=ncitations, fill=region.wb)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_manual(name = "Region", values = color_map) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("number of citations in bootstrap sample")


names_top_eastasia <- all_cit[which(all_cit$region == "East Asia"), ][1:10, "count"]
count_top_eastasia <- long_cit_eastasia[which(is.na(match(long_cit_eastasia$count, names_top_eastasia)) != TRUE), ]
count_top_eastasia_order <- count_top_eastasia[order(count_top_eastasia$ncitations, decreasing = T), ]
count_top_eastasia_order$region.wb <- NA
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_LA), "region.wb"] <- "Latin America"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_EastAsia), "region.wb"] <- "East Asia"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_EU), "region.wb"] <- "Europe"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_MidleEastAfrica), "region.wb"] <- "Middle Africa"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_NA), "region.wb"] <- "USA and Canada"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_SouthAsia), "region.wb"] <- "South Asia"
count_top_eastasia_order[count_top_eastasia_order$count %in% toupper(names_SubSahara), "region.wb"] <- "Sahara"

viol_eastasia <- 
  count_top_eastasia_order %>%
  mutate(ordcount = forcats::fct_reorder(count, ncitations)) %>% # Reorder data
  ggplot( aes(x=ordcount, y=ncitations, fill=region.wb)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_manual(name = "Region", values = color_map) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Number of citations in bootstrap sample")


names_top_la <- all_cit[which(all_cit$region == "Latin Am."), ][1:10, "count"]
count_top_la <- long_cit_la[which(is.na(match(long_cit_la$count, names_top_la)) != TRUE), ]
count_top_la_order <- count_top_la[order(count_top_la$ncitations, decreasing = T), ]
count_top_la_order$region.wb <- NA
count_top_la_order[count_top_la_order$count %in% toupper(names_LA), "region.wb"] <- "Latin America"
count_top_la_order[count_top_la_order$count %in% toupper(names_EastAsia), "region.wb"] <- "East Asia"
count_top_la_order[count_top_la_order$count %in% toupper(names_EU), "region.wb"] <- "Europe"
count_top_la_order[count_top_la_order$count %in% toupper(names_MidleEastAfrica), "region.wb"] <- "Middle Africa"
count_top_la_order[count_top_la_order$count %in% toupper(names_NA), "region.wb"] <- "USA and Canada"
count_top_la_order[count_top_la_order$count %in% toupper(names_SouthAsia), "region.wb"] <- "South Asia"
count_top_la_order[count_top_la_order$count %in% toupper(names_SubSahara), "region.wb"] <- "Sahara"

viol_la <- 
  count_top_la_order %>%
  mutate(ordcount = forcats::fct_reorder(count, ncitations)) %>% # Reorder data
  ggplot( aes(x=ordcount, y=ncitations, fill=region.wb)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_manual(name = "Region", values = color_map) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Number of citations in bootstrap sample")


names_top_europe <- all_cit[which(all_cit$region == "Europe"), ][1:10, "count"]
count_top_europe <- long_cit_eu[which(is.na(match(long_cit_eu$count, names_top_europe)) != TRUE), ]
count_top_europe_order <- count_top_europe[order(count_top_europe$ncitations, decreasing = T), ]
count_top_europe_order$region.wb <- NA
count_top_europe_order[count_top_europe_order$count %in% toupper(names_LA), "region.wb"] <- "Latin America"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_EastAsia), "region.wb"] <- "East Asia"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_EU), "region.wb"] <- "Europe"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_MidleEastAfrica), "region.wb"] <- "Middle Africa"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_NA), "region.wb"] <- "USA and Canada"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_SouthAsia), "region.wb"] <- "South Asia"
count_top_europe_order[count_top_europe_order$count %in% toupper(names_SubSahara), "region.wb"] <- "Sahara"

viol_europe <- 
  count_top_europe_order %>%
  mutate(ordcount = forcats::fct_reorder(count, ncitations)) %>% # Reorder data
  ggplot( aes(x=ordcount, y=ncitations, fill=region.wb)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_manual(name = "Region", values = color_map) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Number of citations in bootstrap sample")

names_top_sahara <- all_cit[which(all_cit$region == "Sahara"), ][1:10, "count"]
count_top_sahara <- long_cit_sahara[which(is.na(match(long_cit_sahara$count, names_top_sahara)) != TRUE), ]
count_top_sahara_order <- count_top_sahara[order(count_top_sahara$ncitations, decreasing = T), ]
count_top_sahara_order$region.wb <- NA
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_LA), "region.wb"] <- "Latin America"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_EastAsia), "region.wb"] <- "East Asia"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_EU), "region.wb"] <- "Europe"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_MidleEastAfrica), "region.wb"] <- "Middle Africa"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_NA), "region.wb"] <- "USA and Canada"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_SouthAsia), "region.wb"] <- "South Asia"
count_top_sahara_order[count_top_sahara_order$count %in% toupper(names_SubSahara), "region.wb"] <- "Sahara"

viol_sahara <- 
  count_top_sahara_order %>%
  mutate(ordcount = forcats::fct_reorder(count, ncitations)) %>% # Reorder data
  ggplot( aes(x=ordcount, y=ncitations, fill=region.wb)) +
  geom_violin(width=2.1, size=0.2) +
  scale_fill_manual(name = "Region", values = color_map) +
  theme_ipsum() +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Number of citations in bootstrap sample")


# Joining all violins
legend <- get_legend(viol_la + theme(legend.position = "bottom")) 
all_violin <- 
  plot_grid(viol_la, viol_usacan, viol_eastasia, viol_europe, viol_sahara, ncol = 2, labels = "AUTO")
plot_violin <- plot_grid(all_violin, legend, ncol = 1, rel_heights = c(2, 0.5))
