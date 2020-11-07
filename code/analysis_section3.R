#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Analysis - Descriptive Section 3

# Description: This codes replicates the analysis in section 3 in 
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# Packages ------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(electoral)
library(ggridges)
library(extrafont)
library(here)

# Getting utils
source(here("code", "functions.R"))

# Open the datasets
load(here("data","vs_fed_march2019.Rdata"))

# Effective Number of Electoral Partiesfor the National Lower Chamber by Municipality ---------------------------------
eff_mun_house <- house_vs_bind %>% 
  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO) %>% 
  summarise(effect = electoral::enp(vs_party)) %>% 
  mutate(year=factor(ANO_ELEICAO, 
                     levels = sort(unique(house_vs_bind$ANO_ELEICAO), decreasing = TRUE))) %>% 
  ungroup()


# Figure 1a

ggplot(eff_mun_house, aes(x = effect, y= year, fill=year, group=year, 
                          vline_color = ..quantile..)) + 
  geom_density_ridges(quantile_lines = TRUE,
                      quantiles=2, 
                      alpha = 0.5) +
  theme(legend.position = "left") +
  ylab("") + xlab("ENP in the House Aggregated at the Municipal Level") +
  scale_fill_discrete(name="Electoral Year", 
                      guide = guide_legend(reverse = TRUE)) + # really cleaver
  scale_discrete_manual("vline_color",
                        values = c("blue", "blue"), 
                        breaks = c(1, 2),
                        name = NULL, 
                        guide="none")

ggsave(here("outputs", "figure_1a.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


mean_mun_house <- house_vs_bind %>% filter(party_abs>0) %>% 
  group_by(ANO_ELEICAO, CODIGO_MUNICIPIO) %>% 
  summarise(mean_vs = mean(vs_party, na.rm = TRUE)) %>% 
  mutate(year=factor(ANO_ELEICAO, 
                     levels = sort(unique(house_vs_bind$ANO_ELEICAO), decreasing = TRUE))) %>% 
  ungroup()


# figure 1b

ggplot(mean_mun_house, aes(x = mean_vs, y= year, fill=year, group=year, 
                           vline_color = ..quantile..)) + 
  geom_density_ridges(quantile_lines = TRUE,
                      quantiles=2, 
                      alpha = 0.5) +
  theme(legend.position = "left") +
  ylab("") + 
  xlab("Average Vote Share of Parties for the House at the Municipal Level") +
  scale_fill_discrete(name="Electoral Year", 
                      guide = guide_legend(reverse = TRUE)) + # really cleaver
  scale_discrete_manual("vline_color",
                        values = c("blue", "blue"), 
                        breaks = c(1, 2),
                        name = NULL, 
                        guide="none")


ggsave(here("outputs", "figure_1b.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


