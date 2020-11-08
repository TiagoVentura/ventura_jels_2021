#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Analysis - Information gains

# Description: This codes replicates the analysis in section 7 (information mechanism) in 
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# Basics ------------------------------------------------------------------
rm(list = ls())
library(rdrobust)
library(readstata13)
library(foreign)
library(Hmisc)
library(lattice)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(pipeR)
library(magrittr)
library(readxl)
library(extrafont)
library(here)

# functions
source(here("code", "functions.R"))



# Setting options
options(width=150)


# Models Figure 6 --------------------------------------------------

load(here("data","effective_number_candidates.Rdata"))
load("data_complete_methods_paper.Rdata")

# Rename two variables

rename_electoral <- function(data){
  data %>% ungroup() %>%
    rename("SIGLA_UE"="CODIGO_MUNICIPIO", 
           "DESCRICAO_UE"="NOME_MUNICIPIO") 
}


enp_bind <- rename_electoral(enp_bind) %>% 
  mutate(ANO_ELEICAO=ANO_ELEICAO-2, 
         enp=ratio_enp_party_list, 
         enp_nolog = no_log_ratio_enp_party_list,
         group="Parties Within List") %>%
  select(-NOME_COLIGACAO) %>% distinct()

d_enp <- left_join(d, enp_bind)

# Data Wragling -----------------------------------------------------------

d_enp$treat <- ifelse(d_enp$vote_margin_share <0, "Non Incumbent", "Incumbent")

d_enp_list <- d_enp %>% 
  select(id, ANO_ELEICAO, SIGLA_UE, treat, contains("enp"), 
         contains("eff"), vote_margin_share, rankvoter, NUMERO_PARTIDO) %>% 
  mutate(ratio=enp_cand_within_party/mean_eff_cand_list_by_party) %>% # with no log
  group_by(ANO_ELEICAO, SIGLA_UE) %>%
  mutate(mean_enp_cand_within_list=mean(enp_cand_within_list), 
         id_list=ifelse(mean_enp_cand_within_list==enp_cand_within_list, 1, 0)) %>%
  ungroup()


# Models ------------------------------------------

# ENP party/mean(parties within the list)
y <- df_fake$ratio
r <- 100*df_fake$vote_margin_share

# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# RD Model

res <- sim_rd(y=y, x=100*df_fake$vote_margin_share, 
              vary = band, poly=1)

result <- map_df(res, extract_conv)

d_together <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "Optimal \n Bandwidth", 25, 100)) 


# Within the Same List

df_same_list <- d_enp_list %>% filter(id_list==1)

# With log

# enp party within list
y <- df_same_list$ratio_enp_party_list
r <- 100*df_same_list$vote_margin_share

# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# RD Model

res <- sim_rd(y=y, x=100*df_same_list$vote_margin_share, 
              vary = band, poly=1)

result <- map_df(res, extract_conv)

d_same_list <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "Optimal \n Bandwidth", 25, 100))



results <- bind_rows(d_together, d_same_list) %>%
  mutate(Coalition=rep(c("All","Same PR List"), each=6), 
         h_fac=as.factor(h), 
         h_fac=fct_relevel(h_fac, "100", "25", "10", "5", "1", "Optimal \n Bandwidth"))

pal <- RColorBrewer::brewer.pal(n=5, "RdBu")

ggplot(results, aes(y=tau, x=h_fac, ymax=CIurb,
                    ymin=CIlrb, color=Coalition, shape=Coalition)) +
  geom_pointrange(position=position_dodge(width = .3), size=1.5) +
  ylab("Effects of Winning Local on ENP") +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dotted") +
  scale_shape_manual(values = c(21,24), 
                     name="") +
  scale_color_manual(name="", values=c(pal[5], pal[1])) +
  coord_flip()


ggsave(here("outputs", "figure_6.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


