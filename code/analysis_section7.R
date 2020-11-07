#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Analysis - Information gains

# Description: This codes replicates the analysis in section 7 in 
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

# ggplot theme ------------------------------------------------------------

my_font <- "Palatino Linotype"
my_bkgd <- "white"

my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(fill = my_bkgd),
                  legend.key.size = unit(1.5, "cm"),
                  legend.text = element_text(size = 18, family = my_font),
                  legend.title = element_text(size=14),
                  plot.title = element_text(size = 22, face = "bold"),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=18),
                  axis.title.x = element_text(hjust=1),
                  strip.background = element_blank(),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 16, face="italic"))
theme_set(theme_light() + my_theme)


# functions
getwd()
#source("C:/Users/Tiago Ventura/Dropbox/artigos/Ventura Partisan Effects Rd/code/functions.r")
source("C:/Users/arodrigu/Documents/Dropbox/artigos/Ventura Partisan Effects Rd/code/functions.r")

# Setting options
options(width=150)

# Open the data

#path_data <- "C:/Users/Tiago Ventura/Dropbox/Datasets/electoral_data/data"
#output_old <- "C:/Users/Tiago Ventura/Dropbox/artigos/Ventura Partisan Effects Rd/output/"
#output <- "C:/Users/Tiago Ventura/Dropbox/Apps/Overleaf/political_boosting_electoral_studies/output/"

path_data <- "C:/Users/arodrigu/Documents/Dropbox/Datasets/electoral_data/data"
output_old <- "C:/Users/arodrigu/Documents/Dropbox/artigos/Ventura Partisan Effects Rd/output/"
output <- "C:/Users/arodrigu/Documents/Dropbox/Apps/Overleaf/political_boosting_electoral_studies/output/"

setwd(path_data)
list.files()

# load the data
load("enp_candidates_parties.Rdata")
load("enp_candidates_list.Rdata")
load("enp_parties_list.Rdata")
load("enp_bind.Rdata")
# Get the rdd data --------------------------------------------------------

load("data_complete_methods_paper.Rdata")

d$NUMERO_PARTIDO <- as.character(d$NUMERO_PARTIDO)

# Rename two variables

rename_electoral <- function(data){
  data %>% 
    rename("SIGLA_UE"="CODIGO_MUNICIPIO", 
           "DESCRICAO_UE"="NOME_MUNICIPIO") 
}

# Recode ano for the merge

#Rename for merge



house_enp_candidates <- rename_electoral(house_enp_candidates) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO-2, 
         enp=enp_cand_within_party, 
         group="Candidates Within Parties") 
house_enp_candidates_list <- rename_electoral(house_enp_candidates_list) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO-2, 
         enp=enp_cand_within_list, 
         group="Candidates Within List")  %>% 
  select(-NOME_COLIGACAO) %>% distinct() 

house_enp_parties_list <- rename_electoral(house_enp_parties_list) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO-2, 
         enp=enp_party_within_list, 
         group="Parties Within List") %>%
  select(-NOME_COLIGACAO) %>% distinct()


range(enp_bind$ratio_enp_party_list, na.rm = TRUE)

# just some descriptive

house <- list(house_enp_candidates, house_enp_candidates_list, house_enp_parties_list)

house <- map(house, ~ .x %>% select(enp, group) %>%
               filter(!enp==1.00)) %>% bind_rows()


# Density

ggplot(house, aes(x=enp, fill=group, group=group, color=group)) +
  geom_density(alpha=.5) + 
  xlim(0,10) + theme_minimal()


# Merge
d <- left_join(d, house_enp_candidates)
d <- left_join(d, house_enp_candidates_list,  
               by = c("ANO_ELEICAO", "SIGLA_UE", 
                      "DESCRICAO_UE", "NUMERO_PARTIDO")) # make sure it works properly
d <- left_join(d, house_enp_parties_list,  
               by = c("ANO_ELEICAO", "SIGLA_UE",
                      "DESCRICAO_UE", "NUMERO_PARTIDO")) # make sure it works properly


## Need to understand the NA's here
d %>% filter(is.na(enp_cand_within_party))  %>% 
  select(ANO_ELEICAO, SIGLA_UE, NUMERO_PARTIDO, DESCRICAO_UE, 
         vs_party_fed, enp_cand_within_party) 

# zero vote share for most of the cases: therefore, just left there it will be excluded

# Let me try onw thing in the data: add extreme values for those with no vote

d <- d %>%
  mutate_at(vars(contains("enp")), list(extreme=~ifelse(is.na(.x), 5, .x)))



# Data Wragling -----------------------------------------------------------

d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")


# Effective Number of Candidates ------------------------------------------


#df_fake <- d %>% filter(!enp_party_within_list==1, ANO_ELEICAO==2008)
df_fake <- d



# With log

# enp party within list
y <- log(df_fake$enp_party_within_list)
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# enp cand within list
y <- log(df_fake$enp_cand_within_list)
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))
rdplot(y,r)

# enp cand within party
y <- log(df_fake$enp_cand_within_party)
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# Pure data

# enp party within list
y <- df_fake$enp_party_within_list
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))

# enp cand within list
y <- df_fake$enp_cand_within_list
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))
rdplot(y,r)

# enp cand within party
y <- df_fake$enp_cand_within_party
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))

df_enp=df_fake

# Models with the ratio --------------------------------------------------
load("enp_bind.Rdata")


load("data_complete_methods_paper.Rdata")
d$NUMERO_PARTIDO <- as.character(d$NUMERO_PARTIDO)

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

## if the mean of enp_cand_within_list == enp_cand_within_list it means both the incumbent 
## and the runner up are running on the same list


# Effective Number of Candidates ------------------------------------------


#df_fake <- d %>% filter(!enp_party_within_list==1, ANO_ELEICAO==2008)
df_fake <- d_enp_list


plot(density(df_fake$ratio, na.rm = TRUE))



# With log

# enp party within list
y <- df_fake$ratio
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# ENP party/mean(parties within the list)
y <- df_fake$ratio
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# RD Model

res <- sim_rd(y=y, x=100*df_fake$vote_margin_share, 
              vary = band)

result <- map_df(res, extract_conv)

d_together <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "Optimal \n Bandwidth", 25, 100)) 


# Same list

df_same_list <- d_enp_list %>% filter(id_list==1)

# With log

# enp party within list
y <- df_same_list$ratio_enp_party_list
r <- 100*df_same_list$vote_margin_share

# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# RD Model

res <- sim_rd(y=y, x=100*df_same_list$vote_margin_share, 
              vary = band)

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


ggsave(paste0(output,"information.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# By Party

rdr_s <- possibly(function(x) rdrobust(y=x$ratio_enp_party_list, x=100*x$vote_margin_share), NA_real_ )
extract_safe <- possibly(function(x) extract_conv(x), NA_real_)



# Splitting the data by party. Tricky here because I should select only cases of incumbents from the party

party <- unique(df_same_list$NUMERO_PARTIDO)

d_party <- map(party, ~ df_same_list %>% 
                 filter(NUMERO_PARTIDO==.x))

d_party_res <-  map(d_party, rdr_s) 

m<- data.frame()

# Looping trhought the models
for(i in 1:length(party)){
  out <- data.frame(extract_safe(d_party_res[[i]])) %>% mutate(id=i)
  m <- plyr::rbind.fill(m, out)
  print(i)
}

# Cleaning

df_local <- m %>% dplyr::select(id, N, tau, CIlrb, CIurb, h) %>%
  mutate(party=party) %>%
  mutate(id= ifelse(CIlrb > 0, "1","2")) %>%
  arrange(id) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  arrange(desc(id), tau) %>%
  mutate(party_fct=as.factor(party),
         party_fct=fct_inorder(party_fct)) %>% drop_na()

ggplot(df_local, aes(y=tau, x=party_fct,  fill=id)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5) + xlab("") + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=10)) +
  ylab("Partisan Effect on Acess to Pork") +
  geom_hline(aes(yintercept = 0.0), color = 'lightcoral') +
  coord_flip() +
  scale_fill_manual(values=c(`2`="tomato2", `1`="blue")) +
  theme_tiago + 
  guides(fill="none")


s <- df_fake %>% filter(NUMERO_PARTIDO=="65")
summary(rdrobust(y=s$ratio_enp_party_list, x=100*s$vote_margin_share))

# Estimate the models with the difference ---------------------------------

# load the data
load("diff_enp_candidates_parties.Rdata")
load("diff_enp_candidates_list.Rdata")
load("diff_enp_parties_list.Rdata")

# Get the rdd data --------------------------------------------------------

load("data_complete_methods_paper.Rdata")
d$NUMERO_PARTIDO <- as.character(d$NUMERO_PARTIDO)

table(diff_enp_cand_within_list$ANO_ELEICAO)
# Rename two variables

rename_electoral <- function(data){
  data %>% 
    rename("SIGLA_UE"="CODIGO_MUNICIPIO", 
           "DESCRICAO_UE"="NOME_MUNICIPIO") 
}

# Recode ano for the merge

#Rename for merge

ls()

diff_enp_cand_within_party <- rename_electoral(
  diff_enp_cand_within_party ) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO+2, 
         enp=diff_enp_cand_within_party, 
         group="Candidates Within Parties") 

diff_enp_cand_within_list <- rename_electoral(diff_enp_cand_within_list) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO+2, 
         enp=diff_enp_cand_within_list, 
         group="Candidates Within List")  %>% 
  distinct() 

diff_enp_party_within_list <- rename_electoral(diff_enp_party_within_list) %>%
  mutate(ANO_ELEICAO=ANO_ELEICAO+2, 
         enp=diff_enp_party_within_list, 
         group="Parties Within List") %>% distinct()

range(diff_enp_cand_within_list$diff_enp_cand_within_list)
range(diff_enp_cand_within_party$diff_enp_cand_within_party)
range(diff_enp_party_within_list$diff_enp_party_within_list)
sum(is.na(diff_enp_cand_within_list$diff_enp_cand_within_list))
sum(is.na(diff_enp_cand_within_party$diff_enp_cand_within_party))
sum(is.na(diff_enp_party_within_list$diff_enp_party_within_list))



# Merge
d <- left_join(d, diff_enp_cand_within_party)

d <- left_join(d, diff_enp_cand_within_list,  
               by = c("ANO_ELEICAO", "SIGLA_UE", 
                      "DESCRICAO_UE", "NUMERO_PARTIDO")) # make sure it works properly
d <- left_join(d, diff_enp_party_within_list,  
               by = c("ANO_ELEICAO", "SIGLA_UE",
                      "DESCRICAO_UE", "NUMERO_PARTIDO")) # make sure it works properly



## Need to understand the NA's here
d %>% filter(is.na(diff_enp_cand_within_party))  %>% 
  select(ANO_ELEICAO, SIGLA_UE, NUMERO_PARTIDO, DESCRICAO_UE, 
         vs_party_fed, diff_enp_cand_within_party) 

# zero vote share for most of the cases: therefore, just left there it will be excluded

# Data Wragling -----------------------------------------------------------

d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")


# Modeling: Amount Proposed -----------------------------------------------
#df_fake <- d %>% filter(!enp_party_within_list==1, ANO_ELEICAO==2008)
df_fake <- d


# With log
# enp party within list
y <- df_fake$diff_enp_cand_within_party
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# enp cand within list
y <- df_fake$diff_enp_cand_within_list
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))


# enp cand within party
y <- df_fake$diff_enp_party_within_list
r <- 100*df_fake$vote_margin_share
summary(rdrobust(y, r))




# Model Votos Legenda -----------------------------------------------------

load("data_complete_methods_paper.Rdata")
load("votes_legenda.Rdata")
load("diff_votes_legenda.Rdata")

d <- d %>% 
  mutate(NUMERO_PARTIDO=as.character(NUMERO_PARTIDO)) %>%
  rename("NOME_MUNICIPIO"="DESCRICAO_UE") %>% 
  mutate(NOME_MUNICIPIO=iconv(NOME_MUNICIPIO, 
                              from="UTF-8", to="ASCII//TRANSLIT"))

# I have to merge by year. Some weird thing with the electionsbr

list_leg <- map(list_leg, ~ .x %>% ungroup() %>%
                  mutate(ANO_ELEICAO=ANO_ELEICAO-2, 
                         NUMERO_PARTIDO=as.character(NUMERO_PARTIDO)))

d_list <- d %>% split(.$ANO_ELEICAO)

# Merge 2002

d_list[[1]] <- left_join(d_list[[1]], list_leg[[2]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[1]]$share_legenda)) # ok

d_list[[1]] <- d_list[[1]] %>%
  mutate_at(vars(NOMINAL_VOTOS, LEGENDA_VOTOS, share_legenda), ~ 
              ifelse(is.na(.x), 0, .x))

sum(is.na(d_list[[1]]$share_legenda))

# Merge 2006: a lot of problems here

d_2006_sigla <- left_join(d_list[[2]], list_leg[[3]], 
                          by=c("ANO_ELEICAO", "SIGLA_UE",
                               "NUMERO_PARTIDO")) 


d_2006_nome <- left_join(d_list[[2]], list_leg[[3]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

nrow(d_2006_nome)


# Put them together

d_2006 <- d_2006_nome %>% 
  mutate(share_legenda_name=d_2006_sigla$share_legenda, 
         share_legenda=ifelse(is.na(share_legenda),
                              share_legenda_name, 
                              share_legenda)) %>%
  mutate_at(vars(NOMINAL_VOTOS, LEGENDA_VOTOS, share_legenda), ~ 
              ifelse(is.na(.x), 0, .x))


d_list[[2]] <- d_2006

sum(is.na(d_list[[2]]$share_legenda))


# Merge 2010
d_list[[3]]<- left_join(d_list[[3]], list_leg[[4]], 
                        by=c("ANO_ELEICAO", "SIGLA_UF", 
                             "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[3]]$share_legenda))

d_list[[3]] <- d_list[[3]] %>%
  mutate_at(vars(NOMINAL_VOTOS, LEGENDA_VOTOS, share_legenda), ~ 
              ifelse(is.na(.x), 0, .x))

sum(is.na(d_list[[3]]$share_legenda))



# Merge 2014

d_list[[4]] <- left_join(d_list[[4]], list_leg[[5]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[4]]$share_legenda))

d_list[[4]] <- d_list[[4]] %>%
  mutate_at(vars(NOMINAL_VOTOS, LEGENDA_VOTOS, share_legenda), ~ 
              ifelse(is.na(.x), 0, .x))

sum(is.na(d_list[[4]]$share_legenda))


d <- bind_rows(d_list)


# Run the Model

d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")


extract_conv <- function(mod) {
  out <- data.frame(year="ALL", 
                    party="INC",
                    outcome="Vote_Share_Party", 
                    sample="all", 
                    options="mserd", 
                    h=mod$bws[1],
                    tau=mod$Estimate[1],
                    PVALrb=mod$pv[3], 
                    CIlrb=mod$ci[1],
                    CIurb=mod$ci[4], 
                    error=0, 
                    diffmeans=NA, 
                    diffmeanspval=NA,
                    diffmeanst=NA,
                    diffmeanscil=NA,
                    diffmeansciu=NA,
                    count=1, 
                    N = mod$Nh[[1]]) }



# log legendas voto

y <- d$share_legenda
r <- 100*d$vote_margin_share
summary(rdrobust(y, r))

# vote share legenda
# Bandwidth
band <- list(1,5,10, "opt", 25, 100)
# Model

res <- sim_rd(y=y, x=r, 
              vary = band)

result <- map_df(res, extract)

d_fig <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "optimal bandwidth", 25, 100))


# leganda share

y <-d$share_legenda
r <- 100*d$vote_margin_share

# vote share legenda
# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# Model

res <- sim_rd(y=y, x=r, 
              vary = band)

result <- map_df(res, extract)

d_fig2 <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "optimal bandwidth", 25, 100))


#treatment effect in porcentage terms


ggplot(d_fig2, aes(y=tau, x=h, ymax=CIurb, ymin=CIlrb, group=h)) +
  geom_point(size=3, shape=21, fill="red") + geom_errorbar(width=.2) + 
  scale_color_discrete(name="Bandwidth") + 
  ylab("Causal Effect of Co-partisanship on Access to Pork") +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dotted") +
  scale_x_discrete(limits=d_fig$h) + 
  theme_tiago


# By party
# Get the party names and numbers
load(paste0(output_old,"model_lme4_party_complete.Rdata"))

df <- d
party <- unique(df$NUMERO_PARTIDO)

rdr_s <- possibly(function(x) rdrobust(y=x$share_legenda, x=100*x$vote_margin_share), NA_real_ )
extract_safe <- possibly(function(x) extract_conv(x), NA_real_)



# Splitting the data by party. Tricky here because I should select only cases of incumbents from the party

d_party <- map(party, ~ df %>% 
                 filter(NUMERO_PARTIDO==.x) %>% 
                 mutate(id_party=1) %>% 
                 dplyr::select(id, id_party) %>% 
                 left_join(., df))


d_party_res <-  map(d_party, rdr_s) 

m <- data.frame()

# Looping trhought the models
for(i in 1:33){
  out <- data.frame(extract_safe(d_party_res[[i]])) %>% mutate(id=i)
  m <- plyr::rbind.fill(m, out)
  print(i)
}

# Cleaning

df_local <- m %>% dplyr::select(id, N, tau, CIlrb, CIurb, h) %>%
  mutate(party=party) %>%
  mutate(id= ifelse(CIlrb > 0, "1","2")) %>%
  arrange(id) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  arrange(desc(id), tau) %>%
  mutate(party_fct=as.factor(party),
         party_fct=fct_inorder(party_fct)) %>% drop_na()

ggplot(df_local, aes(y=tau, x=party_fct,  fill=id)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5) + xlab("") + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=10)) +
  ylab("Partisan Effect on Acess to Pork") +
  geom_hline(aes(yintercept = 0.0), color = 'lightcoral') +
  coord_flip() +
  scale_fill_manual(values=c(`2`="tomato2", `1`="blue")) +
  theme_tiago + 
  guides(fill="none")


# Rate of Change in Legendas ----------------------------------------------

load("data_complete_methods_paper.Rdata")
load("votes_legenda.Rdata")
load("diff_votes_legenda.Rdata")

d <- d %>% 
  mutate(NUMERO_PARTIDO=as.character(NUMERO_PARTIDO)) %>%
  rename("NOME_MUNICIPIO"="DESCRICAO_UE") %>% 
  mutate(NOME_MUNICIPIO=iconv(NOME_MUNICIPIO, 
                              from="UTF-8", to="ASCII//TRANSLIT"))

# I have to merge by year. Some weird thing with the electionsbr

list_leg_diff <- map(list_leg_diff, ~ .x %>% ungroup() %>%
                       mutate(ANO_ELEICAO=ANO_ELEICAO+2, 
                              NUMERO_PARTIDO=as.character(NUMERO_PARTIDO)))

d_list <- d %>% split(.$ANO_ELEICAO)

# Merge 2002

d_list[[1]] <- left_join(d_list[[1]], list_leg_diff[[1]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[1]]$diff_share_legenda)) # ok


# Merge 2006: a lot of problems here

d_list[[2]] <- left_join(d_list[[2]], list_leg_diff[[2]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[2]]$diff_share_legenda))


# Merge 2010
d_list[[3]]<- left_join(d_list[[3]], list_leg_diff[[3]], 
                        by=c("ANO_ELEICAO", "SIGLA_UF", 
                             "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[3]]$diff_share_legenda))



# Merge 2014

d_list[[4]] <- left_join(d_list[[4]], list_leg_diff[[4]], 
                         by=c("ANO_ELEICAO", "SIGLA_UF", 
                              "NOME_MUNICIPIO", "NUMERO_PARTIDO")) 

sum(is.na(d_list[[4]]$diff_share_legenda))

d <- d_list %>% bind_rows()

# Effect
y <- d$diff_share_legenda
r <- 100*d$vote_margin_share
summary(rdrobust(y, r))

models_by_year <- d %>% split(.$ANO_ELEICAO) %>%
  map(~rdrobust(.x$diff_share_legenda, 100*.x$vote_margin_share)) 

map(models_by_year, summary)

results <- map(models_by_year, extract) %>% 
  bind_rows() %>% 
  mutate(year=names(models_by_year))



results %>% dplyr::select(N, tau, CIlrb, CIurb, h, year) %>%
  arrange(year) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  ggplot(., aes(y=tau, x=year,  fill=year)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5)
