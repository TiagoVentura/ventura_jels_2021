#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Main Results

# Description: This codes replicates the results from section 6 (pork mechanism) in  
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# Basics ------------------------------------------------------------------

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
library(sparsereg)
library(rebus)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# functions

source(here("code", "functions.R"))

# Setting options
options(width=150)

# Open the data

# Data Wragling -----------------------------------------------------------
load(here("data", "data_pork.Rdata"))

d<- d_merge
d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")
df <- d
df <- df %>%
  mutate_at(vars(total_send, total_pay),~ replace(.,is.na(.), 0))

df <- df %>% 
  mutate(bin_total_send=ifelse(total_send > 0, 1, 0))

# Modeling -------------------------------------------------

y <- ifelse(df$total_send>0, 1, 0)
r <- 100*df$vote_margin_share
rdplot(y, r)

# Bandwidth
band <- list(1,5,10, "opt", 25, 100)

# Model
res <- sim_rd(y=y, x=100*df$vote_margin_share, 
              vary = band, poly=1)

result <- map_df(res, extract)

d_fig <- result %>% 
          select(h, tau, CIlrb, CIurb,  N) %>%
          arrange(., h) %>%
          mutate(N=as.character(N),
                 h=as.character(round(h, digits = 0)),
                Method=ifelse(h=="18", "MSE Optimal", "Ad-hoc decision"))


#treatment effect in porcentage terms
pal <- RColorBrewer::brewer.pal(n=9, "RdBu")

ggplot(d_fig, aes(y=tau, x=h, ymax=CIurb, fill=Method, ymin=CIlrb, group=h)) +
  geom_point(size=4, shape=21) + geom_errorbar(width=.2) + 
  scale_color_discrete(name="Bandwidth") + 
  ylab("Probability of Allocation of Pork") +
  scale_fill_manual(values=c(pal[1], pal[9]), 
                    name="Bandwidth") +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dotted") +
  scale_x_discrete(limits=d_fig$h) + 
  theme_tiago +
  theme(legend.position = "bottom",   
        legend.text = element_text(size=22))

ggsave(here("outputs", "figure_4a.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Sparse LASSO ------------------------------------------------------------
library(sparsereg)
library(conflicted)
conflict_prefer("select", "dplyr")

### Run the ModelÇ Might take  few hours  ------------------------------------------------------------

# To use Lasso here, I need to build a matrix with all the possible interactions 
# I want to model. 

df$id <- paste(df$ANO_ELEICAO, df$SIGLA_UE, df$DESCRICAO_ELEICAO)
df$nrow <- 1:nrow(df)

# Select party number in du

d_party <- df %>% select(NUMERO_PARTIDO) %>%
  mutate(NUMERO_PARTIDO=factor(NUMERO_PARTIDO)) 


d_party <- BayesTree::makeind(d_party) %>% as.tibble() %>% 
  mutate(id=paste(df$id))


y <- df$bin_total_send

# I have all the dummies here, for example inc 0 and 1
# party change 0 and 1. Not sure I shoudl interact all of them

mat.base <- d_party %>% select(-id) %>% as.matrix() # all the parties

dim(mat.base)

# parties*x = slope left side

mat.x <- df$vote_margin_share*mat.base; colnames(mat.x)<- paste(colnames(mat.base), "x") 

dim(mat.x)

# slope right side (x*t*parties)
df <- df %>% 
  mutate(treat = ifelse(rankvote==1, 1, 0)) #%>%

mat.t.x <- mat.x*df$treat; colnames(mat.t.x)<- paste(colnames(mat.base), "treatx")

dim(mat.t.x)

# parties*treatment (parties*t)

mat.t <- mat.base*df$treat; colnames(mat.t)<- paste(colnames(mat.base), "treat")

dim(mat.t)

# One really important note

# since I have all the parties, I should not add the treat + x
# this was giving me some really weird results. 
# with only the matrix of parties already with the interactions, 
# the results go exactly in the direction I had in the other models. 
# i show in the end that it works using only the lm


var.final <- cbind(mat.base, mat.x, mat.t.x, mat.t)
colnames(var.final)
dim(var.final)

# With bandwidth by party
d_red <- df %>% mutate(n_row=1:nrow(df)) %>% 
  filter(vote_margin_share> -.180, vote_margin_share<.180) %>% 
  select(n_row)

y_red <- y[-d_red$n_row]
var.final_red <- var.final[-d_red$n_row, ]

system.time(mod2 <- sparsereg(y_red, var.final_red, scale.type="none", gibbs=1000, burnin=1000, thin=30))

summary(mod2)

#save(mod2,file="sparse_party_bw_emendas.RData")

## Load Model

load(here("data","sparse_party_bw_emendas.RData"))
names <- df %>% filter(ANO_ELEICAO==2012) %>% 
  group_by(SIGLA_PARTIDO, NOME_PARTIDO, NUMERO_PARTIDO) %>%
  count() %>% 
  select(-n)

# Extract quantitites
res <- extract_sparse(mod2, names)

ggplot(res, aes(y=treat, x=SIGLA_PARTIDO, ymin=lb, ymax=up)) + 
  geom_point(alpha=1, shape=21, size=4, colour="black", fill="tomato2") +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5) + 
  xlab("") + 
  ylab(" Conditional Access to Pork by Parties") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_tiago

ggsave(here("outputs", "figure_4b.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Model with Local Regressors (appendix figure 4) ----------------------------------------------

party <- unique(df$NUMERO_PARTIDO)
rdr_s <- possibly(function(x) rdrobust(y=x$bin_total_send, x=100*x$vote_margin_share), NA_real_ )
extract_safe <- possibly(function(x) extract_rd(x), NA_real_)



# Splitting the data by party. Tricky here because I should select only cases of incumbents from the party
d_party <- map(party, ~ df %>% 
                 filter(rankvoter==1 & NUMERO_PARTIDO==.x) %>% 
                 mutate(id_party=1) %>% 
                 dplyr::select(id, id_party) %>% 
                 left_join(., df))


d_party_res <-  map(d_party, rdr_s) 

m<- data.frame()

# Looping trhought the models
for(i in 1:33){
  out <- data.frame(extract_safe(d_party_res[[i]])) %>% mutate(id=i)
  m <- plyr::rbind.fill(m, out)
  print(i)
}

# Cleaning
table(df$ANO_ELEICAO)
d_label <- df %>%
  select(party=NUMERO_PARTIDO, name=SIGLA_PARTIDO) %>%
  distinct()

df_local


df_local <- m %>% dplyr::select(id, N, tau, CIlrb, CIurb, h) %>%
  mutate(party=party) %>%
  left_join(.,d_label) %>% 
  filter(!is.na(tau)) %>%
  mutate(id= ifelse(CIlrb > 0, "1","2")) %>%
  arrange(id) %>%
  mutate_if(is.numeric, round, digits=3) %>%
  arrange(desc(id), tau) %>%
  mutate(party_fct=as.factor(party),
         party_fct=fct_inorder(party_fct), 
         party_rev=fct_inorder(name)) %>%
  filter(!is.na(name), !(name %in%c("PFL", "PL", "PPB"))) %>% 
  filter(tau<10)

ggplot(df_local, aes(y=tau, x=party_fct,  fill=id)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5) + xlab("") + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_text(size=10)) +
  ylab("Partisan Effect on Acess to Pork") +
  geom_hline(aes(yintercept = 0.0), color = 'lightcoral') +
  coord_flip() +
  scale_x_discrete(labels=df_local$party_rev) +
  scale_fill_manual(values=c(`2`="tomato2", `1`="blue")) +
  guides(fill="none")

ggsave(here("outputs", "app_figure_4.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Modeling the effect of pork on boosting ---------------------------------

# Definining the variables

d_pork_split <- df %>% 
  mutate(total_send_bin = ifelse(total_send>0, 1, 0)) %>%
  group_by(id) %>%
  mutate(id_pork=bin_total_send, 
         id_pork_both=mean(bin_total_send)) %>%
  filter(rankvote==1, id_pork==1) %>%
  select(id, id_pork, id_pork_both) %>% ungroup()


df_join <- left_join(df, d_pork_split) %>% 
  mutate(id_pork=ifelse(is.na(id_pork), "Incumbent (No Pork)", "Incumbent (Pork)"), 
         id_pork_both_new=ifelse(id_pork_both==1, "Incumbent (Pork) x Runner Up (Pork)",
                                 "Control Group"),
         id_pork_both_new=ifelse(is.na(id_pork_both_new), "Control Group", 
                                 id_pork_both_new), 
         id_pork_inc_rp_no=ifelse(treat=="Non Incumbent" & bin_total_send==1 &
                                    id_pork_both_new=="Control Group", 
                                  1, NA),
         id_pork_complete=id_pork_both_new) 



df_lastconditon <- df_join  %>%
  filter(id_pork_inc_rp_no==1) %>%
  select(id, id_pork_inc_rp_no) %>% ungroup()

df_join <- left_join(df_join, df_lastconditon, by="id") %>%
  mutate(id_pork_inc_rp_no=ifelse(is.na(id_pork_inc_rp_no.y), 
                                  0,1),
         id_pork_complete=ifelse(id_pork_inc_rp_no==1,
                                 "Incumbent (No Pork) x Runner Up (Pork)", 
                                 id_pork_complete))

# Estimate models : Incumbent Receives Pork vs Incumbent does not receives pork

results_pork_id <- df_join %>%
  group_by(id_pork) %>%
  nest() %>% 
  mutate(model= map(data, ~ rdrobust(.x$vs_party_fed, 100*.x$vote_margin_share)), 
         coef=map(model, extract)) %>%
  unnest(coef) %>% 
  select(id_pork, h, tau, CIlrb, CIurb,  N) 


# Estimate models : All combinations

results_pork_id_complete <- df_join %>%
  group_by(id_pork_complete) %>%
  nest() %>% 
  filter(id_pork_complete!="Control Group") %>%
  mutate(model= map(data, ~ rdrobust(.x$vs_party_fed, 100*.x$vote_margin_share)), 
         coef=map(model, extract)) %>%
  unnest(coef) %>% 
  select(id_pork=id_pork_complete, h, tau, CIlrb, CIurb,  N) 


results <- bind_rows(results_pork_id, results_pork_id_complete) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits=2) %>%
  mutate(id_pork=as.factor(id_pork), 
         id_pork_new=fct_recode(id_pork,
                                "Incumbent (No Pork) x \n Runner Up (Pork)" ="Incumbent (No Pork) x Runner Up (Pork)", 
                                "Incumbent (Pork) x \n Runner Up (Pork)"="Incumbent (Pork) x Runner Up (Pork)", 
                                "Incumbent (No Pork)"="Incumbent (No Pork)", 
                                "Incumbent (Pork)"="Incumbent (Pork)"), 
         id_pork_new= fct_relevel(id_pork_new, 
                                  "Incumbent (No Pork) x \n Runner Up (Pork)" , 
                                  "Incumbent (Pork) x \n Runner Up (Pork)", 
                                  "Incumbent (No Pork)", 
                                  "Incumbent (Pork)"))


levels(results$id_pork_new)

# Bandwidth

pal <- RColorBrewer::brewer.pal(n=9, "Spectral")

ggplot(results, 
       aes(y=tau, x=id_pork_new, ymax=CIurb, fill=id_pork, ymin=CIlrb, group=h)) +
  geom_point(size=4, shape=21, fill="red") + geom_errorbar(width=.2) + 
  scale_color_discrete(name="Bandwidth") + 
  ylab("Political Boosting on the Vote Share for the House Elections") +
  scale_fill_manual(values=c(pal[2], pal[3], pal[5], pal[9]), 
                    name="Bandwidth", guide=FALSE) +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dotted") +
  theme(axis.text.y = element_text(size=24)) +
  coord_flip()

ggsave(here("outputs", "figure_5.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

