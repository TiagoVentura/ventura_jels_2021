#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Main Results

# Description: This codes replicates the results for section 5.1 in  
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# This code also reproduces figures 1 and 3 in the appendix. 

# Loading packages

library(rdrobust)
library(readstata13)
library(foreign)
library(Hmisc)
library(lattice)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lme4)
library(arm)
library(BayesTree)
library(conflicted)
library(sparsereg)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# Load data ---------------------------------------------------------------


load(here("data", "data_complete_methods_paper.Rdata"))

df <- d


# Data wrangling ----------------------------------------------------------


df$treat <- ifelse(df$vote_margin_share <0, "Non Incumbent", "Incumbent")

df <- df %>% 
  mutate(treat = ifelse(rankvote==1, 1, 0)) #%>%



# Sparse Lasso Estimation -------------------------------------------------


# To use Lasso here, I need to build a matrix with all the possible interactions 
# I want to model. 

df$id <- paste(df$ANO_ELEICAO, df$SIGLA_UE, df$DESCRICAO_ELEICAO)
df$nrow <- 1:nrow(df)

# Select party number in du

d_party <- df %>% select(NUMERO_PARTIDO) %>%
  mutate(NUMERO_PARTIDO=factor(NUMERO_PARTIDO)) 


d_party <- makeind(d_party) %>% as.tibble() %>% 
  mutate(id=paste(df$id))


# To use Lasso here, I need to build a matrix with all the possible interactions 
# I want to model. 

df$id <- paste(df$ANO_ELEICAO, df$SIGLA_UE, df$DESCRICAO_ELEICAO)
df$nrow <- 1:nrow(df)

# Select party number in du

d_party <- df %>% select(NUMERO_PARTIDO) %>%
  mutate(NUMERO_PARTIDO=factor(NUMERO_PARTIDO)) 

d_party <- makeind(d_party) %>% as.tibble() %>% 
  mutate(id=paste(df$id))

# Start LASSO here --------------------------------------------------------

library(sparsereg)

y <- df$vs_party_fed

# I have all the dummies here, for example inc 0 and 1
# party change 0 and 1. Not sure I shoudl interact all of them

mat.base <- d_party %>% select(-id) %>% as.matrix() # all the parties

dim(mat.base)

# parties*x = slope left side

mat.x <- df$vote_margin_share*mat.base; colnames(mat.x)<- paste(colnames(mat.base), "x") 

dim(mat.x)

# slope right side (x*t*parties)

mat.t.x <- mat.x*df$treat; colnames(mat.t.x)<- paste(colnames(mat.base), "treatx")

dim(mat.t.x)

# parties*treatment (parties*t)

mat.t <- mat.base*df$treat; colnames(mat.t)<- paste(colnames(mat.base), "treat")

dim(mat.t)

# One really important note

var.final <- cbind(mat.base, mat.x, mat.t.x, mat.t)
colnames(var.final)
dim(var.final)

# With bandwidth by party

d_red <- df %>% mutate(n_row=1:nrow(df)) %>% 
  filter(vote_margin_share> -.15, vote_margin_share<.15) %>% 
  select(n_row)

y_red <- y[-d_red$n_row]
var.final_red <- var.final[-d_red$n_row, ]

system.time(mod2 <- sparsereg(y_red, var.final_red, scale.type="none", gibbs=1000, burnin=1000, thin=30))

summary(mod2)

#save(mod2,file="sparse_party_bw.RData")

# Load the model
load(here("data", "sparse_party_bw.RData"))

# Name for the parties

names <- df %>% filter(ANO_ELEICAO==2012) %>% 
  group_by(SIGLA_PARTIDO, NOME_PARTIDO, NUMERO_PARTIDO) %>%
  count() %>% 
  select(-n)

# Extract values from sparse object ---------------------------------------
class(mod2)

# Graph -------------------------------------------------------------------

# Model by party

res <- extract_sparse(mod2, names)

ggplot(res, aes(y=treat, x=SIGLA_PARTIDO, ymin=lb, ymax=up)) + 
  geom_point(alpha=1, shape=21, size=4, colour="black", fill="tomato2") +
  geom_errorbar(width=.2, colour="black", 
                fill="black", alpha=0.5) + 
  xlab("") + 
  ylab("Subgroup Partisan Treatment Effects") +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dotted") +
  ylim(0, .1)

ggsave(here("outputs", "figure_3.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Appendix Local Regressors -----------------------------------------------


party <- unique(df$NUMERO_PARTIDO)


rdr_s <- possibly(function(x) rdrobust(y=x$vs_party_fed, x=100*x$vote_margin_share, # this is awesome
                                       c=0, p=1, all=T), NA_real_ )

extract_safe <- possibly(function(x) extract_conv(x), NA_real_)



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
View(m)


d_label <- df %>%
  select(party=NUMERO_PARTIDO, name=SIGLA_PARTIDO) %>%
  distinct()

df_local <- m %>% dplyr::select(id, N, tau, CIlrb, CIurb, h) %>%
              mutate(party=party) %>%
        left_join(.,d_label) %>% 
          filter(!is.na(tau)) %>%
          mutate(id= ifelse(CIlrb > 0, "1","2")) %>%
          arrange(id) %>%
    mutate_if(is.numeric, round, digits=3) %>%
    arrange(tau) %>%
    mutate(party_fct=as.factor(party),
            party_fct=fct_inorder(party_fct), 
            party_rev=fct_rev(name))%>%
  filter(!is.na(name), !(name %in%c("PFL", "PL", "PPB"))) 

# Graphing
# Removing prona to make it the graph better
df_local <- df_local %>% filter(!name=="PRONA")
labels <- df_local$name

ggplot(df_local, aes(y=tau, x=party_fct , fill=id)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5) + xlab("") + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=10)) +
  ylab("Reverse Coattails on House Elections") +
  geom_hline(aes(yintercept = 0.0), color = 'lightcoral') +
  coord_flip() +
  scale_x_discrete(label=labels) +
  scale_fill_manual(values=c(`2`="tomato2", `1`="blue")) +
  guides(fill="none")

ggsave(here("outputs", "app_figure_3.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Table

result <- df_local %>% select(name, everything(), -party, -id, -party_rev, -party_fct) %>% arrange(desc(tau))

colnms  = c("Party", "Number of Cases", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Bandwidth")

table<-Hmisc::latex(result, file=here("outputs", "app_table_6.tex"), 
                    title="",
                    table.env=FALSE,
                    center="none",
                    col.just = c("l",rep("c",ncol(result)-1)),
                    colheads=colnms,
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity for Party Effects using Local Estimators}", 
                    n.cgroup=6, 
                    cgroup = "\\bfseries{\\textsc{Outcome: Vote Share Co-partisans for the House Election}}")




# Presidential Support ----------------------------------------------------
p_2012<- c(13, 45, 40)
p_2008<- c(13, 45, 43)
p_2004<- c(13, 45)
p_2000<- c(13, 45, 23, 40)

d_pres_2000 <- map(p_2000, ~ df %>% filter(ANO_ELEICAO==2000) %>%
                    filter(rankvoter==1 & NUMERO_PARTIDO==.x) %>% 
                    mutate(id_party=1) %>% 
                    dplyr::select(id, id_party) %>% 
                    left_join(., df)) %>% 
                    map(., rdr_s) %>% 
                    map(., extract_safe) %>% 
                    bind_rows() %>% 
                    mutate(party=p_2000, year=2002)
                  

d_pres_2004 <- map(p_2004, ~ df %>% filter(ANO_ELEICAO==2004) %>%
                    filter(rankvoter==1 & NUMERO_PARTIDO==.x) %>% 
                    mutate(id_party=1) %>% 
                    dplyr::select(id, id_party) %>% 
                    left_join(., df)) %>%
                     map(., rdr_s) %>% 
                    map(., extract_safe) %>% 
                    bind_rows() %>% 
                    mutate(party=p_2004, year=2006) 



d_pres_2008 <- map(p_2008, ~ df %>% filter(ANO_ELEICAO==2008) %>%
                    filter(rankvoter==1 & NUMERO_PARTIDO==.x) %>% 
                    mutate(id_party=1) %>% 
                    dplyr::select(id, id_party) %>% 
                    left_join(., df)) %>% 
                    map(., rdr_s) %>% 
                    map(., extract_safe) %>% 
                    bind_rows() %>% 
                    mutate(party=p_2008, year=2010) 

d_pres_2012<- map(p_2012, ~ df %>% filter(ANO_ELEICAO==2012) %>%
                    filter(rankvoter==1 & NUMERO_PARTIDO==.x) %>% 
                    mutate(id_party=1) %>% 
                    dplyr::select(id, id_party) %>% 
                    left_join(., df)) %>%
                    map(., rdr_s) %>% 
                    map(., extract_safe) %>% 
                    bind_rows() %>% 
                   mutate(party=p_2012, year=2014)



# To merge names
d_label$party <- as.numeric(d_label$party)


dlocal_top <-bind_rows(d_pres_2000, d_pres_2004, d_pres_2008, d_pres_2012) %>%
              left_join(d_label) %>% filter(!is.na(name))


# Graphing

ggplot(dlocal_top, aes(y=tau, x=name)) + 
  geom_point(alpha=1, shape=21, size=2, colour="black", fill="black") +
  geom_errorbar(aes(ymax=CIurb, ymin= CIlrb), colour="black", 
                fill="black", alpha=0.5) + 
  ylim(-0.5, 0.4) + xlab("") + 
  theme(plot.title = element_text(size = 14, face = "bold"),axis.title=element_text(size=10)) +
  ylab("Reverse Coattails on House Elections") +
  geom_hline(aes(yintercept = 0.0), color = 'lightcoral') +
  coord_flip() +
  scale_fill_manual(values=c(`2`="tomato2", `1`="blue")) +
  facet_wrap(~ year) 

ggsave(here("outputs", "app_figure_1.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

