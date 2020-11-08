#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Main Results

# Description: This codes replicates parts of the appendix of 
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# Robustness Checks
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
library(rddensity)
library(extrafont)
library(here)

# functions
source(here("code", "functions.R"))

# Setting options
options(width=150)

# Open the data
load(here("data", "data_complete_methods_paper.Rdata"))
load(here("data", "party_name.Rdata"))

# Data Wragling -----------------------------------------------------------
d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")
df <- d
y<- df$vs_party_fed
r <- 100*df$vote_margin_share



# Density Plots -----------------------------------------------------------

# function for the density plot

rdd_density_plot <- function(data, x=vote_margin_share, label=""){

data <- data %>% filter(vote_margin_share < .101 & vote_margin_share>-.101) %>% 
  mutate(margin = vote_margin_share)

rdd <- rddensity::rddensity(data$vote_margin_share)

ggplot() +
  geom_histogram(data=data, aes(y=..density.., x=margin),
                 alpha=.2, col="black", fill="steelblue", bins = 100) +
  ylim(0,10) +
  xlab(paste("Margin of Victory", label)) + ylab("Density") +
  geom_vline(xintercept = 0, color="red") +
  ggplot2::annotate("label", label=paste("p-value density test = ", round(rdd$test$p_jk, 3)),
                    x=0.05, 
                    y=9, size=8) 
}


# Global ------------------------------------------------------------------

rdd_density_plot(df)

ggsave(here("outputs", "app_figure5.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# by years

year <- c(2000, 2004, 2008, 2012)

df_year <- map(year, ~ df %>% filter(ANO_ELEICAO==.x))

unique(df_year[[1]]$ANO_ELEICAO) # ok

map2(df_year, year, ~ rdd_density_plot(data=.x, label=.y)) %>% 
  map2(., year, ~ ggsave(plot=.x, here("outputs", paste0(.y, "_density_global.png")), 
                          width = 12, height = 8, units = "in", pointsize = 12, bg = "white"))

# Parties -----------------------------------------------------------------

parties <- c("13", "45", "15", "11", "25"); names<-c("PT", "PSDB", "PMDB", "PP", "DEM")

df_parties <- map(parties, ~ df %>% filter(., NUMERO_PARTIDO==.x)) 

# Graphing and saving              

map2(df_parties, names, ~ rdd_density_plot(data=.x, label=.y)) %>% 
  map2(., names, ~ ggsave(plot=.x, here("outputs", paste0(.y, "_density_global.png")), 
                          width = 12, height = 8, units = "in", pointsize = 12, bg = "white"))


# Subsamples --------------------------------------------------------------


# Gov

df_gov <- df %>% group_by(id) %>%  # here i am summarizing if the
  # mayor in the pair runs, 
  #   the whole pair gets 1 to be subseted
  summarise(inc_gov= sum(gov_same_party)) %>%
  left_join(., df) %>%
  mutate(inc_gov=ifelse(inc_gov==2, 1, inc_gov),
         gov_winner=ifelse(rankvoter==1 & inc_gov==1 & gov_same_party==1 , 1, 0)) %>% 
  group_by(id) %>% 
  summarise(inc_gov_winner= sum(gov_winner)) %>% 
  left_join(., df) %>% 
  group_by(inc_gov_winner) %>%
  nest()

names <- c("Governor Different Party", "Governor Same Party")

map2(df_gov$data, names, ~ rdd_density_plot(data=.x, label=.y)) %>% 
  map2(., names, ~ ggsave(plot=.x, here("outputs", paste0(.y, "_density_global.png")), 
                          width = 12, height = 8, units = "in", pointsize = 12, bg = "white"))


# Representative

df_ndep <- df %>% filter(rankvoter==1) %>% 
  mutate(id_ndep=ifelse(n_dep_house>0, 1,0)) %>% 
  dplyr::select(id, id_ndep) %>% 
  left_join(., df) %>%
  group_by(id_ndep) %>%                             
  nest()

names <- c("Without House Representative", "With House Representative")

map2(df_ndep$data, names, ~ rdd_density_plot(data=.x, label=.y)) %>% 
  map2(., names, ~ ggsave(plot=.x, here("outputs", paste0(.y, "_density_global.png")), 
                          width = 12, height = 8, units = "in", pointsize = 12, bg = "white"))


# Ambition

df_ld <- df %>% group_by(id) %>%  # here i am summarizing if the mayor in the pair runs, 
  #   the whole pair gets 1 to be subseted
  summarise(inc_id= sum(mayor_t0), 
            lame_duck_id = sum(lame_duck_t0)) %>% 
  left_join(., df) %>% 
  mutate(career=ifelse(inc_id==1, 1, 
                       ifelse(lame_duck_id==1, 2, 0)))


df_inc_nested <- df_ld %>% group_by(career) %>% 
  nest()

names <- c("Open Seat", 
           "Reelection", 
           "Lame Duck")


map2(df_inc_nested$data, names, ~ rdd_density_plot(data=.x, label=.y)) %>% 
  map2(., names, ~ ggsave(plot=.x, here("outputs", paste0(.y, "_density_global.png")), 
                          width = 12, height = 8, units = "in", pointsize = 12, bg = "white"))



# Placebo Tests -----------------------------------------------------------

# Party Effect at t$-2$

load("data_placebo_methods_paper.Rdata")

d_placebo$treat <- ifelse(d_placebo$vote_margin_share <0, "Non Incumbent", "Incumbent")
df <- d_placebo

y<- df$vs_party_fed_lag
r <- 100*df$vote_margin_share

rdd <- rdrobust(y, r)


result_global <-extract(rdd) %>% 
  mutate(id="Incumbent")

# By year -----------------------------------------------------------------

df_year <- df %>% 
          group_by(ANO_ELEICAO) %>% 
          nest()  

result_year <- map(1:3, ~ rdrobust(df_year$data[[.x]]$vs_party_fed_lag,
                              100*df_year$data[[.x]]$vote_margin_share)) %>% 
          map_df(., extract) %>% 
          mutate(id=c("2004", "2008", "2012"))




# By parties


parties <- c(13, 45, 15, 11, 25); names<-c("PT", "PSDB", "PMDB", "PP", "DEM")



df_parties <- df %>% 
              filter(NUMERO_PARTIDO %in% parties) %>%
              group_by(NUMERO_PARTIDO) %>% nest()
  

result_party <- map(1:5, ~ rdrobust(df_parties$data[[.x]]$vs_party_fed_lag,
                    100*df_parties$data[[.x]]$vote_margin_share)) %>% 
  map_df(., extract) %>% 
  mutate(id=names) %>% 
  select(id, everything())

View(result_party)


# Combine

result_bind <- bind_rows(result_global, result_year, result_party) %>% 
              mutate(id=paste("Lagged Vote Share:", id)) %>%
              select(id,PVALrb, tau, CIlrb, CIurb, N, h) %>%
  mutate_if(is.numeric, round, digits=3)

colnms  = c("Outcome", "P-value", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Number of cases", "Bandwidth")


dim(result_bind)
length(colnms)

table<-Hmisc::latex(result_bind, file=here("outputs", "appendix", "table_app_lag.tex"), 
                    title="",
                    table.env=FALSE,
                    center="none",
                    colheads=colnms,
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity on Lagged Values Validity}", 
                    n.cgroup=7, 
                    cgroup = "\\bfseries{\\textsc{Outcome: Vote Share Co-partisans for the House Election in the previous election}}")




# Robustness Checks -------------------------------------------------------

# Models using the rate of change of the partisan vote share

# Data Wragling -----------------------------------------------------------
load(here("data", "data_complete_methods_paper.Rdata"))

d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")
df <- d
df <- df %>% 
  mutate(diff_vote_share_fed=ifelse(is.na(diff_vote_share_fed), 0, diff_vote_share_fed)) 

# Model Global with local estimation ------------------------------------------------------------

# Global Model ------------------------------------------------------------

# Definining the variables
y<- df$diff_vote_share_fed
r <- 100*df$vote_margin_share
band <- list(1,5,10, "opt", 25, 100)

# Model
res <- sim_rd(y=y, x=100*df$vote_margin_share, 
              vary = band, poly=1)

res_global <- rdrobust(y, df$vote_margin_share)

res_global <- extract_conv(res_global)


# Selecting the Results

result <- map_df(res, extract)

# Graph All the data
cut <- cut(df$vote_margin_share,500, include.lowest = TRUE)
tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((df$vote_margin_share), by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(margin = tmp1$x, y = tmp$x)

# Within the bandwidth
ggplot() +
  geom_point(data = data[data$margin <0 & data$margin>-.15,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  geom_point(data = data[data$margin >0 & data$margin<+.15,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  stat_smooth(data = df[df$vote_margin_share >-.15 & df$vote_margin_share<.15, ],
              aes(vote_margin_share, diff_vote_share_fed, group=treat, color=treat)) +
  geom_hline(yintercept=0, linetype="dotted") +
  xlab("Margin (Results within the optimal bandwidth)") +
  geom_vline(xintercept=0, linetype="dotted") +
  ylab("Vote Share for the House Elections") + 
  scale_color_discrete(name="") +
  geom_vline(xintercept=-0.14, linetype="F1", color="tomato2", alpha=.5) +
  geom_vline(xintercept=0.14, linetype="F1", color="tomato2", alpha=.5) 


ggsave(here("outputs","appendix","app_figure_8.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Global

ggplot() +
  geom_point(data = data[data$margin <0 ,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  geom_point(data = data[data$margin >0 ,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  stat_smooth(data = df,
              aes(vote_margin_share, diff_vote_share_fed, group=treat, color=treat)) +
  geom_hline(yintercept=0, linetype="dotted") +
  xlab("Margin (Results within the optimal bandwidth)") +
  geom_vline(xintercept=0, linetype="dotted") +
  ylab("Vote Share for the House Elections") + 
  scale_color_discrete(name="") +
  geom_vline(xintercept=-0.14, linetype="F1", color="tomato2", alpha=.5) +
  geom_vline(xintercept=0.14, linetype="F1", color="tomato2", alpha=.5) 

ggsave(paste0(output,"diff_res_no_band.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# global Results

d_fig <- result %>% select(h, tau, CIlrb, CIurb,  N) %>%
  arrange(., h) %>%
  mutate(N=as.character(N),
         h=c(1, 5, 10, "optimal bandwidth", 25, 100))




# Models Year -------------------------------------------------------------


year <- c(2000, 2004, 2008, 2012)


# Model by year

d_year <- map(year, function(x) df %>% filter(ANO_ELEICAO ==x))  %>% 
  map(~ rdrobust(y=.$diff_vote_share_fed, x=100*.$vote_margin_share, # this is awesome
                 c=0, p=1, all=T)) %>%
  map_df(~extract_conv(.)) 


#~ shortcut for function(x) = map_df(., extract) gives me the same results


d_fig2 <- d_year %>% select(h, tau, CIlrb, CIurb,  N) %>%
  rbind(d_fig[4,  ]) %>%
  mutate(year= c("2000", "2004", "2008", "2012", "All years"), 
         h=c(18.44, 15.79, 14.35, 13.82, 14.4))

# Incumbents who run ------------------------------------------------------

# Local linear for incumbents ---------------------------------------------

# Lame duck effect

df_ld <- df %>% group_by(id) %>%  # here i am summarizing if the mayor in the pair runs, 
  #   the whole pair gets 1 to be subseted
  summarise(inc_id= sum(mayor_t0), 
            lame_duck_id = sum(lame_duck_t0)) %>% 
  left_join(., df) %>% 
  mutate(career=ifelse(inc_id==1, 1, 
                       ifelse(lame_duck_id==1, 2, 0)))


df_inc_nested <- df_ld %>% group_by(career) %>% 
  nest()


res_inc_ld_norun <- map(df_inc_nested$data, ~
                          rdrobust(y=.$diff_vote_share_fed, x=100*.$vote_margin_share, # this is awesome
                                   c=0, p=1, all=T)) %>% 
  map_df(., extract_conv) %>%
  mutate(Career=factor(df_inc_nested$career), 
         label=ifelse(Career==0, "Open Seat Sample",
                      ifelse(Career==1, "Reelection Sample", "Lame Duck Sample")))



# Deputies and governor ---------------------------------------------------

df_ndep <- df %>% filter(rankvoter==1) %>% 
  mutate(id_ndep=ifelse(n_dep_house>0, 1,0)) %>% 
  dplyr::select(id, id_ndep) %>% 
  left_join(., df) %>%
  group_by(id_ndep) %>%                             
  nest()

res_dep <- map(df_ndep$data, ~
                 rdrobust(y=.$diff_vote_share_fed, x=100*.$vote_margin_share, # this is awesome
                          c=0, p=1, all=T)) %>% 
  map_df(., extract_conv) %>%
  mutate(Career=factor(df_ndep$id_ndep), 
         label=ifelse(df_ndep$id_ndep==1, "With a House Representative", "Without a House Representative"))


# Governor ----------------------------------------------------------------

df_gov <- df %>% group_by(id) %>%  # here i am summarizing if the
  # mayor in the pair runs, 
  #   the whole pair gets 1 to be subseted
  summarise(inc_gov= sum(gov_same_party)) %>%
  left_join(., df) %>%
  mutate(inc_gov=ifelse(inc_gov==2, 1, inc_gov),
         gov_winner=ifelse(rankvoter==1 & inc_gov==1 & gov_same_party==1 , 1, 0)) %>% 
  group_by(id) %>% 
  summarise(inc_gov_winner= sum(gov_winner)) %>% 
  left_join(., df) %>% 
  group_by(inc_gov_winner) %>%
  nest()

res_gov <- map(df_gov$data, ~
                 rdrobust(y=.$diff_vote_share_fed, x=100*.$vote_margin_share, # this is awesome
                          c=0.1, p=1, all=T)) %>%  # not working when fixed at zero, weirdd. I think it is a problem few points
  map_df(., extract_conv) %>%
  mutate(Career=as.factor(df_gov$inc_gov_winner), 
         label=ifelse(Career==1, "Governor Same Party", "Governor Different Party"))

#### Latex table

result <- res_global %>% mutate(label="Incumbent") 

d_year <- d_year %>% mutate(label = c("Year: 2000", "Year: 2004", "Year: 2008", "Year: 2012"))

res <- rbind(res_inc_ld_norun,  res_gov, res_dep) %>% select(-Career)

res <- rbind(result, d_year, res)

result <- res  %>% select(label, h, tau, CIlrb, CIurb, N) %>%
  mutate_if(is.numeric, round, digits=3)

colnms  = c("Subsample", "Optimal Bandwidth (Margin of Victory)", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Number of cases")

table<-Hmisc::latex(result, file=here("outputs", "appendix", "table_9.tex"), 
                    title="",
                    table.env=FALSE,
                    center="none",
                    col.just = c("l",rep("c",ncol(result)-1)),
                    colheads=colnms,
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity Results across the subsamples }", 
                    n.cgroup=6, 
                    cgroup = "\\bfseries{\\textsc{Outcome: Vote Share Co-partisans for the House Election}}")

# Global Model ------------------------------------------------------------

# Definining the variables

y<- df$vs_party_top_1
r <- 100*df$vote_margin_share

band <- list(1,5,10, "opt", 25, 100)

# Model
res <- sim_rd(y=y, x=100*df$vs_party_top_1, 
              vary = band)

res_global <- rdrobust(y, df$vote_margin_share)

res_global <- extract(res_global)


# Selecting the Results

result <- map_df(res, extract)

# Graph All the data

cut <- cut(df$vote_margin_share,500, include.lowest = TRUE)
tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((df$vote_margin_share), by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(margin = tmp1$x, y = tmp$x)

# Within the bandwidth
ggplot() +
  geom_point(data = data[data$margin <0 & data$margin>-.14,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  geom_point(data = data[data$margin >0 & data$margin<+.14,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  stat_smooth(data = df[df$vote_margin_share >-.15 & df$vote_margin_share<.15, ],
              aes(vote_margin_share, vs_party_top_1, group=treat, color=treat)) +
  geom_hline(yintercept=0, linetype="dotted") +
  xlab("Margin (Results within the optimal bandwidth)") +
  geom_vline(xintercept=0, linetype="dotted") +
  ylab("Vote Share for the House Elections") + 
  scale_color_discrete(name="") +
  geom_vline(xintercept=-0.14, linetype="F1", color="tomato2", alpha=.5) +
  geom_vline(xintercept=0.14, linetype="F1", color="tomato2", alpha=.5) +
  theme_tiago


ggsave(paste0(output,"top_one_band.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# By year


year <- c(2000, 2004, 2008, 2012)


# Model by year

d_year <- map(year, function(x) df %>% filter(ANO_ELEICAO ==x))  %>% 
  map(~ rdrobust(y=.$vs_party_top_1, x=100*.$vote_margin_share, # this is awesome
                 c=0, p=1, all=T)) %>%
  map_df(~extract(.)) 


#~ shortcut for function(x) = map_df(., extract) gives me the same results
res

d_fig2 <- bind_rows(res_global, d_year) %>%
  select(h, tau, CIlrb, CIurb,  N) %>%
  mutate(year= c("All years", "2000", "2004", "2008", "2012"),
         h=c(14.4, 18.25, 17.11, 16.51, 15.54)) %>%
  select(year, everything())


result <- d_fig2 %>%
  mutate_if(is.numeric, round, digits=3)

colnms  = c("Subsample", "Optimal Bandwidth", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Number of cases")

table<-Hmisc::latex(result, file=paste0(output,"table_top_cand.tex"), 
                    title="",
                    table.env=FALSE,
                    center="none",
                    col.just = c("l",rep("c",ncol(result)-1)),
                    colheads=colnms,
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity Results (Average and by Year) }", 
                    n.cgroup=6, 
                    cgroup = "\\bfseries{\\textsc{Outcome: Probability of Being the Most Voted Party at the Incumbent's Municipality}}")



# Most Voted Candidates ---------------------------------------------------

y<- df$vs_party_top_3
r <- 100*df$vote_margin_share

band <- list(1,5,10, "opt", 25, 100)

# Model
res <- sim_rd(y=y, x=100*df$vs_party_top_3, 
              vary = band, poly=1)

res_global <- rdrobust(y, df$vote_margin_share)

res_global <- extract(res_global)


# Selecting the Results

result <- map_df(res, extract)

# Graph All the data

cut <- cut(df$vote_margin_share,500, include.lowest = TRUE)
tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((df$vote_margin_share), by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(margin = tmp1$x, y = tmp$x)

# Within the bandwidth
ggplot() +
  geom_point(data = data[data$margin <0 & data$margin>-.14,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  geom_point(data = data[data$margin >0 & data$margin<+.14,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  stat_smooth(data = df[df$vote_margin_share >-.15 & df$vote_margin_share<.15, ],
              aes(vote_margin_share, vs_party_top_3, group=treat, color=treat)) +
  geom_hline(yintercept=0, linetype="dotted") +
  xlab("Margin (Results within the optimal bandwidth)") +
  geom_vline(xintercept=0, linetype="dotted") +
  ylab("Vote Share for the House Elections") + 
  scale_color_discrete(name="") +
  geom_vline(xintercept=-0.14, linetype="F1", color="tomato2", alpha=.5) +
  geom_vline(xintercept=0.14, linetype="F1", color="tomato2", alpha=.5) +
  theme_tiago


ggsave(paste0(output,"top_three_band.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# By year


year <- c(2000, 2004, 2008, 2012)


# Model by year

d_year <- map(year, function(x) df %>% filter(ANO_ELEICAO ==x))  %>% 
  map(~ rdrobust(y=.$vs_party_top_3, x=100*.$vote_margin_share, # this is awesome
                 c=0, p=1, all=T)) %>%
  map_df(~extract(.)) 


#~ shortcut for function(x) = map_df(., extract) gives me the same results
res

d_fig2 <- bind_rows(res_global, d_year) %>%
  select(h, tau, CIlrb, CIurb,  N) %>%
  mutate(year= c("All years", "2000", "2004", "2008", "2012"),
         h=c(14.4, 18.25, 17.11, 16.51, 15.54)) %>%
  select(year, everything())

result <- d_fig2 %>%
  mutate_if(is.numeric, round, digits=3)

colnms  = c("Subsample", "Optimal Bandwidth", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Number of cases")

table<-Hmisc::latex(result, file=paste0(output,"table_top3_cand.tex"), 
                    title="",
                    table.env=FALSE,
                    center="none",
                    col.just = c("l",rep("c",ncol(result)-1)),
                    colheads=colnms,
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity Results (Average and by Year) }", 
                    n.cgroup=6, 
                    cgroup = "\\bfseries{\\textsc{Outcome: Probability of Being the Most Voted Party at the Incumbent's Municipality}}")







