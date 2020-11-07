#####################################################
# Do Mayors Matter ? JELS 2021
# Tiago Ventura
# November 7
######################################################

# Code: Main Results

# Description: This codes replicates the main findings of 
# "Do mayors matter? Reverse coattails on congressional elections in Brazil"
# Electoral Studies, 2021.

# Call Packages ------------------------------------------------------------------

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
library(extrafont)
library(here)


# Call Utils --------------------------------------------------------------
source(here("code", "functions.R"))
options(width=150)



# Load Data ---------------------------------------------------------------

load(paste0(path_data, "/data_complete_methods_paper.Rdata"))

# Treatment -----------------------------------------------------------

d$treat <- ifelse(d$vote_margin_share <0, "Non Incumbent", "Incumbent")
df <- d


# Figure 2 ----------------------------------------------------------------

cut <- cut(df$vote_margin_share,500, include.lowest = TRUE)
tmp <- aggregate(y, by=list(cut = cut), FUN=mean, na.rm=T)
tmp1 <- aggregate((df$vote_margin_share), by=list(cut = cut), FUN=mean, na.rm=T)
data <- data.frame(margin = tmp1$x, y = tmp$x)

ggplot() +
  geom_point(data = data[data$margin <0 ,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  geom_point(data = data[data$margin >0 ,], aes(margin, y),na.rm=T,size=3, color = 'gray', alpha=.8) +
  stat_smooth(data = df,
              aes(vote_margin_share, vs_party_fed, group=treat, color=treat)) +
  geom_hline(yintercept=0, linetype="dotted") +
  xlab("Margin (Results within the optimal bandwidth)") +
  geom_vline(xintercept=0, linetype="dotted") +
  ylab("Vote Share for the House Elections") + 
  scale_color_discrete(name="") +
  geom_vline(xintercept=-0.14, linetype="F1", color="tomato2", alpha=.5) +
  geom_vline(xintercept=0.14, linetype="F1", color="tomato2", alpha=.5) 

ggsave(here("outputs","figure_2.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Models ------------------------------------------------------------

# Definining the variables
y<- df$vs_party_fed
r <- 100*df$vote_margin_share
band <- list("opt", 1,5,10, 25, 100)

# Model local linear
res <- sim_rd(y=y, x=100*df$vote_margin_share, 
              vary = band, p=1) 

# Selecting the Results
result <- map_df(res, extract)

# Model local linear: poly1 

band_app <- list("opt", 1, 5, 10, 15, 25, 100)


resp1 <- sim_rd(y=y, x=100*df$vote_margin_share, 
                vary = band_app, p=1) 

resultp1 <- map_df(resp1, extract)

# Model local linear: poly2
band_app <- list( "opt", 1,5,10, 15, 25, 100)

resp2 <- sim_rd(y=y, x=100*df$vote_margin_share, 
              vary = band_app, p=2) 

# Selecting the Results
resultp2 <- map_df(resp2, extract)

# Model
resp3 <- sim_rd(y=y, x=100*df$vote_margin_share, 
              vary = band_app, p=3) 

# Selecting the Results
resultp3 <- map_df(resp3, extract)


# Linear Models -----------------------------------------------------------


df_for_bands <- df %>%
  mutate(b_1= ifelse(vote_margin_share>-0.0101 & vote_margin_share<0.0101, 
                             TRUE, FALSE), 
         b_5= ifelse(vote_margin_share>-0.0501 & vote_margin_share<0.0501, 
                                   TRUE, FALSE), 
         b_10=ifelse(vote_margin_share>-0.101 & vote_margin_share<0.101, 
                                    TRUE, FALSE), 
         b_15=ifelse(vote_margin_share>-0.151 & vote_margin_share<0.151, 
                     TRUE, FALSE), 
         
         b_25 = ifelse(vote_margin_share>-0.2501 & vote_margin_share<0.2501, 
                                    TRUE, FALSE), 
         treat=fct_relevel(treat, "Non Incumbent"))



d_1<- df_for_bands %>% 
  filter(b_1==TRUE)
d_5<- df_for_bands %>% 
  filter(b_5==TRUE)
d_10<- df_for_bands %>% 
  filter(b_10==TRUE)
d_15<- df_for_bands %>% 
  filter(b_15==TRUE)
d_25<- df_for_bands %>% 
  filter(b_25==TRUE)
d_100 <- df_for_bands

l_data <- list(d_1, d_5, d_10, d_15, d_25, d_100)
l_bw <- list("1%", "5%", "10%", "15%", "25%", "Entire Sample")

# Difference in Means -----------------------------------------------------
dif_mean <- map2_df(l_data, l_bw, ~ lm(vs_party_fed ~ treat, data=.x) %>% 
                broom::tidy() %>%
                mutate(bw=.y, 
                       CIlrb=estimate - 1.96*std.error,
                       CIurb=estimate + 1.96*std.error) %>%
                filter(term=="treatIncumbent") %>%
                select(term, estimate, p.value, CIlrb, CIurb, bw))

# Quadratic ---------------------------------------------------------------
dif_quad <- map2_df(l_data, l_bw, ~ lm(vs_party_fed ~ treat + vote_margin_share + I(vote_margin_share^2), data=.x) %>% 
      broom::tidy() %>%
      mutate(bw=.y, 
             CIlrb=estimate - 1.96*std.error,
             CIurb=estimate + 1.96*std.error) %>%
      filter(term=="treatIncumbent") %>%
      select(term, estimate, p.value, CIlrb, CIurb, bw))

# Cubic ---------------------------------------------------------------
dif_cubic <- map2_df(l_data, l_bw, ~ 
      lm(vs_party_fed ~ treat + vote_margin_share + I(vote_margin_share^2) + I(vote_margin_share^3), data=.x) %>% 
      broom::tidy() %>%
        mutate(bw=.y, 
               CIlrb=estimate - 1.96*std.error,
               CIurb=estimate + 1.96*std.error) %>%
        filter(term=="treatIncumbent") %>%
        select(term, estimate, p.value,CIlrb, CIurb, bw))

  



# Table 1 ------------------------------------------------------

d_fig %<>%  mutate_if(is.numeric, round, digits=3) %<>%
  mutate(h=paste(c(1, 5, 10, 14.4, 25, 100),"\\%", sep="")) # add by hand the optimal value label

colnms  = c("Bandwidth (Margin of Victory)", "Estimate", "Lower 95\\% CI",
            "Lower 95\\% CI","Number of cases")


table<- Hmisc::latex(d_fig, file=here("outputs","table_1.tex"),
             title="",
             table.env=FALSE,
             center="none",
             col.just = c("l",rep("c",ncol(d_fig)-1)),
             colheads=colnms,
             booktabs = TRUE, 
             rowname = NULL, 
             caption = "\\textsc{Regression Discontinuity Results}", 
             n.cgroup=5, 
             cgroup = "\\textsc{Outcome: Vote Share Co-partisans for the House Election}")


# Tables 8 Appendix ---------------------------------------------------------

# Local Models

resultp1 <- resultp1 %>% 
  mutate_if(is.numeric, round, digits=3) %>%
  mutate(estimate=paste0(tau, " (", PVALrb, ")"), 
         bw=paste("bw <", h, "%")) %>%
  select(bw, estimate) 

resultp2 <- resultp2 %>% 
          mutate_if(is.numeric, round, digits=3) %>%
          mutate(estimate=paste0(tau, " (", PVALrb, ")"), 
          bw=paste("bw <", h, "%")) %>%
         select(estimate) 
  
resultp3 <- resultp3 %>% 
  mutate_if(is.numeric, round, digits=3) %>%
  mutate( estimate=paste0(tau, " (", PVALrb, ")"), 
    bw=paste("bw <", h, "%")) %>%
  select(estimate) 

# Global Models

# Base for global
base <- tribble(~estimate, "") 

# diff means
dif_mean %<>%
  mutate_if(is.numeric, round, digits=5) %>%
  mutate(estimate=paste0(estimate, " (", p.value, ")")) %>%
  select(estimate) %>%
  bind_rows(base, .)

# quadratic
dif_quad %<>%
  mutate_if(is.numeric, round, digits=5) %>%
  mutate(estimate=paste0(estimate, " (", p.value, ")")) %>%
  select(estimate) %>%
  bind_rows(base, .)

# cubic
dif_cubic %<>%
  mutate_if(is.numeric, round, digits=5) %>%
  mutate(estimate=paste0(estimate, " (", p.value, ")")) %>%
  select(estimate) %>%
  bind_rows(base, .)

# Table 8
res_appendix <- bind_cols(resultp1, resultp2, resultp3, dif_mean, dif_quad, dif_cubic)

table <- Hmisc::latex(res_appendix, 
                    file=here("outputs", "table_appendix_robust.tex"),
                    title="",
                    table.env=FALSE,
                    center="none",
                    col.just = c("l",rep("c",ncol(res_appendix)-1)),
                    colheads=c("Bandwidth", "Local Linear", "Local Quadratic", 
                               "Local Cubic", "Diff-in-Means", "Linear Quadratic", 
                               "Linear Cubic"),
                    booktabs = TRUE, 
                    rowname = NULL, 
                    caption = "\\textsc{Regression Discontinuity Results: Robustness Checks}", 
                    n.cgroup=7, 
                    cgroup = "\\textsc{Outcome: Vote Share Co-partisans for the House Election}")



# Mayors Career: Fig 8 ------------------------------------------------------

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
             rdrobust(y=.$vs_party_fed, x=100*.$vote_margin_share, # this is awesome
                      c=0, p=1, all=T)) %>% 
  map_df(., extract) %>%
  mutate(Career=factor(df_inc_nested$career), 
         label=ifelse(Career==0, "Open Seat Sample",
                      ifelse(Career==1, "Reelection Sample", "Lame Duck Sample")))


ggplot(res_inc_ld_norun, aes(y=tau, x=Career, ymax=CIurb, ymin=CIlrb, fill=factor(Career), group=Career, label=N)) +
  geom_point(size=4, shape=21) + geom_errorbar(width=.2) + 
  scale_color_discrete(name="Bandwidth") +coord_flip()  + 
  ylab("Reverse Coattails on the Vote Share for the House Elections") +
  xlab("") + 
  scale_fill_manual(values=c("salmon", "steelblue", "gold")) +
  geom_hline(yintercept=0, linetype="dotted") +
  ylim(-.05, 0.1) + 
  geom_text(vjust=2, size=6)+
  scale_x_discrete(labels=c("Open Seat Sample", 
                            "Reelection Sample", 
                            "Lame Duck Sample")) +
  guides(fill="none") 

ggsave(here("outputs", "figure_8.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Deputies and governor ---------------------------------------------------

df_ndep <- df %>% filter(rankvoter==1) %>% 
  mutate(id_ndep=ifelse(n_dep_house>0, 1,0)) %>% 
  dplyr::select(id, id_ndep) %>% 
  left_join(., df) %>%
  group_by(id_ndep) %>%                             
  nest()


res_dep <- map(df_ndep$data, ~
             rdrobust(y=.$vs_party_fed, x=100*.$vote_margin_share, # this is awesome
                      c=0, p=1, all=T)) %>% 
  map_df(., extract) %>%
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
                 rdrobust(y=.$vs_party_fed, x=100*.$vote_margin_share, # this is awesome
                          c=0.1, p=1, all=T)) %>%  # not working when fixed at zero, weirdd. I think it is a problem few points
  map_df(., extract) %>%
  mutate(Career=as.factor(df_gov$inc_gov_winner), 
         label=ifelse(Career==1, "Governor Same Party", "Governor Different Party"))


# Combining Governor and Incumbent Running: Fig 7a --------------------------------

res_yes <- rbind(res_gov[2,], res_dep[2,])

ggplot(res_yes, aes(y=tau, x=label, ymax=CIurb, ymin=CIlrb, fill=label,
                    label=N)) +
  geom_point(size=4, shape=21) + geom_errorbar(width=.2) + coord_flip() + 
  scale_color_discrete(name="Bandwidth")  +
  ylab("\n Reverse Coattails on the Vote Share for the House Elections") +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  geom_text(vjust=2, size=6) + 
  theme(axis.text.y = element_text(size=20)) +
  guides(fill="none")


ggsave(here("outputs", "figure_7a.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


## Appendix Figure 2

res_yes <- rbind(res_gov[1,], res_dep[1,])

ggplot(res_yes, aes(y=tau, x=label, ymax=CIurb, ymin=CIlrb, fill=label,
                    label=N)) +
  geom_point(size=4, shape=21) + geom_errorbar(width=.2) + coord_flip() + 
  scale_color_discrete(name="Bandwidth")  +
  ylab("Reverse Coattails on the Vote Share for the House Elections") +
  xlab("") + 
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  geom_text(vjust=2, size=6) + 
  guides(fill="none")

ggsave(here("outputs", "app_figure_2.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# House Incumbency Advantage: Fig 7b -------------------------------------------------

df_house_inc <- df %>% 
  mutate(id_house_inc=ifelse(is.na(n), 0, n), 
         id_house_inc=as.numeric(ifelse(id_house_inc>1, 1,0))) %>% 
  group_by(id) %>%
  mutate(house_inc=sum(id_house_inc))  %>% 
  ungroup() %>%
  mutate(house_inc=ifelse(house_inc==1, "House Members: \n Reelection Sample",
                    ifelse(house_inc==0, "House Members: \n Open-Seat", NA))) %>%
  filter(!is.na(house_inc)) %>%
  group_by(house_inc) %>%                             
  nest()

res_inc_house <- map(df_house_inc$data, ~
                 rdrobust(y=.$vs_party_fed, x=100*.$vote_margin_share, # this is awesome
                          c=0, p=1, all=T)) %>% 
  map_df(., extract_conv) %>%
  mutate(Career=factor(df_house_inc$house_inc))


#save(res_dep, file=paste0(models, "lrdd_resdep.Rdata"))

ggplot(res_inc_house, aes(y=tau, x=Career, ymax=CIurb, ymin=CIlrb, fill=factor(Career), group=Career, label=N)) +
  geom_point(size=4, shape=21) + geom_errorbar(width=.2) + 
  scale_fill_manual(values=c("salmon", "steelblue", "gold")) +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_text(vjust=2, size=6) + 
  scale_color_discrete(name="Bandwidth") +coord_flip()  +  
  ylab("\n Reverse Coattails on the Vote Share for the House Elections") +
  xlab("") + 
  guides(fill="none") + 
  theme(axis.text.y = element_text(size=20)) +
  ylim(-.2, .2)

ggsave(here("outputs", "figure_7b.png"), width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

