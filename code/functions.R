# functions

# ggplto theme

# utils
# My theme

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
                  legend.text = element_text(size = 16, family = my_font),
                  legend.title = element_text(size=16),
                  plot.title = element_text(size = 22, face = "bold"),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=18),
                  axis.title.x = element_text(hjust=1),
                  strip.background = element_blank(),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 16, face="italic"))
theme_set(theme_light() + my_theme)


# Data for my results

results <- data.frame(NULL)

##############
## Model Incumbents
##############

# Function to extract

extract <- function(mod) {
  out <- data.frame(year="ALL", 
                    party="INC",
                    outcome="Vote_Share_Party", 
                    sample="all", 
                    options="mserd", 
                    h=mod$bws[1],
                    tau=mod$Estimate[2],
                    PVALrb=mod$pv[3], 
                    CIlrb=mod$ci[3],
                    CIurb=mod$ci[6], 
                    error=0, 
                    diffmeans=NA, 
                    diffmeanspval=NA,
                    diffmeanst=NA,
                    diffmeanscil=NA,
                    diffmeansciu=NA,
                    count=1, 
                    N = mod$N_h[[1]]) }

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
                    N = mod$N_h[[1]]) }


# Function to run simultaneous models

sim_rd <- function(y, x, vary, poly) {
  
  # Container
  mod <- list()
  
  # Loopring through the bandwidths
  
  for(i in 1:length(vary)) {
    
    if(vary[[i]]%in%"opt"){
      
      mod[[i]] <- rdrobust(y,x, c=0, p=poly, all=T)
      
    } else{
      
      mod[[i]] <- rdrobust(y,x, c=0, p=poly, all=T, h=vary[[i]])
    }
    
    print(i)  
  }  
  
  return(mod)  
  
}

# Function to extract from lme4

extract_lme <- function(model, data, coef){
  
  
  # Collecting the data and formatting a dataframe
  
  df.random <- data.frame(beta=coef(model)[[1]][,coef], 
                          se = arm::se.coef(model)[[2]][,coef]) 
  
  df.random$party <-as.numeric(rownames(df.random))
  
  df.names <- data %>%
    mutate(number=NUMERO_PARTIDO, 
           name = SIGLA_PARTIDO) 
  
  df.names <- df.names[, c("number", "name")] %>% unique()
  
  df.names <- df.names %>% group_by(number, name) %>% 
    filter(!is.na(name))
  
  
  # Excluding by hand. Quite sure there is a easier way to do it
  
  
  remove <- c("PPB", "PRN", "PFL", "PL")
  
  df.names <-  df.names[!df.names$name %in% remove,]
  
  # Joining
  
  df.random  <- df.random %>%
    left_join(., df.names, by=c("party"="number")) %>%
    mutate(upper = beta + se*1.96, 
           lower = beta - se*1.96)
  
  return(df.random)
}

# Function to extract from rd

extract_rd <- function(mod) {
  out <- data.frame(year="ALL", 
                    party="INC",
                    outcome="Vote_Share_Party", 
                    sample="all", 
                    options="mserd", 
                    h=mod$bws[1],
                    tau=mod$Estimate[2],
                    PVALrb=mod$pv[3], 
                    CIlrb=mod$ci[3],
                    CIurb=mod$ci[6], 
                    error=0, 
                    diffmeans=NA, 
                    diffmeanspval=NA,
                    diffmeanst=NA,
                    diffmeanscil=NA,
                    diffmeansciu=NA,
                    count=1, 
                    N = mod$N_h[[1]]) }

# Function to run simultaneous models

sim_rd <- function(y, x, vary, poly) {
  
  # Container
  mod <- list()
  
  # Loopring through the bandwidths
  
  for(i in 1:length(vary)) {
    
    if(band[[i]]=="opt"){
      
      mod[[i]] <- rdrobust(y, r, c=0, all=T, p=poly)
      
    } else{
      
      mod[[i]] <- rdrobust(y,r, c=0, all=T, h=vary[[i]], p=poly)
    }
    
    print(i)  
  }  
  
  return(mod)  
  
}
extract_sparse <- function(model, names) {
  
  # Create a patter to get the RDD results
  
  pattern <- "treat" %R% END
  
  # Extract the results. Using the delta method for CI. Nominal coverage compared to 
  # posterior credible intervals. See Tingley e Ratcikiv 
  
  res <- summary(model, printit = FALSE)$table %>% as.data.frame() %>% 
    mutate(names=rownames(.), 
           names_id= ifelse(str_detect(names, pattern), 1, 0), 
           treat = `Posterior Median`, 
           lb= `5%`, 
           up=`95%`) %>%
    filter(names_id==1, treat !=0) 
  
  # Fixing party names
  
  res %>% 
    mutate(NUMERO_PARTIDO= as.character(str_extract(names, DGT %R% DGT))) %>%
    left_join(., names)
  
}
