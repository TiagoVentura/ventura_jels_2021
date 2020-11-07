## Replication Materials

Replication materials for Ventura, Tiago "Do mayors matter? Reverse coattails on congressional elections in Brazil". Forthcoming in the Electoral Studies 

> __Abstract:__
> In federal democracies, parties often invest in local politics as a strategy to improve their performance on upcoming national elections. In this study, I use the concept of reverse coattails to investigate how winning local elections affect upper-level electoral dynamics in Brazil. Using a regression discontinuity design (RDD), I show that parties in Brazil boost their national performance, earning more votes on House elections in districts where their members control local offices. I discuss how access to ‘‘pork” controlled by co-partisan House members and mechanical information gains explain these effects. Additionally, I use a Bayesian LASSO algorithm to address data sparsity in RDD designs, and to demonstrate the existence of pro-large party bias on the coattail effects. By disentangling the various effects of winning local elections, this paper contributes to a greater understanding of how parties build electoral strength in fragmented democracies.

The latest pre-print can be found [here](do_mayors_matter.pdf). The published version is [here]()

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The electoral data, which I fully describe below, is under the folder **data**. Results are exported to the folder **output**. 

## Codes

- `CV_analysis_descriptive.R`: Implements the first descriptive analysis of the paper. The code replicates figures 1 and 2 of the paper, and figure 6 and table 6 in the SIF. 

- `CV_analysis_diff_in_diff.R`: Replicates the diff-in-diff results for the Speech of Bolsonaro on risk perceptions. (table 1, figures 7, and 8).

- `CV_analysis_experiments.R`: Replicates the results for the social framing experiment, including the section unpacking the null findings. 

## Data

Our paper presents analyses based on observation, quasi-experimental, and a framing experiment using novel data from a national online survey fielded by Netquest-Vanderbilt. The survey uses probabilistic samples drawn by the LAPOP team in Vanderbilt implemented with the panel of users registered with Netquest.  The entire survey and the embedded framing experiment received the approval of the University of Maryland Institutional Board Review 1552091-3. 

The parts of the survey data used in the paper are available under `data/CV_data.Rdata`. We direct the reader to tables 3 and 4 in the appendix for a in-depth description of each variable.  The dataset contains de following variables:

- **runoff_bolsonaro**: Likely to vote for Bolsonaro (runoff)          
- **runoff_haddad**: Likely to vote for Haddad (runoff)
- **vote_haddad**: Likely to Vote for Haddad (First Round)               
- **vote_bolsonaro**: Likely to Vote for Bolsonaro (First Round)        
- **runoff_nulo** : Likely to Vote Null/Blank/Others (Runoff)              
- **ideo_place_self**: Ideological Self-Placement    
- **positive_partisanship**: Which Party do you Like more?     
- **negative_partisanship**:  Which Party do you dislike more? 
- **covid_job**: Job risk assesment due to COVID-19                 
- **covid_health**: Health risk assessment due to COVID-19         
- **covid_government**: Support for how the government is reacting during the pandemic          
- **treat_reaction**: Behavioral Reaction to the Treatment      
- **treat_emotions**: Emotional Reaction to the Treatment            
- **income**: Subjective income measure 
- **age**: Respondents' Age                       
- **gender**: Respondents' Gender   
- **work**: Respondents' Occupational Status
- **education**: Respondents' Education  
- **startdate**: Start date of the Survey Response                 
- **enddate**: End Date of the Survey Response
- **treat_reaction_page_submit**: Lantency Measure to the Treatment
- **treat_negative**: Treatment Negative Tweet  
- **treatment_haddad**: Treatment Haddad Tweet 
- **treatment_bolsonaro**: Treatment Eduardo Bolsonaro Tweet     
- **negative_bolsonaro**: Treatment Negative Edurdo Bolsonaro     
- **neutral_bolsonaro**: Treatment Positive Eduardo Bolsonaro    
- **negative_haddad**: Treatment Negative Haddad           
- **neutral_haddad**: Treatment Positive Haddad




