## Replication Materials

Replication materials for Ventura, Tiago "Do mayors matter? Reverse coattails on congressional elections in Brazil". Forthcoming in the Electoral Studies 

> __Abstract:__
> In federal democracies, parties often invest in local politics as a strategy to improve their performance on upcoming national elections. In this study, I use the concept of reverse coattails to investigate how winning local elections affect upper-level electoral dynamics in Brazil. Using a regression discontinuity design (RDD), I show that parties in Brazil boost their national performance, earning more votes on House elections in districts where their members control local offices. I discuss how access to ‘‘pork” controlled by co-partisan House members and mechanical information gains explain these effects. Additionally, I use a Bayesian LASSO algorithm to address data sparsity in RDD designs, and to demonstrate the existence of pro-large party bias on the coattail effects. By disentangling the various effects of winning local elections, this paper contributes to a greater understanding of how parties build electoral strength in fragmented democracies.

The latest pre-print can be found [here](do_mayors_matter.pdf). The published version is [here]()

## Tutorial 

This README file provides an overview of the replications materials for the article. The R codes used in the article can be found under the folder **Codes**. The electoral data, which I fully describe below, is under the folder **data**. Results are exported to the folder **output**. 

## Codes

- `analysis_main_effects.R`: Implements the main models for reverse coattails presented in section 5. It also implements results on section 8 about party coordination and mayors' career incentives. 

- `analysis_section3.R`: Replicates the descriptive results with the number of effective parties in section 3. 

- `analysis_section6.R`: Replicates the results for section 6; the pork mechanism discussed in the paper. 

- `analysis_section7.R`: Replicates the results for section 7; the information mechanism discussed in the paper. 

- `analysis_partyeffects.R`: Replicates the results with the reverse coattails by parties. It implements the bayesian lasso strategy to deal with sparsity in regression discontinuity designs. It also replicates results with local linear models presented in the appendix. 

- `analysis_appendix.R`: Replicates remaining robustness and validity checks in the appendix. It is important to notice that some of the results in the appendix are presented in the above codes.

- `functions.R`: contains a set of miscelanous functions I heavily rely upon in the previous codes. 

## Data

Here, one can find the data and models used in the paper. Most of the data came from the Superior Electoral Court. The exception is the data on House Budgetary Amendment that was provided by CEBRAP. In this folder, it is also avaiable the Sparse Models used in the paper. 
