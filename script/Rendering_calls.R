library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md

render(input = "script/preparations/1_Extract_parties_voted_in_ESS.Rmd",
       envir = new.env())


render(input = "script/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
       envir = new.env())


render(input = "script/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
       envir = new.env())


render(input = "script/preparations/4_Variable_transformations.Rmd",
       envir = new.env())


render(input = "script/analysis/Descriptive_statistics.Rmd",
       envir = new.env())

render(input = "script/analysis/Analysis.Rmd",
       envir = new.env())

render(input = "script/analysis/Analysis_added_covariates.Rmd",
       envir = new.env())

# Convert RMD to R

purl(input="script/preparations/1_Extract_parties_voted_in_ESS.Rmd",
     output="script/preparations/1_Extract_parties_voted_in_ESS.R",
     documentation = 2)

purl(input="script/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
     output="script/preparations/2_Assign_ESS_party_numbers_to_CHES.R",
     documentation = 2)

purl(input="script/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
     output="script/preparations/3_merge_ESS_and_CHES_by_vote.R",
     documentation = 2)

purl(input="script/preparations/4_Variable_transformations.Rmd",
     output="script/preparations/4_Variable_transformations.R",
     documentation = 2)

purl(input="script/analysis/Descriptive_statistics.Rmd",
     output="script/analysis/Descriptive_statistics.R",
     documentation = 2)

purl(input="script/analysis/Analysis.Rmd",
     output="script/analysis/Analysis.R",
     documentation = 2)

purl(input="script/analysis/Analysis_added_covariates.Rmd",
     output="script/analysis/Analysis_added_covariates.R",
     documentation = 2)

