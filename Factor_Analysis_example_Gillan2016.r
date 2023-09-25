###
# LOAD DEPENDENCIES
### 

library(tidyverse)
library(scales) # for color palette
library(psy) # for scree plot
library(nFactors) # for CNG test
library(psych) # for fa()
library(lavaan) # for SEM
library(ggcorrplot)

###
# PRELIMINARIES
### 

## Clear workspace
rm(list = ls())

## Set working directory
setwd('/Users/angelaradulescu/Dropbox/Princeton/Research/moat/scripts/moat-online/notebooks')

## Read data
sample_data = read.csv('../data/factor_analysis_sample_data/gillan2016_individual_items_study2.csv') 

## Remove stray and subject column
data <- sample_data %>% select(-c('X'))
data <- data %>% select(-c('subject'))

## For graph width
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 500)

###
# DATA WRANGLING
### 

## Define breakpoints in the column list: the column index where a new subscale begins
cols <- colnames(data)
q_breaks <- c()
for (i in 3:length(cols)){
    # split item code into characters
    new_cols <- unlist(strsplit(cols[i],split=''))
    old_cols <- unlist(strsplit(cols[i-1],split=''))
    # Split if new instrument
    if (!(new_cols[1] == old_cols[1] & new_cols[2] == old_cols[2])){
        q_breaks <- c(q_breaks,i)
    }
}
q_breaks <- c(q_breaks, length(cols)+1)

## Check that we split the dataframe correctly
for(i in 2:length(q_breaks)){
    print('New instrument: ')
    print(colnames(data)[q_breaks[i-1]:(q_breaks[i]-1)])
}

## Add subscale names
subscale_names <- c('OCD','Eating disorders', 'Apathy',
                    'Alcohol misuse','Depression','Trait anxiety',
                   'Impulsivity', 'Social anxiety')

sumscore_col_names <- c('OCD','Eating_disorders', 'Apathy',
                    'Alcohol_misuse','Depression','Trait_anxiety',
                   'Impulsivity', 'Social_anxiety')


## Make a dataframe with subscale names
names <- c()
for(i in 2:length(q_breaks)){
    names <- c(names, rep(subscale_names[i-1],q_breaks[i] - q_breaks[i-1]))
    }

###
# COMPUTE SUM SCORES (don't run this if you want to go straight to EFA)
###           

for (q in 2:length(q_breaks)){
    data[sumscore_col_names[q-1]] = rowSums(data[,q_breaks[q-1]:(q_breaks[q]-1)], na.rm=TRUE)
}

###
# EXPLORATORY FACTOR ANALYSIS
###  

# Define color palette
pal = hue_pal(l=65)(13)

# Perform factor analysis with varimax rotation and score regression
fit_4f <- factanal(data, 3, rotation="varimax", scores='regression')
print(fit_4f, digits=2, cutoff=.3, sort=TRUE)

# Save the loadings and the factor scores
load_4f <- data.frame(fit_4f$loadings[,1:3])
load_4f$items <- rownames(load_4f)
write.csv(load_4f, "load_4f.csv")
write.csv(fit_4f$scores, "scores_4f.csv")

###
# CHECK OPTIMAL SOLUTION USING THE CATTELL-NELSON-GORSUCH MODIFIED SCREE TEST
### 

results <- nCng(fit_4f$correlation, cor=TRUE, model = "factors", details = TRUE)

plotuScree(fit_4f$correlation, main=paste(results$nFactors,
                            " factors retained by the CNG procedure",
                            sep=""))

## Other checks

# Parallel analysis
ev <- eigen(cor(questions_only)) # get eigenvalues
ap <- parallel(subject=nrow(questions_only),var=ncol(questions_only), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

fa.parallel(questions_only)

