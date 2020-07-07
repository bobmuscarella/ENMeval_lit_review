### Compile ENMeval citation responses
library(googlesheets4)
library(rcrossref)

### Read the file of papers reviewed
# urldois <- 'https://docs.google.com/spreadsheets/d/1t6u9OU8qVcJGk1SGwpgrc3gZQncsxCX7FEXD2hvj9Cc/edit?usp=sharing'
# doi <- read_sheet(urldois)

### Get citation list
# texttmp <- cr_cn(doi$DOI[!is.na(doi$DOI)], format='text')
# jsoncites <- cr_cn(doi$DOI[!is.na(doi$DOI)], format='citeproc-json')
# years <- unlist(lapply(jsoncites, function(x) x$created$`date-parts`[1]))
# months <- unlist(lapply(jsoncites, function(x) paste(x$created$`date-parts`[1,], collapse='-')))
# sort(months)


### Years of citations
# table(years)
# Reviewed papers ranged from 2016-2019

### Write a file with citations of all papers reviewed
# write.csv(do.call(rbind, texttmp), file='/Users/au529793/Desktop/ENMeval_citations.csv', 
#           row.names = F, fileEncoding='UTF-8')

tmp <- read.csv('/Users/au529793/Projects/GIT/ENMeval_lit_review/ENMeval_citations.csv')

# Read the lit review summary
url <- "https://docs.google.com/spreadsheets/d/1dlde-4E6kVMTKhFfU5N8d1YVkC1MV6W2s3OcKqZ_n64/edit?usp=sharing"

d <- as.data.frame(read_sheet(url))

tmp$doi <- unlist(lapply(strsplit(as.character(tmp$V1), 'doi:'), function(x)x[2]))
x <- unlist(lapply(strsplit(as.character(tmp$V1), ")"), function(x)x[1]))
tmp$year <- substring(x, nchar(x)-3, nchar(x))

d$year <- tmp$year[match(d$`Article DOI (copy from ENMeval citations spreadsheet)`, tmp$doi)]

d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1111/2041-210X.13107"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1029/2018GB005973"] <- 2019
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.5061/dryad.c699f"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1111/2041-210X.13142"] <- 2019
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1111/2041-210X.13142"] <- 2017
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1071/ZO18036"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1071/MF17100"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1017/S0959270916000241"] <- 2017
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.3996/082017-JFWM-067"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1017/S0959270916000241"] <- 2018
d$year[d$`Article DOI (copy from ENMeval citations spreadsheet)`=="10.1111/2041-210X.12945"] <- 2018


##################################################################
### Was ENMeval used for analysis or just cited for concepts?
table(d$`Was ENMeval used for analysis or just cited for concepts?`)

d_analysis <- d[d$`Was ENMeval used for analysis or just cited for concepts?`=='Used for analysis',]

table(d_analysis$`How was ENMeval used?`)

# Clean up
d_analysis$niche_overalp <- NA
d_analysis$niche_overalp <- 1*grepl("Scho", d_analysis$`How was ENMeval used?`)

d_analysis$partition <- NA
d_analysis$partition <- 1*grepl("partition", d_analysis$`How was ENMeval used?`)

d_analysis$evaluation <- NA
d_analysis$evaluation <- 1 * (grepl("evaluation", d_analysis$`How was ENMeval used?`) |
                                grepl("selection", d_analysis$`How was ENMeval used?`) |
                                grepl("Comparing the", d_analysis$`How was ENMeval used?`))
                                
table(d_analysis$evaluation)
100 * mean(d_analysis$evaluation)

# 93% of the studies used ENMeval for evaluating / selecting models

##################################################################
### Which evaluation metric(s) was used to select optimal settings

unique(d_analysis$"Which evaluation metric(s) was used to select optimal settings")

d_analysis$aicc <- NA
d_analysis$aicc <- 1*grepl("AICc", d_analysis$`Which evaluation metric(s) was used to select optimal settings`)

d_analysis$auc_test <- NA
d_analysis$auc_test <- 1*grepl("AUC test", d_analysis$`Which evaluation metric(s) was used to select optimal settings`)

d_analysis$OR <- NA
d_analysis$OR <- 1 * (grepl("Omission rates", d_analysis$`Which evaluation metric(s) was used to select optimal settings`) |
                        grepl("OR", d_analysis$`Which evaluation metric(s) was used to select optimal settings`))

d_analysis$AUCdiff <- NA
d_analysis$AUCdiff <- 1*grepl("AUCdiff", d_analysis$`Which evaluation metric(s) was used to select optimal settings`)

d_analysis$TSS <- NA
d_analysis$TSS <- 1*grepl("TSS", d_analysis$`Which evaluation metric(s) was used to select optimal settings`)

d_analysis$Boyce <- NA
d_analysis$Boyce <- 1*grepl("Boyce", d_analysis$`Which evaluation metric(s) was used to select optimal settings`)

# A figure for this?
b <- barplot(100 * (colSums(d_analysis[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')])/
                 nrow(d_analysis)), ylim=c(0,100))
axis(1, labels=F, at=b)

barplot(colSums(d_analysis[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')]))

d_analysis_list <- split(d_analysis, d_analysis$year)

dat <- do.call(rbind, 
               lapply(d_analysis_list, function(x) 
                 colSums(x[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')])))

cols <- rev(RColorBrewer::brewer.pal(5, 'Set1'))
barplot(dat, col=cols,
        ylim=c(0,110), ylab='Number of Studies')
legend('topright', bty='n', legend=2019:2015, 
       pt.bg=rev(cols), pch=22, pt.cex=2)



round(100 * apply(d_analysis[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')], 2, mean), 0)
# 70% of studies used AICc as the criterion for selecting optimal settings
# 21% of studies used AUCtest as the criterion for selecting optimal settings
# 11% of studies used OR as the criterion for selecting optimal settings

# How many studies use > 1 eval stat?
round(100*mean(rowSums(d_analysis[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')])>1),0)

# How many studies use > 2 eval stat?
round(100*mean(rowSums(d_analysis[,c('aicc','auc_test','OR','AUCdiff','TSS','Boyce')])>2),0)



##################################################################
### Were "optimal settings" reported?

table(d_analysis$'Were \"optimal settings\" reported?')
81/(81+49)
# 62% of studies reported optimal settings

table(d_analysis$'Was variation of performance across model parameters reported?')
34/(99+34)
# 26% of studies reported variation in performance across different model parameters

##################################################################
### How was data partitioned?

unique(d_analysis$'How was data partitioned?')

# Keeping it simple for now...  Just reporting on whether they used one of the 'spatial' partitioning methods or not

table(grepl('not', tolower(d_analysis$'How was data partitioned?')))
112/(112+29)

d_analysis2 <- d_analysis[!grepl('not', tolower(d_analysis$'How was data partitioned?')),]

table(grepl('spatial', tolower(d_analysis2$'How was data partitioned?')))
48/(48+64)

table(grepl('random', tolower(d_analysis2$'How was data partitioned?')))
49/(49+63)

table(grepl('jack', tolower(d_analysis2$'How was data partitioned?')))
16/(96+16)


# 35% of studies used one of the spatial partitioning methods
# 35% of studies used random partitioning
# 11% of studies used the jackknife partitioning

table(grepl('block', tolower(d_analysis2$'How was data partitioned?')))
31/(31+81)
# 28% used block 

table(grepl('checker', tolower(d_analysis2$'How was data partitioned?')))
18/(94+18)
# 16% used checkerboard

##################################################################
### Which evaluation metric(s) were reported?

unique(d_analysis$`Which evaluation metric(s) were reported?`)

aicc <- 1*grepl("aic", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
auctest <- 1*grepl("auc test", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
auctrain <- 1*grepl("auc train", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
aucdiff <- 1*grepl("aucdiff", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
or <- 1*grepl("omission", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
tss <- 1*grepl("tss", tolower(d_analysis$`Which evaluation metric(s) were reported?`))
boyce <- 1*grepl("boyce", tolower(d_analysis$`Which evaluation metric(s) were reported?`))

df <- data.frame(aicc, auctest, auctrain, aucdiff, or, tss, boyce)

## 80 % of studies that used ENMeval for analysis reported at least one evaluation statistic (113/141) 
sum(rowSums(df)>0)
mean(rowSums(df)>0)


sort(round(apply(df, 2, mean)*100), decreasing = T)

round(table(rowSums(df))/nrow(df)*100)


# Most common evaluation metric reported was AUC test (60% of studies) even though AIC used mostly for model comparison.  26% of studies reported Omission rates, 11% reported AUCtraining data and 11% tss. Difference between test and train AUC was reported in 7% of studies and 4% of studies reported the Boyce index.

