
###### Installing Packages ######
install.packages('tidyverse')
install.packages('GGally')
install.packages('ggplot2')
install.packages("ggcorrplot")

###### Loading Packages ######
library('tidyverse')
library('GGally')
library('ggplot2')
library("ggcorrplot")

###### Opening the databases ######
corona <- read.csv(file="Indexes.csv", head=TRUE, sep=",")

###### First Look ######
head(corona)
nrow(corona)
summary(corona)

###### TOTAL INFECTION RATIO CORRELATIONS ######

###### Correlation Education X Infection Ratio ######
educ_infec <- cor(corona$Education,corona$Infection.Ratio)
print(educ_infec)
###### Correlation Inequality X Death Ratio ######
inec_infec <- cor(corona$Inequality,corona$Infection.Ratio)
print(inec_infec)
###### Correlation % of GDP Expence on Health X Death Ratio ######
gdp_infec <- cor(corona$GDP.Health,corona$Infection.Ratio)
print(gdp_infec)
###### Correlation HDI X Death Ratio ######
hdi_infec <- cor(corona$HDI.Index,corona$Infection.Ratio)
print(hdi_infec)


###### TOTAL DEATH RATIO CORRELATIONS ######

###### Correlation Education X Death Ratio ######
educ_death <- cor(corona$Education,corona$Death.Ratio)
print(educ_death)
###### Correlation Inequality X Death Ratio ######
inec_death <- cor(corona$Inequality,corona$Death.Ratio)
print(inec_death)
###### Correlation % of GDP Expence on Health X Death Ratio ######
gdp_death <- cor(corona$GDP.Health,corona$Death.Ratio)
print(gdp_death)
###### Correlation HDI X Death Ratio ######
hdi_death <- cor(corona$HDI.Index,corona$Death.Ratio)
print(hdi_death)

###### Combining Results ######
Results <- rbind(educ_death,educ_infec,gdp_death,gdp_infec,hdi_death,hdi_infec,inec_death,inec_infec)
print(Results)

###### Saving Results into a File ######
write.csv(Results, file="Correlations.csv", row.names=TRUE)

###### Plot 1 ######
corona %>% ggpairs(columns = c('Education', 'Inequality', 'GDP.Health', 'HDI.Index','Death.Ratio', 'Infection.Ratio'),
                 upper = list(continuous = wrap('cor', size = 6)))

###### Plot 2 ######
correlations_plot <- select (corona,-c(Country,Population,HDI.Rank,Total.Cases,Total.Deaths))
correlations_plot <- cor(correlations_plot)
ggcorrplot(correlations_plot,
           type = 'lower',
           method = 'square',
           lab = TRUE,
           title = 'Correlation Coefficients')


