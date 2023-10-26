####Chapter 4###########
#####Bivariate Graphs###https://rkabacoff.github.io/datavis/Bivariate.html
data <- read.csv("UserAnswers_A279.csv", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
class(data)
View(data)

##get the columns names
colnames(data)

###drop####drop columns of the dataframe that are not of interest
data <- subset(data, select = -c(Article.Title, Resolve., Include, 
                          Exposure_Model, Title, Journal,Authors))
View(data)

###Rename multiple columns by name
library("dplyr")
data <- data %>% dplyr::rename("Programming_Statistical_analysis"  = "Programming...Statistical.analysis", 
  "scope_of_study"="scope.of.study", "Known_models"= "Known.models", "Exposure_Model_new" = "Exposure..Model...new", 
       "Analysis_Method" = "Analysis..Method", "Population_study" = "Population.study")
names(data)   

### Remove Row 669 as the Review was performed on a wrong pdf
data <- data[-c(669), ]
View(data)

### In many cases Reviewers used the word 'Other', when in the drop menu 
#was not available the suitable information and they could put in the correct information
###in the Notes column. We need to remove this word as the information is included by
##one of the other 2 Reviewers
data[data =="Other"] <- NA
data[data =="other"] <- NA
data[data =="not reported"] <- NA
###The word equation was a wrong input of the Menu in the Analysis method label at Sysrev, we have to remove
data[data == "equations"] <- NA
View(data)


###Information from the User.Note column is missing, and has to be combined with 
##the scope.of.study column, we combine the the two columns 
library(tidyverse)
data <- data %>% 
  unite(scope_of_study, c(User.Note, scope_of_study), sep = ",")
data[data ==","] <- NA
View(data)

#####fill with NAs the missing information
library(dplyr)
data <- data %>%
  mutate_at(c("Analysis_Method", "Chemical", "Exposure_Model_new", "Programming_Statistical_analysis",
              "Data", "Population_study", "Known_models", "scope_of_study",
              "Exposure_Route","Country", "Notes"), ~ na_if(., ''))
View(data)
###For each paper the reviewing process was performed twice by two different Reviewers. In the first
##round the information was set/add on the labels of data extraction project at Sysrev.
###However, in the second reviewing as the Reviewers were assisted by a data extraction
##programming tool, they were just checking if the data extracted were correct, and only 
###information that was missing was set/add. Therefore, the combined information of the two Reviewers
##is the correct and complete data extracted for each paper.
#The (User.Name = tom+ontox-api),preformed on the stage Full-text screening,
#contains important infor on Country, Exposure_route and Exposure_Model

###Combine the information of the three reviewers
##group by Article.ID and 
##https://stackoverflow.com/questions/69303052/concatenating-strings-rows-using-dplyr-group-by-with-mutate-or-summarize
library(dplyr)
data <- data %>% 
  group_by(Article.ID) %>% 
  summarise(across(everything(), ~ paste(., collapse = ";")))
View(data)

##########################
############Plot Year vs Analysis Methods################
######################

###Separate the year_of_study variable into 3 columns and coalesce the 
#the 3 columns to return the first non-missing value of the 3 
##https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas
library(tidyr)
data <- data %>% 
  separate(year_of_study, c('yearI', 'yearII', 'yearIII'))

###after the separate functions NAs are recognized as stings, we have to convert them back to NA
##to be able to user the coalesce function
data[data == "NA"] <- NA
View(data)

######We have to convert the ch variables of yearI, II, III to integer
data <- data %>% 
  mutate_at(c("yearI", "yearII", "yearIII"), as.integer)

#### In order to be able to coalesce and keep the fist non-missing value of the 3 columns
#https://www.r-bloggers.com/2020/12/demystifying-the-coalesce-function/
data$year_of_study <- coalesce(data$yearI, data$yearII, data$yearIII)
View(data)

###Separate the Reviewing info that some times is put and some times is approved only
#by the other Reviewers of the Analysis_Method column
##https://stackoverflow.com/questions/7069076/split-column-at-delimiter-in-data-frame
library(tidyr)
data <- separate(data=data, col = Analysis_Method, into = c("AnI", "AnII", "AnIII"), sep = ";")
View(data)

###after the separate functions NAs are recognized as stings, we have to convert them back to NA
##to be able to user the coalesce function
data[data == "NA"] <- NA
View(data)

####Coalesce the columns discarding NAs and keeping the strings that are not repeating by selecting the order of the coalesce
library(tidyverse)
library(dplyr)
data$AnI_AnII <- dplyr::coalesce(data$AnI, data$AnII)
data$Analysis_Method <- coalesce(data$AnIII, data$AnI_AnII)
View(data)

###Some of the information is mentioned twice e.g. deterministic,statistical 
## and statistical deterministic, so we need to make this being one term as category so we can group them
##replace multiple strings in a column of a data frame
#https://sparkbyexamples.com/r-programming/replace-string-with-another-string-in-r/
###Also, the deterministic, statistical means the exposure was done in a deterministic way
##and they include some statistics for correlation, so we correct this with deterministic


####The model simulation is not an analysis method. It will not be representative here to keep it
##for the graph. I need the journals that had prob., deter., and statistical analysis.
##I change the model simulations to deterministic, model simul., prob., to probabilistic etc.
###Usually, when using modelling tools, they aim for probabilistic studies


##### Replace String with Another String on the Known models column
library(dplyr)
data <- data %>% 
  mutate(Analysis_Method = dplyr::recode(Analysis_Method, "equations,deterministic" = "deterministic", 
                                 "deterministic,equations" = "deterministic",
                                 "deterministic,probabilistic,statistical" = "deterministic,probabilistic",
                             "deterministic,probabilistic,equations" = "deterministic,probabilistic",
                        "probabilistic,deterministic" = "deterministic,probabilistic",
        "probabilistic,deterministic,model simulations" = "deterministic,probabilistic",
        "deterministic,statistical" = "deterministic",
         "probabilistic,statistical" = "probabilistic",
        "probabilistic,statistical,model simulations" = "probabilistic",
        "statistical,deterministic" = "deterministic",
        "statistical,probabilistic" = "probabilistic",
      "probabilistic,model simulations" = "probabilistic",
  "deterministic,model simulations,probabilistic" = "deterministic,probabilistic",
  "model simulations,statistical" = "statistical",
  "model simulations,deterministic,probabilistic" = "deterministic,probabilistic",
  "model simulations,probabilistic" = "probabilistic",
  "model simulations" = "deterministic,probabilistic"))
View(data)


#### Visualizing the the Analysis_Method with years 
##Subset the Analysis_Method, year_of_study columns to arrange the strings
dataI <- data %>% 
  subset(select = c(Article.ID, Analysis_Method, year_of_study)) 
View(dataI)

#####Calculate the percentage within a group
#https://datacornering.com/calculate-the-percentage-by-a-group-in-r-dplyr/
library(dplyr)
library(formattable)
AnalMethod_Per<- dataI %>% 
  group_by(Analysis_Method) %>% 
  summarise(cnt = n()) %>% 
  mutate(freq_per = percent (round(cnt / sum(cnt), 2))) %>% #### relative freq. expressed in percentages
  arrange(desc(freq_per))
AnalMethod_Per

###How to Make Stunning Boxplots in R: A Complete Guide to ggplot Boxplot
###https://appsilon.com/ggplot2-boxplots/
####
#https://rkabacoff.github.io/datavis/Bivariate.html
#https://mq-software-carpentry.github.io/r-ggplot-extension/02-categorical-data/index.html
#https://datacarpentry.org/R-ecology-lesson/04-visualization-ggplot2.html
#https://rkabacoff.github.io/datavis/Time.html
library(tidyverse)
library(ggplot2)
Analysis_years_plot <- dataI %>%
  ggplot(aes(x= year_of_study, y = Analysis_Method, color = Analysis_Method)) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1982, 2022, 30), 
                     limits = c(1932, 2022)) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+
  labs(title = "Analysis Method vs Year of the study", subtitle = "Plot counts vs years", 
       caption = "ONTOX - Scoping review study",
       x = "Year", y = "Analysis Method")
Analysis_years_plot

####Attempt for line graphs
yearly_counts_graph <- dataI %>%
  count(year_of_study, Analysis_Method) %>%
  ggplot(aes(x = as.numeric(year_of_study), y = n, color = Analysis_Method)) +
  geom_point(size = 3.5) +
 # geom_line() +
  geom_smooth() +
  scale_x_continuous(breaks = seq(2000, 2022, 5), 
                     limits = c(2000, 2022)) +
  scale_y_continuous(breaks = seq(1, 10, 3), 
                     limits = c(1, 10)) +
  theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 17),
        axis.text.y = element_text(face = 'bold', size = 17),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Yearly number of Analysis methods", #subtitle = "Plot counts vs years", 
       #caption = "ONTOX - Scoping review study",
       x = "Years", y = "Counts of Analysis methods", color = "Analysis methods")
yearly_counts_graph


            ################
#### Plot Exposure route vs Analysis methods
##################
View(data)

##separate the Exposure_Route column
library(tidyr)
data <- separate(data=data, col = Exposure_Route, into = c("ExI", "ExII", "ExIII"), sep = ";")
View(data)

##Separating the Exposure_Route column ExI column has all the information mentioned
dataII <-  data %>% 
  subset(select = c(Article.ID, Analysis_Method, ExI)) 
View(dataII)

####We observe that ExI column has all the information for all the papers
####Fix the order of the Exposure route labels, input by every Reviewer, so we are able to group them
library(stringr)
rep_str<-c("Dermal,Inhalation,Oral" = "Oral,Dermal,Inhalation", 
           "Dermal,Oral" = "Oral,Dermal",
           "Dermal,Oral,Inhalation" = "Oral,Dermal,Inhalation",
           "Inhalation,Dermal" = "Dermal,Inhalation",
           "Inhalation,Dermal,Oral" = "Oral,Dermal,Inhalation",
           "Inhalation,Oral" = "Oral,Inhalation",
           "Inhalation,Oral,Dermal" = "Oral,Dermal,Inhalation",
           "Oral,Inhalation,Dermal" = "Oral,Dermal,Inhalation")
dataII$ExI <- str_replace_all(dataII$ExI, rep_str)
data$ExI<-str_replace_all(data$ExI, rep_str)
View(dataII)

###
#https://www.statology.org/ggplot2-legend-size/
#https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
#https://mq-software-carpentry.github.io/r-ggplot-extension/02-categorical-data/index.html
Exposure_analysis_plot <- dataII %>%
  ggplot(aes(x= Analysis_Method, fill = ExI)) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name="Exposure route") +
  stat_count(geom = "text", 
           aes(label = stat(count)),
           position=position_fill(vjust=0.5), colour="white") +
theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold', size = 15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Exposure route versus Analysis methods", 
       #caption = "ONTOX - Scoping review study",
       x = "Analysis Method", y = "Exposure route counts in proportion")
Exposure_analysis_plot 

                ##########################
###############Plot Exposure route vs Chemicals###########3
          ###########################
View(data)
View(as.data.frame(data$Chemical))

dataIII <- subset(data, select = c(Article.ID, Chemical, ExI, Exposure_Model_new))
View(dataIII)

##separate the data input by the 3 Reviewers, unite the data and then separate the multiple input Chemicals
#data per Reviewer
library(tidyr)
dataIII <- separate(data=dataIII, col = Chemical, into = c("ChI", "ChII", "ChIII"), sep = ";") %>% 
  unite(col = "ChemAll", c("ChI", "ChII", "ChIII"), sep = ",") %>% 
  separate(col = "ChemAll", into = c("C1","C2","C3","C4","C5", "C6", "C7", "C8"), sep = ",")
View(dataIII)

###Substitute "NA" and "Other" word with NA 
dataIII[dataIII == 'NA'] <- NA
dataIII[dataIII == 'Other'] <- NA
View(dataIII)

##In many studies more than one Chemicals is studied, so each column have more Chemical categories, 
#We pivot_longer to count each Chemical category separately per Analysis method
#https://dcl-wrangle.stanford.edu/pivot-advanced.html
###
#pivot the data frame into a long format and removerows with NA's using drop_na()
dataIII <- dataIII %>% 
  pivot_longer(cols=c("C1","C2","C3","C4","C5", "C6", "C7", "C8"),
               names_to='ChemicalC',
               values_to='Chemicals')
View(dataIII)

#Remove duplicates on selected columns, recode empty stings by NAs and remove them
#Remove leading and trailing white space on each row
dataIII <- dataIII %>% 
  distinct(Article.ID, Chemicals, .keep_all = TRUE) %>% 
  na.omit() %>% 
  mutate_all(str_trim)
View(dataIII)

####Fix the order of the Exposure route labels, so we are able to group them
library(stringr)
rep_str<-c( "Bisphenols,Non-persistent" = "Bisphenols",
            "Cosmetic ingredients,Non-persistent" = "Cosmetic ingredients",
            "VOCs,Other" = "VOCs",
            "Cosmetic ingredients,Other" = "Cosmetic ingredients",
            "Cosmetic ingredients,Persistent" = "Cosmetic ingredients",
            "Metals,Other" = "Metals","Other,Metals" = "Metals",
            "VOCs,Other" = "VOCs","Other,VOCs" = "VOCs",
            "Non-persistent,Other" = "Non-persistent","Other,Non-persistent" = "Non-persistent",
            "Pesticide,Non-persistent,Persistent" = "Pesticide",
            "VOCs,Phthalates,Other" ="VOCs,Phthalates","Other,Pesticide" = "Pesticide",
            "Metals,Mycotoxins,Persistent" = "Metals,Mycotoxins",
            "Metals,Pesticide,Persistent" = "Metals,Pesticide",
            "Metals,Persistent,Non-persistent,VOCs" = "Metals,VOCs",
            "Persistent,PFASs" = "PFASs","PFASs,Persistent" = "PFASs",
            "Pesticide,Metals" = "Metals,Pesticide",
            "Persistent,Metals" = "Metals","Persistent,VOCs" = "VOCs",
            "Pesticide,Persistent" = "Pesticide",
            "Cosmetic ingredients,PFASs,Phthalates,Persistent" = "Cosmetic ingredients,Phthalates",
            "Phthalates,Cosmetic ingredients" = "Cosmetic ingredients,Phthalates",
            "Pesticide,VOCs,Cosmetic ingredients" = "Cosmetic ingredients,Pesticide,VOCs",
            "VOCs" = "Volatile Org. Compounds", 
            "Pesticide" = "Pesticides", 
            "Food additive" = "Food additives")
dataIII$Chemicals <- str_replace_all(dataIII$Chemicals, rep_str)
View(dataIII)

###Percentage of Chemicals
library(formattable)
Chemicals_Per<- dataIII %>% 
  group_by(Chemicals) %>% 
  summarise(cnt = n()) %>% 
  mutate(freq_per = percent (round(cnt / sum(cnt), 3))) %>% #### relative freq. expressed in percentages
  arrange(desc(freq_per))
View(as.data.frame(Chemicals_Per))


#######Plot count number of chemical classes
Chemicals_count <- dataIII %>%
  group_by(Chemicals) %>% 
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(Chemicals, n), y = n)) +
  geom_bar(stat = "identity", color = "blue", fill = 'orange') +
  coord_flip() +
  geom_text(aes(label = n), 
            hjust = -.2) +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 18),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 12), #change the facets font
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Types of study design", 
       #caption = "ONTOX - Scoping review study",
       x = "Chemical classes", y = "Count of chemical classes")
Chemicals_count



##############Plot Exposure route vs Chemical
### Plot 
ExRoute_ChemicalI<- dataIII %>% 
  group_by(Chemicals) %>% 
  count(ExI, Chemicals) %>% 
ggplot(aes(x= ExI, y = n, fill = Chemicals)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(title = "Chemicals versus Exposure route", 
       caption = "ONTOX - Scoping review study",
       x = "Exposure route", y = "Count of chemicals")
ExRoute_ChemicalI

View(dataIII)
######
######Plotting with faceting
#https://sscc.wisc.edu/sscc/pubs/dvr/two-variables.html#one-discrete-one-continuous
#https://rkabacoff.github.io/datavis/Bivariate.html
#####
Ch_Exp_P <- dataIII %>% 
  group_by(Chemicals, ExI) %>% 
  summarize(n = n()) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = Chemicals, y = perc, fill = ExI)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Exposure route") +
  #geom_col(show.legend = F) +
  facet_grid( ~ ExI, labeller = label_wrap_gen(width=20)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, 50))+
 # ylim(c(0, 116, 25)) +
  geom_text(aes(label = round(perc, 0)), size = 6, hjust = -.2) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(face = 'bold', size = 25),
        strip.background = element_rect(fill="white", colour="black",size=1))  +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold', size = 19),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 17),
        strip.text.x = element_text(size = 12), #change the facets font
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Chemicals versus Exposure route", 
       #caption = "ONTOX - Scoping review study",
        y = "percentage (%)", x = "Chemical classes")
Ch_Exp_P


#############Estimation Methodologies versus Chemicals########
### data frame with the Estimation methodologies and Chemicals column
View(dataIII)

##separate the Exposure_Model_new column
dataIII<-separate(data =dataIII, col = Exposure_Model_new, into = c("MI", "MII","MIII"), sep = ";")
View(dataIII)

###Replace character NA values  with NA 
dataIII <- dataIII %>%
  mutate_at(c("MI", "MII","MIII"), ~ na_if(., 'NA'))
View(dataIII)

###coalesce the columns
dataIII$Est_MI <- dplyr::coalesce(dataIII$MI, dataIII$MII)
dataIII$Est_M <- dplyr::coalesce(dataIII$Est_MI, dataIII$MIII)
View(dataIII)


###Change the order of the Estimation Methodologies column in order to group  them
##The important information is that the study was using a Toolbox and not the additional
##information that in the journal a mathematical model,or a script/software was included

##### Replace String with Another String on the Known models column
library(dplyr)
dataIII <- dataIII %>% 
  mutate(Est_M = dplyr::recode(Est_M, "Script/Software,Mathematical model" = "Mathematical model",
            "Script/Software,Empirical equations" = "Empirical equations",
            "Script/Software,Toolbox" = "Toolbox/Software",
            "Toolbox,Script/Software" = "Toolbox/Software",
            "Toolbox" = "Toolbox/Software",
            "Mathematical model,Toolbox" = "Toolbox/Software",
            "Empirical equations,Script/Software" = "Empirical equations",
            "Mathematical model,Script/Software" = "Mathematical model",
            "Script/Software" = "Toolbox/Software"))
View(dataIII)


###Plot the Estimation Methodologies used for various Chemicals
EstMeth_Chem <- dataIII %>% 
  ggplot(aes(x= Est_M, fill = Chemicals)) +
  geom_bar() +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold', size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 15))+
  labs(title = "Estimatation Methodologies versus Chemicals", 
       caption = "ONTOX - Scoping review study",
       x = "Estimatation Methodologies", y = "Count of chemicals")
EstMeth_Chem

###Percent of Estimation methodologies
library(formattable)
AnalMethod_PerI<- dataIII %>% 
  group_by(Est_M) %>% 
  summarise(cnt = n()) %>% 
  mutate(freq_per = percent (round(cnt / sum(cnt), 2))) %>% #### relative freq. expressed in percentages
  arrange(desc(freq_per))


####reoder the count
#https://juliasilge.com/blog/reorder-within/
library(forcats)
library(tidytext)
library(tidyverse)
library(tidyr)
EstMeth_ChemI <- dataIII  %>% 
  group_by(Est_M, Chemicals) %>% 
  summarize(n = n()) %>%
  mutate(Est_M = as.factor(Est_M), Chemicals = reorder_within(Chemicals, n, Est_M)) %>% 
  ggplot (aes(x = Chemicals, y = n, fill = Est_M)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Est_M, scales = "free_y", labeller = label_wrap_gen(width=10)) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_discrete(name="Exposure route") +
  ylim(c(0, 34)) +
  geom_text(aes(label = round(n, 1)), hjust = -.1) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(face = 'bold', size = 6))  +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(face = 'bold', size = 13),
        axis.text.y = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        strip.text.x = element_text(size = 17), #change the facets font
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 20))+
  labs(# title = "Estimatation Methodologies versus Chemicals", 
      # caption = "ONTOX - Scoping review study",
        y = "Count of chemical classes", x = "Chemicals classes")
EstMeth_ChemI 

###Since there is only 1 study for Disinfecting chemicals, Antimicrobial chemicals and
###2 studies for Pharmaceuticals we will exclude this chemical classes from our data
View(dataIII)
dataIIIa <- dataIII %>% 
  filter(Chemicals != "Antimicrobial chemicals" & Chemicals != "Nicotine (e-cigarettes)" & Chemicals != "Pharmaceuticals")
View(dataIIIa)

######
######Plotting with faceting
#https://sscc.wisc.edu/sscc/pubs/dvr/two-variables.html
#https://rkabacoff.github.io/datavis/Multivariate.html#Faceting
#####
###https://stackoverflow.com/questions/40211451/geom-text-how-to-position-the-text-on-bar-as-i-want
Est_Chem_P <- dataIIIa %>% 
  group_by(Chemicals, Est_M) %>% 
  summarize(n = n()) %>%
  mutate(perc = round(100*n/sum(n))) %>%
  mutate(Est_M = as.factor(Est_M)) %>% 
  ggplot() +
  scale_fill_brewer(palette = "Set2") +
  geom_bar(aes(x = reorder_within(Chemicals, perc, Est_M),  
               y = perc, fill = Est_M), stat = "identity", show.legend = FALSE) +
  geom_text(aes(x = reorder_within(Chemicals, perc, Est_M), y = perc,
                label = round(perc, 1)), size =6, hjust = -.1,
                position = position_dodge(width = 1),
                          inherit.aes = TRUE) +
  facet_wrap(~ Est_M, scales = "free_y", labeller = label_wrap_gen(width=10)) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 90)) +
 # ylim(c(0, 134)) +
  theme_bw() +
  theme(strip.text.x = element_text(face = 'bold', size = 17),
        strip.background = element_rect(fill="white", colour="black",size=1)) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 17),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 20))+
  labs(#title = "Estimatation Methodologies versus Chemicals", 
       # caption = "ONTOX - Scoping review study", 
       x = "Chemical classes", y = "percentage (%)")
Est_Chem_P

#############
####Population analysis study####
###############################
#https://www.andrewheiss.com/blog/2022/06/23/long-labels-ggplot/
View(data)
###subset the column of interest
dataP <-  data %>% 
  subset(select = c( Article.ID, Population_study)) 
View(dataP)

###separate the column to the information put from the 2 reviewers
dataP<-separate(data =dataP, col = Population_study, into = c("PI", "PII","PIII"), sep = ";")
View(dataP)

###Convert NA strings to NAs and use the coalesce function
dataP[dataP == "NA"] <- NA
View(dataP)

dataP$PI_PII <- dplyr::coalesce(dataP$PI, dataP$PII)
dataP$Pop_Anal <- coalesce(dataP$PI_PII, dataP$PIII)
View(dataP)

###Change the order of the Population study column in order to group  them
library(stringr)
rep_str<-c( "cohort,population analysis" = "population analysis,cohort",
            "individual,population analysis" = "population analysis,individual",
            "age groups,cohort" = "cohort,age groups",
            "population analysis,age groups,individual" = "population analysis,age groups",
            "age groups,population analysis" = "population analysis,age groups",
            "individual,age groups" = "age groups,individual",
            "individual,cohort" = "cohort,individual")
dataP$Pop_Anal <- str_replace_all(dataP$Pop_Anal, rep_str)
View(dataP)

###Plot count number of studies for each study design
Pop_anal_plot <- dataP %>%
  group_by(Pop_Anal) %>% 
  summarize(n = n()) %>%
  mutate(perc = (round(100*n/sum(n), 1))) %>%
 # count() %>% 
  ggplot(aes(x = reorder(Pop_Anal, +perc), y = perc)) +
  geom_bar(stat = "identity", color = "darkblue", fill = 'lightblue') +
  coord_flip() +
  geom_text(aes(label = perc), 
            hjust = -.2) +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        strip.text.x = element_text(size = 12), #change the facets font
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Types of study design", 
       #caption = "ONTOX - Scoping review study",
       x = "Types of study design", y = "percentage (%)")
Pop_anal_plot

#############
####Scope of study ####
###############################
View(data)
###subset the column of interest
dataS <-  data %>% 
  subset(select = c( Article.ID, scope_of_study)) 
View(dataS)

###separate the column to the information put from the 2 reviewers
dataS<-separate(data =dataS, col = scope_of_study, into = c("SI", "SII","SIII"), sep = ";")
View(dataS)

#library(stringr)
##remove the "," symbol from the strings in the SI,SII,SIII columns
#dataS <- apply(dataS, 2, function(x) str_remove_all(x,"[,]"))

###Substitute "NA" with NA
dataS[dataS == 'NA'] <- NA
View(dataS)

###coalesce the columns
dataS$SI_SII <- dplyr::coalesce(dataS$SI, dataS$SII)
View(dataS)

###Unite and separate to  set one information per Article.ID
dataS <- dataS %>%  
  unite(col = "Sc", c("SI_SII", "SIII"), sep = ",") %>% 
  separate(col = "Sc", into = c("S1","S2","S3","S4", "S5"), sep = ",")
View(dataS)

###Substitute "NA" and "other" word with NA 
dataS[dataS == 'NA'] <- NA
dataS[dataS == ''] <- NA
View(dataS)

dataS1 <- dataS %>% 
  pivot_longer(cols=c("S1","S2","S3","S4"),
               names_to="Scs",
               values_to="Scope") 
View(dataS1)

#Remove duplicates on selected columns, recode empty stings by NAs and remove them 
dataS1 <- dataS1 %>% 
  distinct(Article.ID, Scope, .keep_all = TRUE)
View(dataS1)

###subset the column of interest, remove leading and trailing white space on each row
dataS1 <-  dataS1 %>% 
  subset(select = c( Article.ID, Scope)) %>% 
  na.omit()%>% 
 mutate_all(str_trim)
View(dataS1)


##### Replace String with Another String on the Known models column
library(dplyr)
dataS1 <- dataS1 %>% 
  mutate(Scope = dplyr::recode(Scope, "human exposure (HE) estimates only" = "Human exposure estimates",
                  "human exposure (HE) estimates onlyNA" = "Human exposure estimates",
             "HE associated with risk assessment"= "Human exposure and\nrisk assessment",
        "HE associated with risk assessmentNA"= "Human exposure and\nrisk assessment",
             "HE associated with other health outcomes" = "Human exposure and\nhealth outcomes",
                 "HE associated with liver toxicity" = "Human exposure and\nliver toxicity",
            "HE associated with brain toxicity" = "Human exposure and\nbrain toxicity",
              "HE associated with kidney toxicity" = "Human exposure and\nkidney toxicity",
        "HE associated with kidneys toxicity" = "Human exposure and\nkidney toxicity",
                       "validation biomonitoring" = "Human exposure and\nbiomonitoring validation",
          "Validation againist  DEEM-FCID Version 3.16 software used by EPA USA" = "Human exposure and\nbiomonitoring validation"))
View(dataS1)

##Plot percentage of studies per scope of the paper
Scope_plot <- dataS1 %>% 
  group_by(Scope) %>% 
  summarize(n = n()) %>%
  mutate(perc = (round(100*n/sum(n), 0))) %>% 
  ggplot(aes(x = reorder(Scope, +perc), y = perc)) + 
  coord_flip()+
  geom_text(aes(label = perc), 
            hjust = -.2) +
  geom_bar(stat = "identity", color ="blue", fill = 'orange')+
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 14),
        axis.text.y = element_text(face = 'bold', size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Scope of the papers", 
       #caption = "ONTOX - Scoping review study",
       x="Scope of study", y = "percentage (%)")
Scope_plot

###########
#####The Notes column contains a combination of data from the other data label columns.
###When the information was not offered by the drop menu at the Sysrev webplatform,
##the Reviewers had to write down on the Notes column, the name of the data label column and the information 
View(data)

###subset the column of interest
dataN <-  data %>% 
  subset(select = c( Article.ID, Notes)) 
View(dataN)

###separate each row of the Notes column at the symbol ';' , put in from the 3 reviewers
dataN<-separate(data =dataN, col = Notes, into = c("NI", "NII","NIII", "NIV", "NV", "NVI"), sep = ";")
View(dataN)

###Substitute "NA" with NA
dataN[dataN == 'NA'] <- NA
View(dataN)

###As we combined the information from 3 Reviewers, and Reviewers are writing the information
##there were different empty spaces for each string
##in each row. We remove leading and trailing white space on each row
dataN <-dataN %>% 
  mutate_all(str_trim)
View(dataN)


###Separate the all the columns to Names and Values
library(tidyr)
dataN <-separate(data =dataN, col = NI, into = c("NamesI", "ValuesI"), sep = ":")
dataN <-separate(data =dataN, col = NII, into = c("NamesII", "ValuesII"), sep = ":")
dataN <-separate(data =dataN, col = NIII, into = c("NamesIII", "ValuesIII"), sep = ":")
dataN <-separate(data =dataN, col = NIV, into = c("NamesIV", "ValuesIV"), sep = ":")
dataN <-separate(data =dataN, col = NV, into = c("NamesV", "ValuesV"), sep = ":")
dataN <-separate(data =dataN, col = NVI, into = c("NamesVI", "ValuesVI"), sep = ":")
View(dataN)


####pivot_wider the NI column
library(tidyr)
dataNI<- dataN %>% 
  subset(select = c( Article.ID, NamesI, ValuesI)) %>% 
  pivot_wider(names_from = NamesI, values_from = ValuesI) %>% 
  select(-'NA')
View(dataNI)

##pivot_wider the NII column
dataNII<- dataN %>% 
  subset(select = c( Article.ID, NamesII, ValuesII)) %>% 
  pivot_wider(names_from = NamesII, values_from = ValuesII)%>% 
  select(-'NA')
View(dataNII)
###

##pivot_wider the NIII column
dataNIII<- dataN %>% 
  subset(select = c( Article.ID, NamesIII, ValuesIII)) %>% 
  pivot_wider(names_from = NamesIII, values_from = ValuesIII)%>% 
  select(-'NA')
View(dataNIII)

#######pivot_wider the NIV column
dataNIV<- dataN %>% 
  subset(select = c( Article.ID, NamesIV, ValuesIV)) %>% 
  pivot_wider(names_from = NamesIV, values_from = ValuesIV)%>% 
  select(-'NA')
View(dataNIV)

#######pivot_wider the NV column
dataNV<- dataN %>% 
  subset(select = c( Article.ID, NamesV, ValuesV)) %>% 
  pivot_wider(names_from = NamesV, values_from = ValuesV)%>% 
  select(-'NA')
View(dataNV)

#######pivot_wider the NVI column
dataNVI<- dataN %>% 
  subset(select = c( Article.ID, NamesVI, ValuesVI)) %>% 
  pivot_wider(names_from = NamesVI, values_from = ValuesVI)%>% 
  select(-'NA')
View(dataNVI)

####Create a new dataframe by joining the main dataframe with the Known_models,Data filled by the menu provided by Sysrev, and the 
###the dataframes from the Notes column when the information is not in the Sysrev menus
data_Mod_Dat <- data %>% 
  subset(select = c(Article.ID, Known_models, Data))
View(data_Mod_Dat)

###Using the separate function the columns have transformed into characters, 
##the Article.ID column has become a character, while remains an interger at the data_Mod_Dat df
##we need to convert it to be able to join them with dataNI...dataNVI
data_Mod_Dat$Article.ID <- as.character(data_Mod_Dat$Article.ID)    
class(data_Mod_Dat$Article.ID)

##### Join multiple data.frames
list_df = list(data_Mod_Dat,dataNI,dataNII,dataNIII,dataNIV,dataNV,dataNVI)
df_Tot <- list_df %>% reduce(full_join, by='Article.ID')
View(df_Tot)


##Unite all the columns with the Known_models data into a total info column Known_modelsT 
#and separate to clean from NA and duplicated values
library(tidyr)
df_Tot <- df_Tot %>%  
unite(col = "Known_modelsT", c("Known_models", "Known models.x", "Known models.y", "Known models.x.x", "Known models.y.y"),
      sep = ";") %>% 
  separate(col = "Known_modelsT", into = c("Km1","Km2","Km3","Km4","Km5","Km6","Km7"), sep = ";")
View(df_Tot)

###Substitute "NA" and "other" word with NA 
df_Tot[df_Tot == 'NA'] <- NA
View(df_Tot)

##### Unite all the information while removing NAs
library(tidyr)
df_Totfm <- df_Tot %>%  
  unite(col = "Known_modT", c("Km1","Km2","Km3","Km4","Km5","Km6","Km7"), sep = ",", na.rm = TRUE) %>% 
  separate(col = "Known_modT", into = c("K1","K2","K3","K4","K5","K6","K7"), sep = ",") 
View(df_Totfm)

df_Totfm[df_Totfm == "other"] <- NA

df_Totfm1 <- df_Totfm %>% 
  subset(select = c("Article.ID","K1","K2","K3","K4","K5","K6","K7"))
View(df_Totfm1)

df_Totfm2 <- df_Totfm1 %>% 
  pivot_longer(cols=c("K1","K2","K3","K4","K5","K6","K7"),
               names_to="Ks",
               values_to="Knownmod") %>%
                drop_na()
View(df_Totfm2)

#Remove duplicates on selected columns, recode empty stings by NAs and remove them 
##We remove leading and trailing white space on each row
df_Totfm3 <- df_Totfm2 %>% 
  distinct(Article.ID,Knownmod, .keep_all = TRUE) %>% 
  mutate_at("Knownmod", ~ na_if(.,'')) %>% 
  na.omit()%>% 
  mutate_all(str_trim)
View(df_Totfm3)

###Remove the row 75 as it contains repeated information 
df_Totfm3 <- df_Totfm3[-c(75), ]
View(df_Totfm3)


##### Replace String with Another String on the Known models column
library(dplyr)
df_Totfm3 <- df_Totfm3 %>% 
  mutate( Knownmod = dplyr::recode( Knownmod, 'PRIMo (EFSA)' = 'Pesticide Residue Intake Model (PRIMo) EFSA', 'PRIMo' = 'Pesticide Residue Intake Model (PRIMo) EFSA',
                                    'PRIMo (Pesticide Residue Intake Model)' = 'Pesticide Residue Intake Model (PRIMo) EFSA',
                                    'Cares' = 'Cumulative and Aggregate Risk Evaluation System (Cares) USEPA', 'CARES' = 'Cumulative and Aggregate Risk Evaluation System (Cares) USEPA', 
                                    'CalTox' = 'Multimedia transport & transform. model (CalTOX) USEPA', 'Caltox' = 'Multimedia transport & transform. model (CalTOX) USEPA', 
                                    'CalTOX' = 'Multimedia transport & transform. model (CalTOX) USEPA',
                                    'PRoTEGE' = 'PRoTEGE (Prioritiz./Ranking Toxic Exposures, GIS Ext.) eohsi US',
                                    'Prioritization/Ranking of Toxic Exposures with GIS Extension (PRoTEGE)' = 'PRoTEGE (Prioritiz./Ranking Toxic Exposures, GIS Ext.) eohsi US',
                                    'Kinetic Dietary Exposure Model' = 'Kinetic Dietary Exposure Model (KDEM) INRA – Met@risk FR',
                                    'Kinetic Dietary Exposure Model (KDEM)' = 'Kinetic Dietary Exposure Model (KDEM) INRA – Met@risk FR',
                                    'Creme Food Safety' = 'Creme Food Safety® software Creme global',
                                    'Creme Food Safety® Software' = 'Creme Food Safety® software Creme global',
                                    'Creme RIFM model' = 'Creme RIFM Exposure Model Creme global',
                                    'Creme RIFM Exposure Model' = 'Creme RIFM Exposure Model Creme global',
                                    'FACET  (Flavours' = 'Flavours Additives and Food Contact Materials Exp. Task FACET FSA UK)',
                                    'SHEDS-HT' = 'High throughput Stoch. Human Exp. Dose Simul. model (SHEDS-HT) USEPA',
                                    'High throughput (HT) Stochastic Human Exposure and Dose Simulation model (SHEDS-HT)' = 'High throughput Stoch. Human Exp. Dose Simul. model (SHEDS-HT) USEPA',
                                    'SHEDS-PM' = 'Population Exposure model (SHEDS-PM)', 'ExpoFIRST' = 'Exposure Factors Interactive Resource Scenarios Tool (ExpoFIRST) USEPA',
                                    'Exposure Factors Interactive Resource for Scenarios Tool (ExpoFIRST)' = 'Exposure Factors Interactive Resource Scenarios Tool (ExpoFIRST) USEPA',
                                    'PACEM' = 'Probabilistic Aggreg. Consm. Exp. Model (PACEM) RIVM',
                                    'Dermal (PACEM-KD)' = 'Dermal Kinetic Model (PACEM-KD) RIVM',
                                    'Probabilistic Aggregated Consm. Exposure Model–Kinetic' = 'Dermal Kinetic Model (PACEM-KD) RIVM',
                                    'ECETOC TRA' = 'Targeted Risk Assessment tool (Ecetoc TRA)',
                                    'Ecetoc Targeted Risk Assessment (Ecetoc TRA)' = 'Targeted Risk Assessment tool (Ecetoc TRA)',
                                    'Sprayexpo' = 'SprayExpo Fraunhofer ITEM',
                                    'SprayExpo' = 'SprayExpo Fraunhofer ITEM',
                                    'Exposure Forecasting (ExpoCast)' = 'Exposure Forecasting (ExpoCast) USEPA', 'ExpoCast EPA' = 'Exposure Forecasting (ExpoCast) USEPA',
                                    'European Solvents Industry Group (ESIG) Generic Exposure Scenario (GES) Risk and Exposure Tool (EGRET)' = ' Risk and Exposure Tool (EGRET) ESIG/EU',
                                    'EGRET' = 'Risk and Exposure Tool (EGRET) ESIG/EU',
                                    'USEtox' = 'Environment Program (USEtox) model UN','Hazardous Air Pollutant Exposure Model (HAPEM)' = 'Hazardous Air Pollutant Exposure Model (HAPEM) USEPA',
                                    'HAPEM or Hazardous Air Pollutant Exposure Model' = 'Hazardous Air Pollutant Exposure Model (HAPEM) USEPA',
                                    'The RAIDAR-Indoor and Consumer Exposure' = 'Risk Assess. Ident. And Ranking model (RAIDAR) cemc-TrentU CA',
                                    'Multimedia Exposure Assessment Model by USEPA' = 'Multimedia Exp. Assessment Model (MULTIMED) USEPA', 
                                    'EFAST-CEM' = 'Exposure & Fate Assess. Screening Tool (EFAST) USEPA',
                                    'Exposure and Fate Assessment Screening Tool (EFASTv2)' = 'Exposure & Fate Assess. Screening Tool (EFAST) USEPA',
                                    'RACE' = 'Rapid Assessment of Contaminant Exposure (RACE) EFSA',
                                    'FEIM' = 'Food Enzyme Intake Model (FEIM) EFSA', 
                                    'FAIM' = 'Food Additives Intake Model (FAIM) EFSA',
                                    'FACE' = 'Feed Additive Consumer Exposure (FACE) EFSA', 
                                    'REACT' = 'Reach Exposure Assessment Consumer Tool (REACT) AISE',
                                    'ABICAM' = 'Activity-Based Indoor Chemical Assessment Model (ABICAM) USEPA',
                                    'Pangea' = 'Multimedia fate and exposure assessment model (Pangea) SPH US',
                                    "Qsar(for risk assessment)" = "Open Qsar App (OPERA) NICEATM/USEPA",
                                    "OPERA" = "Open Qsar App (OPERA) NICEATM/USEPA",
                                    "ExDoM" = "Exposure Dose Model (ExDoM) Dept. Environmental Engg. UoC",
                                    "DERMWIN" = "Dermal permeability modeling program (DERMWIN) USEPA",
                                    "Combined Environmental Stressors' Exposure (CENSE) tool" = "Combined Environmental Stressors' Exp. (CENSE) tool AUTH",
                                    "Browse model" = "Bystanders Residents Operators WorkerS Exp. model BROWSE ssau UK",
                                    "Mentor software" = "Mentor tool eohsi US",
                                    "GExFRAME/Scibin" = "GExFRAME/Scibin EC US",
                                    'ArcGIS software' = 'ArcGIS software Esri',
                                    'BREAM (Bystander and Resident Exposure Assessment Model)' = 'Bystander and Resident Exposure Assess. Model (BREAM) fera UK',
                                    'VLIER-HUMAAN' = 'VLIER-HUMAAN Vito BE',
                                    'Far-field Human Exposure (FHX)' = 'Far-field Human Exposure (FHX) cemc-TrentU CA',
                                    'ICECRM' = 'Indoor chemical exp. classif./ranking model (ICECRM) UToronto CA',
                                    'IMPACT 2002' = "IMPACT 2002 epfl CH",
                                    'Modul’ERS tool' = 'Modul’ERS tool ineris FR',
                                    'INDAIR/EXPAIR' ='INDAIR/EXPAIR sei UK',
                                    'Transient Transport through the epiDERMis (TTDERM)' = 'Transient Transport through epiDERMis (TTDERM) lbl US',
                                    'PROduction-To-EXposure  (PROTEX)' = 'PROduction-To-EXposure  (PROTEX) UToronto CA',
                                    'positive matrix factorization (PMF)' = 'positive matrix factorization (PMF) USEPA',
                                    'personal delivered dose (PDD) model' = 'personal delivered dose (PDD) model BU SPH US',
                                    'INTEGRA web platform' = 'INTEGRA web platform ugl AU',
                                    'Consumer Product Exposure and Risk assessment system (COPER)' = 'Consumer Product Exp. and Risk assess. (COPER) Yoon Seong-kyu KR',
                                    'Exposure Potential Model (DEPM)' = 'Exposure Potential Model (DEPM) USEPA',
                                    'ACC-HUMAN' = 'ACC-HUMAN itm su SE',
                                    'Calendex EPA' = 'Calendar-based multi-pathway exp. model (DEEM-FCID/Calendex) USEPA',
                                    'IEUBK EPA' = "Integrated Exposure Uptake Biokinetic IEUBK USEPA",
                                    'ConsExpo RIVM' = 'Consumer Exposure & Uptake Model (ConsExpo) RIVM',
                                    'MCRA RIVM/EFSA' = 'Monte Carlo Risk Assessment (MCRA) RIVM/EFSA',
                                    'MERLIN-Expo LIFE-VERMEER project Italy' ='Modelling Exp. to chemic. for Risk assess. (MERLIN-Expo) EU',
                                    'SHEDS-multimedia EPA' = 'Stoch. Human Exp. Dose Simul. model (SHEDS-multimedia) USEPA'))
View(df_Totfm3) 

####The Creme food global is a commercial software, that is we have to buy
###Remove the row 75 as it contains repeated information 
df_Totfm3 <- df_Totfm3[df_Totfm3$Knownmod != 'Creme Food Safety® software Creme global', ] 
df_Totfm3 <- df_Totfm3[df_Totfm3$Knownmod != 'Creme RIFM Exposure Model Creme global', ] 
df_Totfm3 <- df_Totfm3 %>% 
  mutate_all(str_trim)
View(df_Totfm3)



#####Calculate the percentage within a group, as I will filter the most frequent Known models
###to be able to visualize more variables
library(formattable)
KnownModels_Per<- df_Totfm3 %>%
  mutate_all(str_trim) %>% 
  group_by(Knownmod) %>% 
  summarise(cnt = n()) %>% 
  na.omit() %>% 
  mutate(freq_per = formattable::percent(round(cnt / sum(cnt), 3))) %>% #### relative frequency expressed in percentages
  arrange(desc(freq_per))
View(as.data.frame(KnownModels_Per))



##Plot percentage of known model and tools  of the papers
Models_Tools_Perc <- df_Totfm3 %>% 
  group_by(Knownmod) %>% 
  summarize(n = n()) %>%
  mutate(perc = (round(100*n/sum(n), 1))) %>% 
  ggplot(aes(x = reorder(Knownmod, +perc), y = perc)) + 
  coord_flip()+
  scale_y_continuous(limits = c(0, 14)) +
  geom_text(aes(label = perc), size = 5,
            hjust = -.2) +
  geom_bar(stat = "identity", color ="blue", fill = 'orange')+
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(x="Mathematical models/toolboxes", y = "percentage (%)")
Models_Tools_Perc 



#### dataIV contains clean the Exposure route, Chemicals and Estimated methodologies data columns 
View(dataIII)
###we tranform into character so we can join the dataframes
dataIII$Article.ID <- as.character(dataIII$Article.ID)
class(dataIII$Article.ID)

##We subset the Exposure route, Chemicals and Estimated methodologies columns that have been cleaned
dataIV_m <- dataIII %>% 
  subset(select = c(Article.ID, ExI, Chemicals, Est_M))
View(dataIV_m)

##Join the df df_Totfm3 that contain clean the Known models data
##and dataIV_m that contains Chemicals, Exposure_route and Est_m data
df_Models <- full_join(df_Totfm3, dataIV_m, by = "Article.ID", multiple = "all", relationship = "many-to-many")
View(df_Models)

df_Models <- df_Models %>% 
  mutate_all(str_trim)
View(df_Models)

#####
########Plot Known models versus Exposure route
##############
#Remove duplicates on selected columns, remove NAs, goup by Knownmod 
###Plot Known models versus Exposure route
Models_ExpRoute<- df_Models %>% 
  distinct(Article.ID,Knownmod,ExI, .keep_all = TRUE) %>% 
  na.omit() %>%
  group_by(Knownmod) %>% 
  count(ExI, Knownmod) %>% 
  ggplot(aes(y= Knownmod, x=n, fill= ExI)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Exposure route") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(face = 'bold', size = 11),
        axis.text.y = element_text(face = 'bold', size = 9),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        #strip.text.x = element_text(size = 12), #change the facets font
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(title = "Known models versus Exposure route", 
       caption = "ONTOX - Scoping review study",
       x = "Counts of Exposure route", y = "Known models")
Models_ExpRoute


###########
####The models are too many, I filter the most frequent used models and I try 
###to visualize with more variables in order to combine information
############

####filter the most frequent models
freq_knownMod <- c ('Stoch. Human Exp. Dose Simul. model (SHEDS-multimedia) USEPA', 'Monte Carlo Risk Assessment (MCRA) RIVM/EFSA',
      'Environment Program (USEtox) model UN', 'Consumer Exposure & Uptake Model (ConsExpo) RIVM',
    'Multimedia transport & transform. model (CalTOX) USEPA', 
    'Pesticide Residue Intake Model (PRIMo) EFSA', 'Exposure Forecasting (ExpoCast) USEPA',
  'Modelling Exp. to chemic. for Risk assess. (MERLIN-Expo) EU', 'Probabilistic Aggreg. Consm. Exp. Model (PACEM) RIVM',
  "Risk and Exposure Tool (EGRET) ESIG/EU", "Risk Assess. Ident. And Ranking model (RAIDAR) cemc-TrentU CA", 
  'Targeted Risk Assessment tool (Ecetoc TRA)')
df_Models_filt <- df_Models %>% 
  filter(Knownmod %in% freq_knownMod)
View(df_Models_filt)

#Recode some longer labels
df_Models_filt <- df_Models_filt %>% 
  mutate( Knownmod = dplyr::recode( Knownmod, 'Stoch. Human Exp. Dose Simul. model (SHEDS-multimedia) USEPA' = 'Stoch. Human Exp. Dose Simul. \nmodel (SHEDS-multimedia) USEPA',
                      'Monte Carlo Risk Assessment (MCRA) RIVM/EFSA' = 'Monte Carlo Risk Assessment\n(MCRA) RIVM/EFSA',                 
                      'Environment Program (USEtox) model UN' = 'Environment Program\n(USEtox) model UN',
                      'Consumer Exposure & Uptake Model (ConsExpo) RIVM' = 'Consumer Exposure & Uptake\nModel (ConsExpo) RIVM',
                      'Multimedia transport & transform. model (CalTOX) USEPA' = 'Multimedia transport & transform.\nmodel (CalTOX) USEPA',
                  'Pesticide Residue Intake Model (PRIMo) EFSA' ='Pesticide Residue Intake\nModel (PRIMo) EFSA',
                  'Exposure Forecasting (ExpoCast) USEPA' = 'Exposure Forecasting\n(ExpoCast) USEPA',
                  'Modelling Exp. to chemic. for Risk assess. (MERLIN-Expo) EU' = 'Modelling Exp. to chemic. for\nRisk assess. (MERLIN-Expo) EU',
                  'Probabilistic Aggreg. Consm. Exp. Model (PACEM) RIVM' = 'Probabilistic Aggreg. Consm. Exp.\nModel (PACEM) RIVM',
                  "Risk and Exposure Tool (EGRET) ESIG/EU" = "Risk and Exposure Tool\n(EGRET) ESIG/EU",
                  "Risk Assess. Ident. And Ranking model (RAIDAR) cemc-TrentU CA" = "Risk Assess. Ident. And Ranking \nmodel (RAIDAR) cemc-TrentU CA",
                  'Targeted Risk Assessment tool (Ecetoc TRA)' = 'Targeted Risk Assessment\ntool (Ecetoc TRA)'))
View(df_Models_filt)


                      
###Plot Known models versus Exposure route
Models_ExpRoute_filt<- df_Models_filt %>% 
  group_by(Knownmod, ExI) %>% 
  summarize(n = n()) %>%
  mutate(perc = round(100*n/sum(n), 0)) %>%
  ggplot(aes(y = Knownmod, x = perc, fill= ExI)) +
  geom_bar(stat = "identity") + 
 # scale_x_continuous(breaks = seq(0, 100, 25), 
         #            limits = c(0, 100)) +
  geom_text(aes(label = perc), size=6,
            position=position_stack(vjust=0.5), colour="white") +
  scale_fill_discrete(name="Exposure route") +
  theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 15),
        axis.text.y = element_text(face = 'bold',size = 18),
        legend.text = element_text( size = 18),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(face = 'bold',size = 20),
        plot.caption = element_text(face = 'bold', size = 15),
        plot.subtitle = element_text(face = 'bold',size = 15))+
  labs(#title = "Known models versus Exposure route", 
       #caption = "ONTOX - Scoping review study",
       x = "Percentage (%)", y = "Mathematical models and toolboxes")
Models_ExpRoute_filt

#####
########Plot all the Known models found versus Chemicals
##############
#Remove duplicates on selected columns and remove NAs
#df_Models_3 <- df_Models %>% 
 # distinct(Article.ID,Knownmod,Chemicals, .keep_all = TRUE) %>% 
  # na.omit()
#View(df_Models_3)

###### Group Knownmod and count Chemicals versus models
#dfModels_3 <- df_Models_3 %>% 
 # group_by(Knownmod) %>% 
  #count(Chemicals, Knownmod)
#View(dfModels_3)

#######
###Plot Known models versus Chemicals
###########
Models_Chemicals<- df_Models %>% 
  distinct(Article.ID,Knownmod,Chemicals, .keep_all = TRUE) %>% 
  na.omit() %>%
  group_by(Knownmod) %>% 
  count(Chemicals, Knownmod) %>% 
  ggplot(aes(y= Knownmod, x=n, fill= Chemicals)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Chemical classes") +
  theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 20))+
  labs(#title = "Known models versus Chemical classes", 
       #caption = "ONTOX - Scoping review study",
       x = "Counts of chemical classes", y = "Mathematical models/toolboxes")
Models_Chemicals


####Plot only the 12 more used Known models versus Chemicals
###

### Group Knownmod and count Chemicals versus Known models
###Plot Known models versus Exposure route
Models_ExpRoute_filt_Ch<- df_Models_filt %>%
  group_by(Knownmod) %>% 
  count(Chemicals, Knownmod) %>% 
  ggplot(aes(y = Knownmod, x = n, fill= Chemicals)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), size=5,
            position=position_stack(vjust=0.5), colour="white") +
  scale_fill_discrete(name="Chemicals") +
  theme_bw() +
  theme(axis.title = element_text(face = 'bold', size = 15),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 12),
        legend.text = element_text(face = 'bold', size = 11),
        legend.title = element_text(face = 'bold',size = 11),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(face = 'bold',size = 15),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(face = 'bold',size = 15))+
  labs(title = "Known models versus Chemicals", 
       caption = "ONTOX - Scoping review study",
       x = "Counts of Chemicals", y = "Known models")
Models_ExpRoute_filt_Ch


######
######Plotting with faceting

#Remove duplicates on selected columns and remove NAs
#df_Models_filt_d <- df_Models_filt %>% 
 # distinct(Article.ID,Knownmod,Chemicals, .keep_all = TRUE) %>% 
 # na.omit()
# View(df_Models_filt_d)
View(df_Models_filt)

##Recode to short acronyms of the models
library(dplyr)
df_Models_filt_short <- df_Models_filt%>%
  mutate(Knownmod = dplyr::recode(Knownmod, "Monte Carlo Risk Assessment\n(MCRA) RIVM/EFSA" = "MCRA",
                                  "Consumer Exposure & Uptake\nModel (ConsExpo) RIVM" = "ConsExpo",
                                  "Stoch. Human Exp. Dose Simul. model\n (SHEDS-multimedia) USEPA" = "SHEDS\nmultimedia",
                                  'Multimedia transport & transform.\nmodel (CalTOX) USEPA' = 'CalTOX',
                                  'Pesticide Residue Intake\nModel (PRIMo) EFSA' = 'PRIMo',
                                  'Targeted Risk Assessment\ntool (Ecetoc TRA)' = 'Ecetoc',
                                  'Risk and Exposure Tool\n(EGRET) ESIG/EU' = 'EGRET',
                                  'Environment Program\n(USEtox) model UN' = 'USEtox',
                                  'Risk Assess. Ident. And Ranking model \n(RAIDAR) cemc-TrentU CA' = 'RAIDAR',
                                  'Probabilistic Aggreg. Consm. Exp.\nModel (PACEM) RIVM' = 'PACEM',
                                  'Exposure Forecasting\n(ExpoCast) USEPA' = 'ExpoCast',
                                  'Modelling Exp. to chemic. for\nRisk assess. (MERLIN-Expo) EU' = 'MERLIN-Expo'))
View(df_Models_filt_short)


##### Replace String with Another String on the Known models column
library(dplyr)
df_Models_filt_short <- df_Models_filt_short %>% 
  mutate(Chemicals = dplyr::recode(Chemicals, 'Pharmaceuticals' = 'Pharms.',
                                   "Non-persistent" = "Non persistent",
                                   "Enviromental emissions" = "Enviromen. emissions",
                          "Volatile Org. Compounds" = "Volatile Org. Comps.",
                          "Cosmetic ingredients" = "Cosmetic ingrs."))

Models_Chemicals_f <- df_Models_filt_short %>% 
  distinct(Article.ID,Knownmod,Chemicals, .keep_all = TRUE) %>% 
  na.omit() %>% 
  group_by(Knownmod, Chemicals) %>% 
  summarize(n = n()) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = Knownmod, y = perc, fill = Chemicals)) +
  geom_bar(stat = "identity") +
 # scale_fill_discrete(name="Chemicals") +
  geom_col(show.legend = F) +
  facet_grid( ~ Chemicals, labeller = label_wrap_gen(width=10)) + 
  coord_flip() +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 160, 60))+
  geom_text(aes(label = round(perc, 0)), size = 6, hjust = -.1) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(face = 'bold', size = 12),
        strip.background = element_rect(fill="white", colour="black",size=1)) + #change the facets font)
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 14),
        axis.text.y = element_text(face = 'bold', size = 17),
        #legend.text = element_text(size = 11),
        #legend.title = element_text(size = 11),
        #legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Known models versus Chemicals", 
       #caption = "ONTOX - Scoping review study",
       x = "Mathematical models / toolboxes", y = "percentage (%)")
Models_Chemicals_f

#####
##### Data or Input variables
#############################
View(df_Tot)
##Unite all the columns with the Data columns into a total info column DataT coming either from the Sysrev menu or as
#an input at the Notes colum by the Reviwers and separate to clean from NA and duplicated values
library(tidyr)
df_Tot_D <- df_Tot %>%  
  unite(col = "DataT", c("Data.x", "Data.y","Data.x.x", "Data.y.y","Data.x.x.x", "Data.y.y.y","Data"),
        sep = ";", na.rm = TRUE) %>% 
  separate(col = "DataT", into = c("D1","D2","D3","D4","D5","D6","D7"), sep = ";")
View(df_Tot_D)

###Substitute "NA" and "other" word with NA 
df_Tot_D[df_Tot_D == 'NA'] <- NA
View(df_Tot_D)

##### Unite all the information with "," while removing NAs and separate by "," all the possible word inputs
library(tidyr)
df_Tot_Dm <- df_Tot_D %>%  
  unite(col = "Data", c("D1","D2","D3","D4","D5","D6","D7"), sep = ",", na.rm = TRUE) %>% 
  separate(col = "Data", into = c("d1","d2","d3","d4","d5","d6","d7"), sep = ",") 
View(df_Tot_Dm)

##Remove the word "Other", it was used when the Reviewers had to input words at the Notes column
df_Tot_Dm[df_Tot_Dm == "Other"] <- NA

###
df_Tot_Dm1 <- df_Tot_Dm %>% 
  subset(select = c("Article.ID","d1","d2","d3","d4","d5","d6","d7"))
View(df_Tot_Dm1)

df_Tot_Dm2 <- df_Tot_Dm1 %>% 
  pivot_longer(cols=c("d1","d2","d3","d4","d5","d6","d7"),
               names_to="Ks",
               values_to="Data") %>%
  drop_na()
View(df_Tot_Dm2 )

#Remove duplicates on selected columns, recode empty stings by NAs and remove them 
##We remove leading and trailing white space on each row
df_Tot_Dm3 <- df_Tot_Dm2 %>% 
  distinct(Article.ID, Data, .keep_all = TRUE) %>% 
  mutate_at("Data", ~ na_if(.,'')) %>% 
  na.omit() %>% 
  mutate_all(str_trim)
View(df_Tot_Dm3)

##### Replace String with Another String on the Data column
library(dplyr)
df_Tot_Dm3 <- df_Tot_Dm3 %>% 
  mutate( Data = dplyr::recode( Data, 'area' = 'surface area', 'cross-sectional area' = 'surface area', 
                                'dermal absorption' = 'dermal absorption coefficient',
                                'fraction absorbed in skin' = 'absorbed fraction',
                                'absorption fraction' = 'absorbed fraction', 
                                'dermal absorption efficiency' = 'absorption efficiency',
                                'area' = 'surface area',
                                "hand area" = "surface area",
                                "skin surface area" = "surface area",
                                "skin area" = "surface area",
                                "average time" = "average exposure time",
                                "averaging time" = 'average exposure time',
                                'avearing time' = 'average exposure time',
                                'averaged time' = 'average exposure time', 'time' = 'average exposure time',
                                'time activity' = 'activity paterns(time)',
                                'partition coefficients' = 'partition coefficient',
                                'skin permeability coefficient' = 'permeability coefficient',
                                'quantity of airborne spray' = 'quantity of spray',
                                'quantity of spray liquid' = 'quantity of spray',
                                'vapor pressure' = 'vapour pressure',
                                'frequency' = 'Frequency',
                                'fugacity level' = 'fugacity',
                                'intake fraction' = 'intake',
                                'diffusitivity' = 'diffusivity',
                                'diffution coefficient' = 'diffusion coefficient',
                                'body  weight' = 'body weight',
                                'activity' = 'activity paterns(time)',
                                'absorption' = 'absorption efficiency',
                                'trasfer efficiency' = 'transfer efficiency',
                                'use patterns' = 'activity paterns(time)',
                                'layer thickness' = 'thickness',
                                'volume fraction' = 'volume', 'building volume' = 'volume',
                                'volume/mass' = 'volume',
                                'consumer products use' = 'Frequency',
                                'number of uses' = 'Frequency',
                                'concentration/area' = 'Concentration',      
                                'mass transfer efficiency' = 'transfer efficiency',
                                'initial age of children' = 'age','active residue' = 'residue level',
                                'load pesticide by hand' = 'Amount','mass' = 'Amount',
                                'ventilation flow' = 'ventilation rate',
                                'fraction of the product' = 'fraction of chemical',
                                'chemical Intake' = 'intake', 'intake time' = 'intake',
                                'air volume' = 'volume', 'FoodEx2 EFSA database' = 'Concentration',
                                'maximum residue limit (MRL) values (Codex Alimentarius)' = 'residue level',
                                'limit of detection' = 'residue level', 'remaining fraction' = 'residue level',
                                'pesticide use reports (PUR)' = 'Frequency', 'food ingestion ration' = 'Consumption',
                                'reaction  half-lives' = 'half-life values', 'biotransformation half-life' = 'half-life values',
                                'half-life in air' = 'half-life values', 'skin laoding' = 'deposition fraction',
                                'fraction' = 'fraction of chemical', 'exposure fraction' = 'Duration',
                                'indoor exposure fraction' = 'Duration', 'fraction of exposed skin' = 'surface area',
                                'air exchange/h' = 'Rate', 'diffusivity' = 'diffusion coefficient',
                                'NOAELs for substance s' = 'NOAEL values', 'vaporization percentage' = 'volatilazation fraction'))
View(df_Tot_Dm3)


View(df_Tot_Dm3)
View(df_Models_filt) 

###we tranform into character so we can join the dataframes
df_Tot_Dm3$Article.ID <- as.character(df_Tot_Dm3$Article.ID)
df_Models_filt$Article.ID <- as.character(df_Models_filt$Article.ID)

####Joint the Filtered Known models and Data
## that is the dfs df_Tot_Dm3 and f_Models_filt
df_Km_D <-full_join(df_Tot_Dm3, df_Models_filt, by = "Article.ID", multiple = "all", relationship = "many-to-many")
View(df_Km_D)

#Remove duplicates on selected columns and remove NAs
df_Km_D <- df_Km_D %>% 
  distinct(Article.ID,Data,Knownmod, .keep_all = TRUE) %>% 
  na.omit()
View(df_Km_D)

######Data versus filtered Known models
Models_Km_filt_D<- df_Km_D %>%
  group_by(Knownmod) %>% 
  count(Knownmod,Data) %>% 
  ggplot(aes(y = Data, x = n, fill= Knownmod)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = n), size=5,
            position=position_stack(vjust=0.5), colour="white") +
  scale_fill_discrete(name="Known models") +
  theme_bw() +
  theme(axis.title = element_text(face = 'bold', size = 20),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 12),
        legend.text = element_text(face = 'bold', size = 11),
        legend.title = element_text(face = 'bold',size = 14),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(face = 'bold',size = 20),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(face = 'bold',size = 15))+
  labs(title = "Parameters versus Known models", 
       caption = "ONTOX - Scoping review study",
       x = "Counts of Known models", y = "Parameters")
Models_Km_filt_D

##### Replace String with Another String on the Known models column
class(df_Km_D)
library(dplyr)
df_Km_D <- df_Km_D %>%
  mutate(Knownmod = dplyr::recode(Knownmod, "Monte Carlo Risk Assessment (MCRA) RIVM/EFSA" = "MCRA",
         "Consumer Exposure & Uptake Model (ConsExpo) RIVM" = "ConsExpo",
         "Stoch. Human Exp. Dose Simul. model  (SHEDS-multimedia) USEPA" = "SHEDS",
         'Multimedia transport & transform. model (CalTOX) USEPA' = 'CalTOX',
         'Pesticide Residue Intake Model (PRIMo) EFSA' = 'PRIMo',
         'Targeted Risk Assessment tool (Ecetoc TRA)' = 'Ecetoc',
         'Risk and Exposure Tool (EGRET) ESIG/EU' = 'EGRET',
         'Environment Program (USEtox) model UN' = 'USEtox',
         'Risk Assess. Ident. And Ranking model (RAIDAR) cemc-TrentU CA' = 'RAIDAR',
         'Probabilistic Aggreg. Consm. Exp. Model (PACEM) RIVM' = 'PACEM',
         'Exposure Forecasting (ExpoCast) USEPA' = 'ExpoCast',
         'Modelling Exp. to chemic. for Risk assess. (MERLIN-Expo) EU' = 'MERLIN-Expo'))
View(df_Km_D)

View(df_Tot_Dm3)
View(df_Models_filt_short)

####Joint the Filtered Known models and Data
## that is the dfs df_Tot_Dm3 and f_Models_filt
df_Km_D_sh <-full_join(df_Tot_Dm3, df_Models_filt_short, by = "Article.ID", multiple = "all", relationship = "many-to-many") %>% 
  distinct(Article.ID,Data,Knownmod, .keep_all = TRUE) %>% 
  na.omit()
View(df_Km_D_sh)


###Plot with faceting
Models_Km_filt_facet <- df_Km_D_sh %>% 
  group_by(Data, Knownmod) %>% 
  summarize(n = n()) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = Data, y = perc, fill = Knownmod)) +
  geom_bar(stat = "identity") +
  # scale_fill_discrete(name="Chemicals") +
  geom_col(show.legend = F) +
  facet_grid( ~ Knownmod, labeller = label_wrap_gen(width=10)) + 
  coord_flip() +
  ylim(c(0, 142)) +
  geom_text(aes(label = round(perc, 0)), size =5.5, hjust = -.1) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.text.x = element_text(face = 'bold', size = 14),
        strip.background = element_rect(fill="white", colour="black",size=1)) + #change the facets font)
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 14),
        axis.text.y = element_text(face = 'bold', size = 16),
        #legend.text = element_text(size = 11),
        #legend.title = element_text(size = 11),
        #legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 20),
        plot.caption = element_text(face = 'bold', size = 10),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Parameters versus Known models", 
       #caption = "ONTOX - Scoping review study",
       x = "Parameters", y = "percentage (%)")
Models_Km_filt_facet


###### Group Est_M and count Data versus models
#######

##Join the df df_Tot_Dm3 that contain clean the Data column
##and dataIV_m that contains Chemicals, Exposure_route and Est_m data
df_D <- full_join(df_Tot_Dm3, dataIV_m, by = "Article.ID", multiple = "all")
View(df_D)

#Remove duplicates on selected columns and remove NAs

#df_D_Est <- df_D %>% 
 # distinct(Article.ID,Data,Est_M, .keep_all = TRUE) %>% 
#  na.omit()
#View(df_D_Est)

###### Group Est_M and count Data versus models
#######
###########
Data_Methods<- df_D  %>% 
  distinct(Article.ID,Data,Est_M, .keep_all = TRUE) %>% 
  na.omit() %>% 
  group_by(Est_M) %>% 
  count(Data, Est_M) %>% 
  ggplot(aes(y= Data, x=n, fill= Est_M)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Estimated Methodologies") +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(title = "Data versus Estimated Methodologies", 
       caption = "ONTOX - Scoping review study",
       x = "Counts of Estimated Methodologies", y = "Data")
Data_Methods


######
######Plotting with faceting
#####
Data_Methods_f <- df_D  %>% 
  distinct(Article.ID,Data,Est_M, .keep_all = TRUE) %>% 
  na.omit() %>%  
  group_by(Data, Est_M) %>% 
  summarize(n = n()) %>%
  mutate(perc = 100*n/sum(n)) %>%
  mutate(Est_M = as.factor(Est_M)) %>% 
  ggplot() +
  scale_fill_brewer(palette = "Set2") +
  geom_bar(aes(x = reorder_within(Data, perc, Est_M),  
               y = perc, fill = Est_M), stat = "identity", show.legend = FALSE) +
  geom_text(aes(x = reorder_within(Data, perc, Est_M), y = perc,
                label = round(perc, 0)), size = 6, hjust = -.1,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  facet_wrap(~ Est_M, scales = "free_y", labeller = label_wrap_gen(width=10)) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, 119)) +
  theme_bw() +
 theme(legend.position = "none") +
  theme(strip.text.x = element_text(face = 'bold', size = 16),
        strip.background = element_rect(fill="white", colour="black",size=1)) + #change the facets font)) + #change the facets font)
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 14),
        axis.text.y = element_text(face = 'bold', size = 16),
        #legend.text = element_text(size = 11),
        #legend.title = element_text(size = 11),
        #legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Parameters versus Estimated Methodologies", 
       #caption = "ONTOX - Scoping review study",
       x = "Parameters", y = "percentage (%)")
Data_Methods_f
View(as.data.frame(Data_Methods_f))


#####Data and Ex route
#Remove duplicates on selected columns and remove NAs
#df_D_Ex <- df_D %>% 
 # distinct(Article.ID,Data,ExI, .keep_all = TRUE) %>% 
#  na.omit()
# View(df_D_Ex)

###### Group Est_M and count Data versus models
#df_D_Ex_p <- df_D_Ex %>% 
 # group_by(ExI) %>% 
#  count(Data, ExI)
#View(df_D_Ex_p)

#######
###Plot Data versus Exposure route
###########
Data_ExRout<- df_D %>% 
  distinct(Article.ID,Data,ExI, .keep_all = TRUE) %>% 
  na.omit() %>% 
  group_by(ExI) %>% 
  count(Data, ExI) %>%
  ggplot(aes(y= Data, x=n, fill= ExI)) +
  geom_bar(stat = "identity") +
  scale_fill_discrete(name="Exposure route") +
  theme_bw() +
  theme(axis.title = element_text(size = 22),
        axis.text.x = element_text(face = 'bold', size = 12),
        axis.text.y = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(#title = "Data versus Exposure route", 
       #caption = "ONTOX - Scoping review study",
       x = "Counts of Exposure route", y = "Parameters")
Data_ExRout

######
######Plotting with faceting
#####
Data_ExRout_f <- df_D %>% 
  distinct(Article.ID,Data,ExI, .keep_all = TRUE) %>% 
  na.omit() %>% 
  group_by(Data, ExI) %>% 
  summarize(n = n()) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = Data, y = perc, fill = ExI)) +
  geom_bar(stat = "identity") +
  # scale_fill_discrete(name="Chemicals") +
  geom_col(show.legend = F) +
  facet_grid( ~ ExI,labeller = label_wrap_gen(width=10)) + 
  coord_flip() +
  ylim(c(0, 130)) +
  geom_text(aes(label = round(perc, 1)), hjust = -.1) +
  theme_bw() +
  # theme(legend.position = "none") +
  theme(axis.title = element_text(size = 15),
        axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10),
        #legend.text = element_text(size = 11),
        #legend.title = element_text(size = 11),
        #legend.key.size = unit(1, 'cm'),
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 15))+
  labs(title = "Data versus Exposure route", 
       caption = "ONTOX - Scoping review study",
       x = "Input variables", y = "percent (%)")
Data_ExRout_f

