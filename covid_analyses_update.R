#######################################################
# COVID19 case study
#######################################################

# 15 June 2022
rm(list=ls())

# Load in data


# SET OUTPUT DIRECTORY
OutputDirectory <- "./outputs/"
OutputDirectoryData <- "../Data/"


# Source functions files
source("./script/multiplot.R")

# load package
pacman::p_load(readxl, writexl, lubridate, zoo, ggplot2, tidyverse, stringi,Hmisc, ggtext)

# load data
d = read_xlsx('./data/raw/COVID19_table.xlsx')

# select data and cleaning
d = d %>% filter(Complete =="y") 

d = d %>% mutate(
 st_problem_solve = tolower(st_problem_solve),
 st_problem_solve = factor(st_problem_solve,levels=
                            c("monitor cases",
                              "monitor human mobility","monitor risk perceptions, behaviour, trust, knowledge",
                              "understand transmission-dynamics (origin)","risk factors disease",
                              "understand transmission-dynamics (risk animal-human)","early signal detection/monitoring (text mining)",
                              "estimate epidemiological parameters","understand transmission-dynamics (role human mobility)",
                              "understand transmission-dynamics","risk importation/reintroduction", "improve data access/use",
                              "estimate burden/transmission","evaluate impact npi")),
 st_data_sources = tolower(st_data_sources),
 st_missing_data = tolower(st_missing_data),
 st_reason_missing_data = tolower(st_reason_missing_data),
 discussion_ph_before = tolower(discussion_ph_before),
 discussion_ph_before = ifelse(discussion_ph_before=="na", NA,discussion_ph_before),
 discussion_ph_before = factor(discussion_ph_before, levels=c("y","n","partly")),
 influence_decision = tolower(influence_decision),
 influence_decision = ifelse(influence_decision=="na", NA,influence_decision),
 influence_decision = factor(influence_decision, levels=c("y","n","partly")),
 increase_awareness = tolower(increase_awareness),
 increase_awareness = ifelse(increase_awareness=="na", NA,increase_awareness),
 increase_awareness = factor(increase_awareness, levels=c("y","n","partly")),
 st_communication_means = tolower(st_communication_means),
 st_outbreak_period = tolower(st_outbreak_period),
 st_outbreak_period = factor(st_outbreak_period, levels=c("pre-pandemic period","early pandemic period","middle pandemic period","late pandemic period","unknown")),
) 
  
  

names(d) = tolower(names(d))
names(d) = gsub(" ", "_",names(d))

ds = d %>%
  select(problem_to_solve, 
         st_problem_solve,
         data_the_study_relied_on, 
         st_data_sources,
         data_that_would_have_helped_improve_the_study,
         st_missing_data,
         why_was_the_missing_data_not_available_to_you,
         st_reason_missing_data, 
         st_approaches_used,
         discussion_ph_before,                                                                                                          
         influence_decision,                                                                      
         increase_awareness,  
         outbreak_period_the_study_refers_to, 
         st_communication_means,
         st_outbreak_period,
         ref.,
         ) %>%
  rename(
    data_sources = data_the_study_relied_on,
    missing_data = data_that_would_have_helped_improve_the_study,
    outbreak_period = outbreak_period_the_study_refers_to,
    reason_missing_data = why_was_the_missing_data_not_available_to_you
  )

ds = ds %>%
  mutate(
    data_s_incidence = 0,
    data_s_mobility = 0,
    data_s_social = 0,
    data_s_genomic = 0,
    data_s_demographic = 0,
    data_s_airtravel = 0,
    data_s_mobility = 0,
    data_s_serological = 0,
    data_s_vaccination = 0,
    data_s_unstructured = 0,
    data_s_other = 0,
    data_m_underreporting = 0,
    data_m_mobility = 0,
    data_m_social = 0,
    data_m_standardised_case = 0,
    data_m_comorbidity = 0,
    data_m_travel = 0,
    data_m_genomic = 0,
    data_m_serological = 0,
    data_m_disaggregated = 0,
    data_m_demographic = 0,
    data_m_epi_par = 0,
    data_m_intervention = 0,
    data_m_delays = 0,
    data_m_other = 0
  )


ds$data_s_incidence[grep("incidence",ds$st_data_sources)] = 1
ds$data_s_demographic[grep("demographic",ds$st_data_sources)] = 1
ds$data_s_serological[grep("serological",ds$st_data_sources)] = 1
ds$data_s_mobility[grep("mobility",ds$st_data_sources)] = 1
ds$data_s_social[grep("social",ds$st_data_sources)] = 1
ds$data_s_genomic[grep("genetic",ds$st_data_sources)] = 1
ds$data_s_airtravel[grep("air travel",ds$st_data_sources)] = 1
ds$data_s_vaccination[grep("vaccination",ds$st_data_sources)] = 1
ds$data_s_unstructured[grep("unstructured",ds$st_data_sources)] = 1
ds$data_s_other[grepl(c("census|oxford|scientific|co-variate|biologging"),ds$st_data_sources)] = 1

ds$data_m_demographic[grep("demographic",ds$st_missing_data)] = 1
ds$data_m_comorbidity[grep("comorbidity",ds$st_missing_data)] = 1
ds$data_m_mobility[grep("mobility",ds$st_missing_data)] = 1
ds$data_m_social[grep("social",ds$st_missing_data)] = 1
ds$data_m_genomic[grep("genomic",ds$st_missing_data)] = 1
ds$data_m_travel[grep("travel",ds$st_missing_data)] = 1
ds$data_m_standardised_case[grep("standardised",ds$st_missing_data)] = 1
ds$data_m_epi_par[grep("epidemiological",ds$st_missing_data)] = 1
ds$data_m_underreporting[grep("underreporting",ds$st_missing_data)] = 1
ds$data_m_intervention[grep("intervention",ds$st_missing_data)] = 1
ds$data_m_disaggregated[grep("disaggregated",ds$st_missing_data)] = 1
ds$data_m_delays[grep("delays",ds$st_missing_data)] = 1
ds$data_m_other[grep(c("other"),ds$st_missing_data)] = 1




# Descriptives
#table(d$`standardized problem to solve_old`)
#table(d$`standardized activity`)
#table(d$`standardized data sources`)
#table(d$st_missing_data)

table(d$st_problem_solve)
table(d$st_data_sources)
table(d$st_approaches_used)
table(ds$st_reason_missing_data)
table(ds$st_approaches_used)
table(ds$st_communication_means)


# Visualisations
###########################################################
# Problems to solve
p1 = ds %>% 
  group_by(st_problem_solve) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,0),'%')) %>%
ggplot(aes(x = reorder(st_problem_solve,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) + 
  labs(title= paste0("Aim of the work (n=",nrow(ds), ")"),
       x="")+
  geom_text(aes(label = perc,hjust=-0.2), size=8)
p1

# Problems to solve by pandemic period
p2 = ggplot(ds, aes(x = st_problem_solve, fill=st_outbreak_period)) + 
  geom_bar(position = 'stack') +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) +
  labs(title= paste0("Aim of the work by pandemic period (n=",nrow(ds), ")"),
       x="") 
p2

p3 = ds %>% filter(!st_outbreak_period %in%c("NA","unknown")|is.na(st_outbreak_period))%>% 
  group_by(st_problem_solve,st_outbreak_period) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,0),'%')) %>%
  ggplot(aes(x = reorder(st_problem_solve,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25),
        strip.text.x = element_text(size = 15)) +
  labs(title= paste0("Aim of the work by pandemic period (n=",nrow(ds), ")"),
         x="") +
  facet_wrap(~st_outbreak_period, nrow =1)+
  scale_y_continuous(breaks = seq(0, 10, by=2)) #+
  #geom_text(aes(label = perc,hjust=0.2))
p3

# Data sources used
data_used = as.data.frame(sapply(ds %>% select(data_s_incidence,
                       data_s_mobility,
                       data_s_social,
                       data_s_genomic,
                       data_s_demographic,
                       data_s_airtravel,
                       data_s_mobility,
                       data_s_serological,
                       data_s_vaccination,
                       data_s_unstructured,
                       data_s_other), function(x) sum(x)))

names(data_used) = "count"
data_used$type = rownames(data_used)

p4 = data_used %>% 
  mutate(countT = length(ds$problem_to_solve),
         perc=paste0(round(100*count/countT,0),'%')) %>%
  ggplot(aes(x = reorder(type,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) +
  labs(title= "Types of data used",
       x="") +
  geom_text(aes(label = perc,hjust=-0.2), size=5)+
  scale_y_continuous(breaks = seq(0, 35, by=5))
p4

# Data sources missing
data_missing = as.data.frame(sapply(ds %>% select(data_m_demographic,
                                                  data_m_comorbidity,
                                               data_m_mobility,
                                               data_m_social,
                                               data_m_genomic,
                                               data_m_travel,
                                               data_m_disaggregated,
                                               data_m_standardised_case,
                                               data_m_epi_par,
                                               data_m_underreporting,
                                               data_m_intervention,
                                               data_m_delays,
                                               data_m_other), function(x) sum(x)))
names(data_missing) = "count"
data_missing$type = rownames(data_missing)



p5 = data_missing %>% 
  mutate(countT = length(ds$problem_to_solve[!is.na(ds$missing_data)]),
         perc=paste0(round(100*count/countT,0),'%')) %>%
  ggplot(aes(x = reorder(type,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) +
  labs(title= "Types of data missing",
       x="") +
  geom_text(aes(label = perc,hjust=-0.2), size=5)+
  scale_y_continuous(breaks = seq(0, 10, by=2))
p5


# Evidence used
p6 = ds %>% filter(!discussion_ph_before %in% c("unknown","na")& !is.na(discussion_ph_before)) %>%
  group_by(discussion_ph_before) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,1),'%')) %>%
  ggplot(aes(x = discussion_ph_before, y = count)) +
  geom_bar(stat = 'identity', fill="black") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(title= "Discussion with PH/Decision-makers",
       x="") +
  theme_bw() +
  geom_text(aes(label = perc,vjust=-0.6))+
  ylim(0,50)
p6

p7 =  ds %>% filter(!influence_decision %in% c("unknown","na")& !is.na(influence_decision)) %>%
  group_by(influence_decision) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,1),'%')) %>%
  ggplot(aes(x = influence_decision, y = count)) +
  geom_bar(stat = 'identity', fill="black") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(title= "Influence decision-making",
       x="") +
  theme_bw() +
  geom_text(aes(label = perc,vjust=-0.6))+
  ylim(0,50)
p7

p8 =  ds %>% filter(!increase_awareness%in% c("unknown","na")& !is.na(increase_awareness)) %>%
  group_by(increase_awareness) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,1),'%')) %>%
  ggplot(aes(x = increase_awareness, y = count)) +
  geom_bar(stat = 'identity', fill="black") +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold")) +
  labs(title= "Increase situational awareness",
       x="") +
  theme_bw() +
  geom_text(aes(label = perc,vjust=-0.6))+
  ylim(0,50) 
p8

multiplot(p6,p7,p8, cols=3)

# WHy data missing
p10 =  ds %>% filter(!st_reason_missing_data%in% c("unknown","na")& !is.na(st_reason_missing_data)) %>%
  group_by(st_reason_missing_data) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,1),'%')) %>%
  ggplot(aes(x = reorder(st_reason_missing_data,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) +
  labs(title= "Reasons for data missing",
       x="") +
  geom_text(aes(label = perc,hjust=-0.2), size=5)+
  scale_y_continuous(breaks = seq(0, 20, by=5))
p10

# Communication means
p11 = ds %>% filter(!st_communication_means%in% c("unknown","na")& !is.na(st_communication_means)) %>%
  group_by(st_communication_means) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,0),'%')) %>%
  ggplot(aes(x = reorder(st_communication_means,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) + 
  labs(title= paste0("Communication means (n=",nrow(ds), ")"),
       x="")+
  geom_text(aes(label = perc,hjust=-0.2), size=8)
p11


# Approaches used
p12 = ds %>% filter(!st_approaches_used%in% c("unknown","na")& !is.na(st_approaches_used)) %>%
  group_by(st_approaches_used) %>% 
  summarise(count = n()) %>%
  mutate(countT = sum(count),
         perc=paste0(round(100*count/countT,0),'%')) %>%
  ggplot(aes(x = reorder(st_approaches_used,(count)), y = count)) + 
  geom_bar(stat = 'identity', fill="black") +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25)) + 
  labs(title= paste0("Approaches used (n=",nrow(ds), ")"),
       x="")+
  geom_text(aes(label = perc,hjust=-0.2), size=8)
p12

# Aim study by approaches used
p13 = ds %>% filter(!st_approaches_used%in% c("unknown","na")& !is.na(st_approaches_used)) %>%
  ggplot(aes(fill = st_approaches_used,x=st_problem_solve))+
  geom_bar() +
  coord_flip() +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        title=element_text(size=25),
        legend.text = element_text(size=15)) + 
  labs(title= paste0("AIms of the work (n=",nrow(ds), ")"),
       x="",
       fill="Approaches used")
p13

# Save plots
pdf(paste0(OutputDirectory,"plots/problems.pdf"), width=10,height=6)
print(p1)
dev.off()

png(paste0(OutputDirectory,"plots/problems.png"), width=1000,height=700)
print(p1)
dev.off()

pdf(paste0(OutputDirectory,"plots/problems_period.pdf"), width=12,height=6)
print(p2)
dev.off()


pdf(paste0(OutputDirectory,"plots/problems_period_facet.pdf"), width=18,height=6)
print(p3)
dev.off()

png(paste0(OutputDirectory,"plots/problems_period_facet.png"), width=1600,height=600)
print(p3)
dev.off()

pdf(paste0(OutputDirectory,"plots/data_used.pdf"), width=10,height=6)
print(p4)
dev.off()

png(paste0(OutputDirectory,"plots/data_used.png"), width=1000,height=700)
print(p4)
dev.off()


pdf(paste0(OutputDirectory,"plots/data_missing.pdf"), width=10,height=6)
print(p5)
dev.off()

png(paste0(OutputDirectory,"plots/data_missing.png"), width=1000,height=700)
print(p5)
dev.off()

png(paste0(OutputDirectory,"plots/data_missing_reason.png"), width=1500,height=700)
print(p10)
dev.off()


png(paste0(OutputDirectory,"plots/communication.png"), width=1500,height=700)
print(p11)
dev.off()

png(paste0(OutputDirectory,"plots/approaches_used.png"), width=1500,height=700)
print(p12)
dev.off()

png(paste0(OutputDirectory,"plots/problems_vs_approaches_used.png"), width=1500,height=700)
print(p13)
dev.off()


# Save dataset
write_xlsx(ds,"./data/clean/COVID19_selected.xlsx")
write_xlsx(d,"./data/clean/COVID19.xlsx")
