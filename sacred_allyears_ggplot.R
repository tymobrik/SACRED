#************************************************************************************************

#Spatial Analysis of Contemporary Religious Diversity in Ukraine (SACRED-Ukraine)

#************************************************************************************************
#
library(readxl)  #to read xlsx
library(tidyverse)
library(haven)
library(labelled)
library(here)
library(reshape)
#
#************************************************************************************************
#PART 1 #	Religious communities 1991-2015 (used for Brik, 2019)
#************************************************************************************************

sheet_name <- c("PART1", "PART2")

revivals_sources <- read_excel(here::here("SACRED_Ukraine_data1.xlsx"),sheet=sheet_name[2])

dim(revivals_sources)
head(revivals_sources)
str(revivals_sources)

#
#LIBERAL APPROACH HERE 
#TREAT ALL TYPES AS EQUAL (active,)
names(revivals_sources)


revivals_sources <- revivals_sources %>% mutate(Prot_sum = rowSums(.[grep("Prot", names(.))], na.rm = T)) %>% 
                                         mutate(Cath_sum = rowSums(.[grep("Cath", names(.))], na.rm = T)) %>% 
                                         mutate(Orth_sum = rowSums(.[grep("Orth", names(.))], na.rm = T))

#By region
revivals_sources <- revivals_sources %>% mutate(region=recode(Oblast, "Donetska oblast"="1.east",
                                                "Luganska oblast"="1.east",
                                                "Kharkivska oblast"="1.east",
                                                "Volynska oblast"="6.west",
                                                "Zakarpatska oblast"="6.west",
                                                "Rivnenska oblast"="6.west",
                                                "Ternopilska oblast"="6.west",
                                                "Chernivetska oblast"="6.west",
                                                "Ivano-Frankivska oblast"="6.west",
                                                "Lvivska oblast"="6.west",
                                                "Kyiv city"="5.kyiv",
                                                "Mykolaivska oblast"="2.south",
                                                "Odesska oblast"="2.south",
                                                "Zaporizska oblast"="2.south",
                                                "Khersonska oblast"="2.south",
                                                "Crimea, Autonomy Republic"="2.south",
                                                "Khmelnitska oblast"="4.center",
                                                "Cherkasska oblast"="4.center",
                                                "Kirovogradska oblast"="4.center",
                                                "Vynnytska oblast"="4.center",
                                                "Poltavska oblast"="4.center",
                                                "Dnipropetrovska oblast"="4.center",
                                                "Zhytomyrska oblast"="3.north",
                                                "Chernigivska oblast"="3.north",
                                                "Kyivska oblast"="3.north",
                                                "Sumska oblast"="3.north"))
#
table(revivals_sources$Oblast,revivals_sources$region)
tbl_reg <- table(revivals_sources$Oblast,revivals_sources$region)
tbl_reg <- as.matrix(tbl_reg)
reg_descr <- tbl_reg[order(-tbl_reg[,1],-tbl_reg[,2], -tbl_reg[,3], -tbl_reg[,4], -tbl_reg[,5], -tbl_reg[,6]),]
reg_descr

#
hist(revivals_sources$All)
p<-ggplot(revivals_sources, aes(x=All)) + 
  geom_histogram(color="black", fill="white") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))
p

#
tapply(revivals_sources$Prot_sum, revivals_sources$year,sum)

result_table <- revivals_sources %>%       # Specify your table
  group_by(region, year) %>%      # Specify your groups (two variables in your case)
  summarize(aver  = mean(Prot_sum),         # Calculate mean for your groups
            total = sum(Prot_sum))           # Calcualte sd for your groups

prot_p<-ggplot(data = result_table, aes(x = year, y = total, shape = region, colour=region)) + 
  geom_line() + xlim(1991,2020)+
  geom_point(size=2)+
  theme(legend.position = "bottom",legend.text = element_text(size=15, face="bold"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold")) +
  facet_wrap(~region)
prot_p

jpeg("REVIVALS_data1_delete1998.jpeg", res = 300, width=3000, height=1500)
prot_p
dev.off()


#RESULT - very bad quality for 1998! Get rid of this year

#
revivals_sources_final <- revivals_sources %>% 
                            filter(year!=1998)

table(revivals_sources_final$year)

tbl_reg <- table(revivals_sources_final$Oblast,revivals_sources_final$region)
tbl_reg <- as.matrix(tbl_reg)
reg_descr <- tbl_reg[order(-tbl_reg[,1],-tbl_reg[,2], -tbl_reg[,3], -tbl_reg[,4], -tbl_reg[,5], -tbl_reg[,6]),]
reg_descr

summary(revivals_sources_final$All)
names(revivals_sources_final)

revivals_sources_final %>%
  group_by(region) %>%
  summarise(mean = mean(Orth_sum), sd=sd(Orth_sum), min=min(Orth_sum), max=max(Orth_sum), n = n())

revivals_sources_final %>%
  group_by(region) %>%
  summarise(mean = mean(Cath_sum), sd=sd(Cath_sum), min=min(Cath_sum), max=max(Cath_sum), n = n())

revivals_sources_final %>%
  group_by(region) %>%
  summarise(mean = mean(Prot_sum), sd=sd(Prot_sum), min=min(Prot_sum), max=max(Prot_sum), n = n())

revivals_sources_final %>% 
  filter(Prot_sum==553) 

# check_sum <- revivals_sources_final %>% 
#                select(starts_with("Prot_")) %>% 
#                filter(Prot_sum==553) 
# 
# write.csv(check_sum,"sacred_check_sum.csv")
