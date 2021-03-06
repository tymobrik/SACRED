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

revivals_sources <- read_excel(here::here("SACRED_Ukraine_data1.xlsx"),sheet=sheet_name[1])

dim(revivals_sources)
head(revivals_sources)
str(revivals_sources)

sources_reshaped <- melt(revivals_sources, id=c("Source","Year"))

sources_p <- ggplot(data = sources_reshaped, aes(x = Year, y = value, shape = Source, colour=Source)) + 
  geom_line() + xlim(1991,2015)+
  geom_point(size=2)+
  theme(legend.position = "bottom",legend.text = element_text(size=15, face="bold"),
        axis.text=element_text(size=15), axis.title=element_text(size=15,face="bold")) +
  facet_wrap(~variable) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(family = 'Times'))

sources_p

jpeg("REVIVALS_data1_sources.jpeg", res = 300, width=3000, height=1500)
sources_p
dev.off()

#Sources
#Brik, T. (2019). When church competition matters? Intra-doctrinal competition in Ukraine, 1992–2012. Sociology of Religion, 80(1), 45-82.
#https://www.wilsoncenter.org/blog-post/religious-regulations-and-orthodox-competition-ukraine 
#https://voxukraine.org/en/church-competition-and-religious-participation-new-evidence-from-ukraine/