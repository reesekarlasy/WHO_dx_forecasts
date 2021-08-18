library(pacman)

p_load("ggplot2", "plyr","tidyverse", "data.table",
       "lubridate", "tidyr", "dplyr", 
       "ggpubr", "openxlsx",  "gridExtra", "zoo", 
       "rgdal", "broom", "rgeos", "readxl", "pacman"
)

#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to source file

###############################
#Import data and data clean
###############################

linelist <- read.csv("Data/data_all.csv", stringsAsFactors=FALSE) %>%
  dplyr::filter(set == "country") %>%
  dplyr::select(time, name, cap_new_cases) %>%
  dplyr::select(time, name, cap_new_cases) %>%
  dplyr::rename(.,"date" = time,
                "country" = name,
                "incidence" = cap_new_cases) %>% #Daily number of cases per capita (7 day rolling average)
  mutate(incidence = ifelse(is.na(incidence), 0, incidence)) %>%
  mutate(date = mdy(date)) %>%
  subset(date > "2020/08/01")

################################
#Case threshold
################################

peak <- linelist %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(peak_incidence=max(incidence,na.rm=TRUE))

linelist2 <- left_join(linelist, peak, by="country")


df_country3 <- linelist2 %>%
  group_by(country) %>%
  arrange(date) %>% #date
  arrange(country) %>% #character
  mutate(threshold = ifelse(incidence>=peak_incidence*0.50 & incidence > 0, "Peak", 
                     ifelse(incidence>=peak_incidence*0.20 & incidence < peak_incidence*0.50 & incidence > 0, "Increase/Decrease", "Constant")
                     ))

###########################
#Plots
############################

colors <- c('#FE6100', '#785EF0','#648FFF')
levels<-c("Peak", "Increase/Decrease", "Constant")
c<-unique(linelist$country)

# Lineplot for country per metric ####
for (i in 1:length(c)) {
  df_country_line = df_country3 %>%
    filter(country == c[i] )
    inc_col = ggplot(df_country_line, aes(x=date,group=country)) +
      geom_col(aes(y=incidence, fill=threshold), width = 1) + 
      facet_wrap(~country) + 
      theme(
        legend.position= "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text   = element_text(size = 14)) +
      scale_fill_manual(breaks = levels,
                        values= colors,
                        labels = levels) +
      scale_size_manual(values = c(2)) +
      labs(x='', y='Rolling Daily Incidence per capita' ) 
    
    png(paste("Figures/",c[i],".png", sep=""), width=12, height=8, units="in", res=100)
    print(inc_col)
    dev.off()
}


###########################
#Total number per country
############################

aggregate_country <- df_country3 %>%
  group_by(country) %>%
  arrange(country) %>% #character
  dplyr::summarise(peak=length(which(threshold=="Peak")),
                   inc_dec=length(which(threshold=="Increase/Decrease")),
                   constant=length(which(threshold=="Constant")))


################################
#Income groups
################################

countrylist <- read.csv("Data/Dx_country_list2.csv", stringsAsFactors=FALSE)
final <- left_join(aggregate_country, countrylist, by="country")

aggregate_income <- final %>%
  group_by(income_gp) %>%
  dplyr::summarise(peak=sum(peak),
                   inc_dec=sum(inc_dec),
                   constant=sum(constant))

aggregate_country %>% write.csv(.,"Output/Country.csv")
aggregate_income %>% write.csv(.,"Output/Income.csv")
final %>% write.csv(.,"Output/rawfile.csv")
