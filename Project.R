#a) 
covid19 <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
hospitalbeds <- read_csv("WHS6_102.csv")
demographics <- read_csv("demographics.csv")

#b)
covid19_tidy <- covid19 %>% pivot_longer(-(1:4), names_to = "Days", values_to = "Cases")
covid19_deaths_tidy <- covid19_deaths %>% pivot_longer(-(1:4), names_to = "Days", values_to = "Deaths")

#match_country_names
covid19_tidy <- covid19_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Korea, South", "South Korea"))
covid19_tidy <- covid19_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Ireland", "United Kingdom"))
covid19_deaths_tidy <- covid19_deaths_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Korea, South", "South Korea"))
covid19_deaths_tidy <- covid19_deaths_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Ireland", "United Kingdom"))

#c)
covid19_tidy <- covid19_tidy %>% group_by(`Country/Region`) %>% summarise(cases=max(Cases))
covid19_deaths_tidy <- covid19_deaths_tidy %>% group_by(`Country/Region`) %>% summarise(deaths=max(Deaths))
hospitalbeds_tidy <- hospitalbeds %>% rename('Country/Region' = Country)
demographics <- demographics %>% rename('Country/Region' = 'Country Name')

#d)
covid19_tidy <- covid19_tidy %>% inner_join(covid19_deaths_tidy)
covid19_deaths_tidy <- covid19_tidy %>% inner_join(covid19_deaths_tidy)
covid19_tidy <- covid19_tidy %>% mutate(deathrate=deaths/cases) %>% select(-"deaths")

#e)
hospitalbeds_tidy <- hospitalbeds_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
hospitalbeds_tidy <- hospitalbeds_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Iran (Islamic Republic of)", "Iran"))
hospitalbeds_tidy <- hospitalbeds_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Democratic People's Republic of Korea", "South Korea"))
hospitalbeds_tidy <- hospitalbeds_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Republic of Korea", "South Korea"))
hospitalbeds_tidy <- hospitalbeds_tidy %>% group_by(`Country/Region`) %>% filter(Year == max(Year)) %>% rename(beds = `Hospital beds (per 10 000 population)`) %>% select(-"Year")

#f) part 1
demographics_tidy <- demographics %>% drop_na()
demographics_tidy <- demographics %>% select(-"Country Code") %>% pivot_wider(names_from = "Series Code", values_from = YR2015)
demographics_tidy[is.na(demographics_tidy)] <- 0
demographics_tidy <- demographics_tidy %>% 
  mutate(SP.POP.80UP=SP.POP.80UP.FE+SP.POP.80UP.MA) %>% 
  mutate(SP.POP.65UP.IN=SP.POP.65UP.FE.IN+SP.POP.65UP.MA.IN) %>% 
  mutate(SP.POP.0014.IN=SP.POP.0014.FE.IN+SP.POP.0014.MA.IN) %>% 
  mutate(SP.POP.1564.IN=SP.POP.1564.FE.IN+SP.POP.1564.MA.IN) %>% 
  mutate(SP.DYN.AMRT=SP.DYN.AMRT.FE+SP.DYN.AMRT.MA) %>% 
  mutate(SP.POP.TOTL.IN=SP.POP.TOTL.FE.IN+SP.POP.TOTL.MA.IN) %>% 
  select(-contains(".FE")) %>% 
  select(-contains(".MA"))

#match_country_names
demographics_tidy <- demographics_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Korea, Dem. People's Rep.", "South Korea"))
demographics_tidy <- demographics_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Korea, Rep.", "South Korea"))
demographics_tidy <- demographics_tidy %>% mutate(`Country/Region` = replace(`Country/Region`, `Country/Region` == "Iran, Islamic Rep.", "Iran"))

#f) part 2
demographics_tidy <- demographics_tidy %>% group_by(`Country/Region`) %>% summarise('SP.DYN.LE00.IN'=max(SP.DYN.LE00.IN), 'SP.URB.TOTL'=max(SP.URB.TOTL), 
                                                                                    'SP.POP.TOTL'=max(SP.POP.TOTL), 'SP.POP.80UP'=sum(SP.POP.80UP), 
                                                                                    'SP.POP.0014.IN'=sum(SP.POP.0014.IN),'SP.POP.1564.IN'=sum(SP.POP.1564.IN))
demographics_tidy[demographics_tidy == 0] <- NA
demographics_tidy <- demographics_tidy %>% drop_na()

#group all tables together
covid19_tidy <- covid19_tidy %>% inner_join(hospitalbeds_tidy) %>% inner_join(demographics_tidy)

#Liner modeling and transformation
model1 <- lm(data = covid19_tidy, deathrate~cases)
summary(model1)
r1 <- 0.002445

cases_tranformation <- (covid19_tidy$cases)^2
model1_transformation <- lm(data = covid19_tidy, deathrate~cases_tranformation)
summary(model1_transformation)
r1_transformation <- 0.0001176
  
model2 <- lm(data = covid19_tidy, deathrate~beds)
summary(model2)
r2 <- 0.007315

beds_tranformation <- (covid19_tidy$beds)^2
model2_transformation <- lm(data = covid19_tidy, deathrate~beds_tranformation)
summary(model2_transformation)
r2_transformation <- 0.003992

model3 <- lm(data = covid19_tidy, deathrate~SP.POP.80UP)
summary(model3)
r3 <- 0.06

SP.POP.80UP_tranformation <- (covid19_tidy$SP.POP.80UP)^2
model3_transformation <- lm(data = covid19_tidy, deathrate~SP.POP.80UP_tranformation)
summary(model3_transformation)
r3_transformation <- 0.05727

model4 <- lm(data = covid19_tidy, deathrate~SP.URB.TOTL)
summary(model4)
r4 <- 0.07078

SP.URB.TOTL_tranformation <- (covid19_tidy$SP.URB.TOTL)^2
model4_transformation <- lm(data = covid19_tidy, deathrate~SP.URB.TOTL_tranformation)
summary(model4_transformation)
r4_transformation <- 0.05833

model5 <- lm(data = covid19_tidy, deathrate~SP.POP.TOTL)
summary(model5)
r5 <- 0.04102

SP.POP.TOTL_tranformation <- (covid19_tidy$SP.POP.TOTL)^2
model5_transformation <- lm(data = covid19_tidy, deathrate~SP.POP.TOTL_tranformation)
summary(model5_transformation)
r5_transformation <- 0.02836

rvalues <- data.frame(Predictor=c("cases","beds","SP.POP.80UP","SP.URB.TOTL","SP.POP.TOTL"), rvalues=(c(r1,r2,r3,r4,r5)))
ggplot(data = rvalues) + geom_col(aes(x=Predictor, y=rvalues)) 

rvalues_transformed <- data.frame(Predictor=c("cases","beds","SP.POP.80UP","SP.URB.TOTL","SP.POP.TOTL"), rvalues=(c(r1_transformation,r2_transformation,r3_transformation,r4_transformation,r5_transformation)))
ggplot(data = rvalues_transformed) + geom_col(aes(x=Predictor, y=rvalues)) 


#plot of confirmed cases and deaths
ggplot(data = covid19_deaths_tidy) + geom_point(aes(x=cases, y=deaths)) + ylim(0, 5000) + xlim(0, 200000)