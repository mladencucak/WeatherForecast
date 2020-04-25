
#######################################################
# Quick analysis of the weather forecast data
#######################################################

# Convert scripts from the weather forecast project
# Spread this data set so it is in same format as previous analysis  
# Check the solar radiation data




#######################################################
# Run the models
#######################################################

source(here::here("scr","lib",  "pkg.R"))
source(here::here("scr", "lib", "funs.R"))
load(here::here("out", "fore", "fore_dat_full.Rdata"))

# Add geo references to the data
load(file = here::here("dat", "locations.Rdata"))
df_loc$stna[df_loc$stna == "JohnstownII"|df_loc$stna == "JOHNSTOWNII"] <- "Johnstown"
df_loc$stna[df_loc$stna == "Oak Park"|df_loc$stna == "OAK PARK"|df_loc$stna == "Oak_Park"] <- "Oakpark"
df_loc$stna[df_loc$stna == "Moore Park"|df_loc$stna == "Moore_Park"|df_loc$stna == "MOORE PARK"] <- "Moorepark"
df_loc$stna[df_loc$stna == "DUNSANY"] <- "Dunsany"
df_loc$stna[df_loc$stna == "GURTEEN"] <- "Gurteen"

#prop succesfull
prop_succesfull_data <- 
  c(length(data_ls)/c(time %>% length()*length(unique(stations))))
length(data_ls)
c(time %>% length()*length(unique(stations))) -length(data_ls)

100 - round(prop_succesfull_data *100 ,2)

data_ls <- 
  lapply(data_ls, function(x) {
    x <- left_join(x, df_loc, by ="stna")
    return(x)
  })

#####
#convert from w/m2 to MJ/m2
#conversion https://cliflo-niwa.niwa.co.nz/pls/niwp/wh.do_help?id=ls_rad
data_ls <- 
  lapply(data_ls, function( fundf){
    fundf$sol_rad <- fundf$sol_rad * 0.0036
    return(fundf)
  })

# wider format for the comparison
full_data <- 
  lapply(data_ls, function(fundf){
    fundfob <- fundf[fundf$set == "obs",]
    colnames(fundfob)[ colnames(fundfob) %in%c("temp","rhum","sol_rad","wdsp","rain" )] <- 
      paste0(c("temp","rhum","sol_rad","wdsp","rain" ), "_ob")
    bind_cols(fundf[fundf$set == "fore",], 
              fundfob[, grep("_ob", colnames(fundfob))]) %>% 
      filter(hour_step %in% 1:240) 
  }) %>% 
  bind_rows() 

#Stations
full_data %>% 
  group_by(stna) %>% 
  summarise(Latitude = unique(lat),
            Longitude = unique(long)) %>% 
  write_csv(here::here("out", "fore", "Stations.csv"))


####################################################################################
#RH errors overall
####################################################################################


full_data %>% 
  group_by(hour_step, for_date) %>% 
  mutate(rhum_ob = as.numeric(rhum_ob)) %>% 
  summarise( rmse = rmse(rhum_ob, rhum),
             mse = mse(rhum_ob, rhum),
             rsq = cor(rhum_ob, rhum)
  )%>% 
  ungroup() %>% 
  mutate(var = "rhum") ->xx 

full_data %>% 
  group_by(day_step) %>% 
  mutate(rhum_ob = as.numeric(rhum_ob)) %>% 
  summarise( rmse = rmse(rhum_ob, rhum),
             mse = mse(rhum_ob, rhum),
             rsq = cor(rhum_ob, rhum),
             ccc = epiR::epi.ccc(
               rhum_ob,
               rhum,
               ci = "z-transform",
               conf.level = 0.95,
               rep.measure = FALSE
             )$rho.c[, 1])%>% 
  ungroup()  %>% 
  mutate(var = "rhum")->errors_daily_rh


#Relative humidity
ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSEs Relatve Humidity", 
       subtitle="Tufte boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-2, label =as.character(round(errors_daily_rh$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh rmse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)

ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSEs Relatve Humidity", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-2, label =as.character(round(errors_daily_rh$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh mse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)


ggplot(xx,aes(factor(hour_step), rsq))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSEs Relatve Humidity", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rsq)-2, label =as.character(round(errors_daily_rh$rsq,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "rh rsq .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)



######################################################
#Temp
#################################################################
full_data %>% 
  group_by(hour_step, for_date) %>% 
  summarise( rmse = rmse(temp_ob, temp),
             mse = mse(temp_ob, temp),
             rsq = cor(temp_ob, temp)) %>% 
  ungroup() %>% 
  mutate(var = "temp") ->xx 

full_data %>% 
  group_by(day_step) %>% 
  mutate(temp_ob = as.numeric(temp_ob)) %>% 
  summarise( rmse = rmse(temp_ob, temp),
             mse = mse(temp_ob, temp),
             rsq = cor(temp_ob, temp),
             ccc = epiR::epi.ccc(
               temp_ob,
               temp,
               ci = "z-transform",
               conf.level = 0.95,
               rep.measure = FALSE
             )$rho.c[, 1]) %>% 
  ungroup() %>% 
  mutate(var = "temp") ->errors_daily_temp


ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Temperature", 
       subtitle="Boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily_temp$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "temp rmse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)


ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="mse Temperature", 
       subtitle="Boxplot of mse hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="mse")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-max(xx$mse)*0.01, label =as.character(round(errors_daily_temp$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "temp mse .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)



ggplot(xx,aes(factor(hour_step), rsq))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title=expression("r" ~ R^2 ~  "Temperature"), 
       subtitle="Boxplot of rsq hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="rsq")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rsq)-max(xx$rsq)*0.01, label =as.character(round(errors_daily_temp$rsq,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "tmp rsq .png"),
         width = 8, 
         height = 4,
         units = "in",
         dpi = 2000)




#####################################
#Solar Radiation
#########################################

full_data %>% 
  group_by(hour_step, for_date) %>% 
  mutate(temp_ob = as.numeric(temp_ob)) %>% 
  summarise(     rmse = rmse(sol_rad, sol_rad_ob),
                 mse = mse(sol_rad, sol_rad_ob),
                 rsq = cor(sol_rad, sol_rad_ob),
                 ccc = epiR::epi.ccc(
                   sol_rad,
                   sol_rad_ob,
                   ci = "z-transform",
                   conf.level = 0.95,
                   rep.measure = FALSE
                 )$rho.c[, 1]
  ) %>% 
  ungroup() %>% 
  mutate(var = "sol_rad") ->xx 

errors_daily_sol <- 
  full_data %>%
  group_by(id,day_step) %>%
  mutate(sol_rad_ob = as.numeric(sol_rad_ob)) %>%
  summarise(sol_rad = sum (sol_rad),
            sol_rad_ob = sum(sol_rad_ob)) %>%
  ungroup() %>%
  group_by(day_step) %>%
  summarise(
    rmse = rmse(sol_rad, sol_rad_ob),
    mse = mse(sol_rad, sol_rad_ob),
    rsq = cor(sol_rad, sol_rad_ob),
    ccc = epiR::epi.ccc(
      sol_rad,
      sol_rad_ob,
      ci = "z-transform",
      conf.level = 0.95,
      rep.measure = FALSE
    )$rho.c[, 1]
  ) %>%
  ungroup() %>%
  mutate(var = "sol_rad")

ggplot(xx,aes(factor(hour_step), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Solar Radiation", 
       subtitle="Boxplot of RMSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))

ggplot(xx,aes(factor(hour_step), mse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="MSE Solar Radiation", 
       subtitle="Boxplot of MSE hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="MSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$mse)-max(xx$mse)*0.01, label =as.character(round(errors_daily$mse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))


ggplot(errors_daily,aes(factor(daystep), rmse))+
  geom_vline(xintercept = seq(24,240,24), linetype="dotted", size=0.5)+
  geom_tufteboxplot() + 
  # theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="RMSE Solar Radiation", 
       subtitle="Boxplot of ccc hourly lead time as factor.",
       # caption="Source: mpg",
       x="Lead hours",
       y="RMSE")+
  theme_tufte()+ 
  annotate("text",x = seq(12,228,24), y = max(xx$rmse)-max(xx$rmse)*0.01, label =as.character(round(errors_daily$rmse,2)) )+
  scale_x_discrete(breaks= seq(0,240,12),labels= seq(0,240,12))



bind_rows(errors_daily_rh, errors_daily_temp, errors_daily_sol) %>% 
  reshape2::melt(
    .,
    id.vars = c("var",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = var))+
  theme_article()+
  theme(legend.position = "right") +
  facet_wrap(~ind, scales = "free", ncol =1 )+
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1))+
  labs(
    colour = "Idicator",
    title = "Validation of forecasted risk",
    x = "Lead time (days)"
  )+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "daily all indicators .png"),
         width = 6, 
         height = 4,
         units = "in",
         dpi = 2000)



pcccfore <- 
  bind_rows(errors_daily_rh, errors_daily_temp, errors_daily_sol) %>% 
  reshape2::melt(
    .,
    id.vars = c("var",  "day_step"),
    # measure.vars = models,
    variable.name = "ind",
    value.name = "skill",
    factorsAsStrings  = FALSE
  ) %>% 
  mutate(var = ifelse(var== "rhum", "Relative Humidity (%)", 
                      ifelse(var == "sol_rad", "Solar Radiation (MJ/m2/day)",
                             ifelse(var == "temp" ,  "Temperature (°C)", "")))) %>% 
  filter(ind == "ccc") %>% 
  ggplot()+
  geom_line(aes(day_step, skill, color = var))+
  theme_bw()+
  theme(legend.position = "right") +
  scale_x_continuous(breaks = seq(1,10,1),labels = seq(1,10,1),minor_breaks = seq(1, 10, 1))+
  scale_y_continuous(limits = c(0,1.03),breaks = seq(0,1,.2),labels = seq(0,1,.2),minor_breaks = seq(0,1,.2))+
  scale_color_manual(values=c("#56B4E9", "#E69F00", "salmon"))+
  labs(
    colour = "Forecasted variable:",
    x = "Lead time (days)",
    y = "Concordnace Correlation Coefficient"
  )+
  theme(
    text = element_text(size=12),
    legend.position = c(.77, .82),
    legend.text = element_text(size = 11),
    legend.title = element_text(11),
    legend.key.width = unit(1, "cm")
  )+
  ggsave(filename = here::here("out", "fore", "fig", "wth_vars", "daily .png"),
         width = 7, 
         height = 4,
         units = "in",
         dpi = 800)

saveRDS(pcccfore, file =  here::here("out", "fore", "fig", "wth_vars", "CCCdaily_wth_vars.png"))



# Split the data into data frames for each id to run the model 
# Model needs to be run in that way to sync the outputs
data_ls <- 
  data.table::rbindlist(data_ls)
data_ls <- 
  split(data_ls, data_ls$id)

sapply(data_ls, function(x) sum(is.na(x[,c( "temp", "rhum")])))%>% as.vector()

nas <- sapply(data_ls, function(x) mean(is.na(x[,c( "temp", "rhum")])))%>% as.vector() %>% round(3)
sum(nas<0.01)
length(data_ls)

