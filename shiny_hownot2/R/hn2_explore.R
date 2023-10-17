library(dplyr)
library(ggplot2)
library(data.table)
library(stats)

rm(list=ls())
cat('\f')
gc()

# wd
wd <- list(img    = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data/images", 
           data   = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/data", 
           R      = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/R", 
           output = "C:/Users/bende/Documents/R/play/shiny_misc/shiny_hownot2/output", 
           shiny  = NA)

# load data----

setwd(wd$output)
load("hownot2.RData")
setwd(wd$R)

# explore----


#dfout_hardiseasy_640hz

ggplot(data = dfout_hardiseasy_640hz, 
       aes(x = quickdraw, y = belayer + climber)) + 
  geom_point()+
  geom_smooth(method = "lm", se = F, 
              aes(color = "lm()")) +
  geom_function(fun = function(x)x, 
                aes(color = "y = x"))+
  labs(title = "Force Distribution Between Climber, Belayer and Quickdraw\n on Lead-Climb Fall")

lm(quickdraw ~ belayer + climber, dfout_hardiseasy_640hz) %>% plot
lm(quickdraw ~ belayer + climber, dfout_hardiseasy_640hz) %>% summary

#dfout_ropesinshaprhangers

dfout_ropesinshaprhangers
dforh <- dfout_ropesinshaprhangers

dforh$hanger_f    <- factor(dforh$hanger)
dforh$rope_f      <- factor(dforh$rope, 
                         levels = c("Static", "Dynamic"))
dforh$config_f    <- factor(dforh$configuration)
dforh$broke.loc_f <- dforh$notes %>%
  gsub("^Broke at |^Broke in |^Broke on |!", 
       "", .)

ggplot() + 
  geom_boxplot(data = dforh, 
                 aes(x = rope, y = as.numeric(results.Kn), 
                     group = rope))

pre.dnorm <- dforh %>%
  mutate(., 
         results.Kn = as.numeric(results.Kn)) %>%
  group_by(rope_f, 
           config_f) %>%
  summarise(n = n(), 
            avg_kn = mean(results.Kn), 
            sd_kn = sd(results.Kn)) %>%
  mutate(., 
         config_f = ifelse(config_f == "For rappel", "rappel", "fig_8")) 




# generate random values based on normal distribution from mean and sd
df_sharp.dist <- NULL  
for(i in 1:nrow(pre.dnorm)){
  df_sharp.dist <- rbind(df_sharp.dist, 
                         data.frame(grp = paste(pre.dnorm$rope_f[i], 
                                                pre.dnorm$config_f[i], 
                                                collapse = " // ", sep = " // "), 
                                    avg = pre.dnorm$avg_kn[i], 
                                    sd  = pre.dnorm$sd_kn[i], 
                                    rnorm1 = rnorm(n = 10000, 
                                                   mean = pre.dnorm$avg_kn[i], 
                                                   sd   = pre.dnorm$sd_kn[i]))) %>% 
    as_tibble()
}


ggplot() + 
  geom_density(data = df_sharp.dist, alpha = 0.7,
               aes(x = rnorm1, fill = grp), 
               color = NA)+
  geom_vline(data = summarise(group_by(df_sharp.dist, grp, avg)), 
             aes(xintercept = avg, color = "average"), 
             linetype = 2232) +
  theme(legend.position = "bottom", 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) +
  facet_wrap(grp~., scales = "free_y")+
  scale_x_continuous(name = "Kn")+
  scale_y_continuous(name = "Density (not scaled)")
 



# #dfout_cowstail (data is ruined)----
# dfout_cowstail
# 
# ggplot() +
#   geom_point(data = dfout_cowstail[dfout_cowstail$filename != 
#                                      "cows_tail_drop_test_300lbs.jpg",], 
#              aes(x = material, y = kn)) +
#   facet_grid(length~knot)

#dfout_crossloadcarabiners
