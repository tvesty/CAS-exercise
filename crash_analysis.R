library(dplyr)
library(tidyr)
library(plotly)


df <- read.csv("Crash_Analysis_System_(CAS)_data.csv")

year <- df %>%
  group_by(crashYear, crashSeverity) %>%
  tally()

# tally information for slides
total_crashes <- df %>%
  group_by(crashYear) %>%
  tally()

total_death <- df %>%
  group_by(crashYear) %>%
  summarise(fatalCount = sum(fatalCount))

total_crashes_by_severity <- df %>%
  group_by(crashYear, crashSeverity) %>%
  tally() 


# Injury crash
injury <- df %>%
  filter(crashSeverity %in% c("Minor Crash", "Serious Crash"),
         crashYear != 2023,
         region != "") %>% #2023 isn't a complete year yet
  group_by(crashYear, region) %>%
  summarise(minorInjuryCount = sum(minorInjuryCount),
            seriousInjuryCount = sum(seriousInjuryCount))

cost_injury <- injury %>%
  ungroup() %>%
  mutate(Minor = 0.0837 * minorInjuryCount,
         Serious =  0.8989 * seriousInjuryCount) %>%
  group_by(crashYear, region) %>%
  summarise(Minor = sum(Minor),
            Serious = sum(Serious)) %>%
  pivot_longer(cols = c("Minor", "Serious"), names_to = "Condition",
               values_to = "Cost")

# Fatal crash
fatal <- df %>%
  filter(crashSeverity == "Fatal Crash",
         crashYear != 2023,
         region != "") %>% #2023 isn't a complete year yet
  group_by(crashYear, region) %>%
  summarise(fatalCount = sum(fatalCount))

cost_fatal <- fatal %>%
  ungroup() %>%
  mutate(Cost = 4.88 * fatalCount) %>%
  group_by(crashYear, region) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(Condition = "Fatal")

# bind together fatal, serious and minor injuries
cost_crash <- rbind(cost_injury, cost_fatal)

plots_crash <- list()

region <- df %>% select(region) %>% 
  mutate(region = as.character(region)) %>%
  distinct() %>% filter(region != "")


for (i in region$region){
  
  
  plots_crash[[i]] <- (data=cost_crash %>% filter(region == i) %>%
                         rename(Year = crashYear,
                                `Cost (Million NZD)` = Cost) %>%
                         ggplot(aes(x=Year, y=`Cost (Million NZD)`, group = Condition, color=Condition)) +
                         geom_line() +
                         geom_point() +
                         scale_colour_manual(values = rev(c("#0099ff", "#f57e20", "#add136"))))
  
  
}


# Minor/serious/fatal crash cost as a percentage

cost_crash_pct <- cost_crash %>%
  group_by(crashYear, region) %>%
  mutate(total = sum(Cost),
         pct = Cost/total * 100) %>%
  
  mutate(Condition = as.character(Condition)) %>% 
  mutate(Condition = factor(Condition, levels=c("Fatal", "Serious", "Minor"))) # reordering
  

plots_crash_pct <- list()

region <- df %>% select(region) %>% 
  mutate(region = as.character(region)) %>%
  distinct() %>% filter(region != "")


for (i in region$region){
  
  
  plots_crash_pct[[i]] <- (data=cost_crash_pct %>% filter(region == i) %>%
                         rename(Year = crashYear,
                                `Percentage (%)` = pct) )%>%
                         ggplot(aes(x = Year, y = `Percentage (%)`, fill=Condition)) +
                         geom_bar(position="stack", stat="identity") + 
                         coord_flip() +
                          scale_fill_manual(values = rev(c("#0099ff", "#f57e20", "#add136")))
  
  
}




# Pedestrian fatal crash
pedestrian <- df %>%
  filter(crashSeverity != "Non-Injury Crash",
         !is.na(pedestrian),
         crashYear != 2023) %>% # 2023 isn't a complete year yet
  group_by(crashYear, region, crashSeverity, pedestrian, minorInjuryCount,
           seriousInjuryCount, fatalCount) %>%
  tally()
  # summarise(pedestrian = sum(pedestrian, na.rm=TRUE),
  #           minorInjuryCount = sum(minorInjuryCount),
  #           seriousInjuryCount = sum(seriousInjuryCount),
  #           fatalCount = sum(fatalCount))
  
pedestrian_fatal <- pedestrian %>%
  filter(pedestrian == fatalCount & 
           minorInjuryCount == 0 & seriousInjuryCount == 0)


cost_pedestrian_fatal <- pedestrian_fatal %>%
  ungroup() %>%
  select(crashYear, region, pedestrian, n) %>%
  mutate(VOSL = 4.88 * pedestrian *n) %>%
  group_by(crashYear, region) %>%
  summarise(VOSL = sum(VOSL))


plots <- list()

region <- df %>% select(region) %>% 
  mutate(region = as.character(region)) %>%
  distinct() %>% filter(region != "")



for (i in region$region){
  
  
  plots[[i]] <- (data=cost_pedestrian_fatal %>% filter(region == i) %>%
    rename(Year = crashYear,
           `VOSL (Million NZD)` = VOSL) %>%
    ggplot(aes(x=Year, y=`VOSL (Million NZD)`)) +
    geom_line() +
    geom_point()) 
  

}


# Fatal pedestrian crash cost pct
# Fatal pedestrian over fatal crash

cost_pedestrian_fatal_pct <- cost_pedestrian_fatal %>%
  full_join(cost_fatal, by = c("crashYear", "region")) %>%
  select(-Condition) %>%
  mutate(pct = VOSL/Cost * 100) %>%
  mutate(pct = ifelse(is.na(pct),0,pct))

plots_pedestrian_fatal <- list()

region <- df %>% select(region) %>% 
  mutate(region = as.character(region)) %>%
  distinct() %>% filter(region != "")



for (i in region$region){
  
  
  plots_pedestrian_fatal[[i]] <- (data=cost_pedestrian_fatal_pct %>% filter(region == i) %>%
                   rename(Year = crashYear,
                          `Percentage of total fatal crash cost` = pct) %>%
                   ggplot(aes(x=Year, y=`Percentage of total fatal crash cost`)) +
                   geom_line() +
                   geom_point()) 
  
  
}



# p <- ggplot(data=cost_pedestrian_fatal, aes(x=crashYear, y=VOSL, color = region)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~ region, scales = "free")
# 
# fig <- ggplotly(p)
# 
# fig


# predict cost

# logistic_model <- glm(VOSL ~ crashYear,
#                       data = cost_pedestrian_fatal,
#                       family = "binomial")
