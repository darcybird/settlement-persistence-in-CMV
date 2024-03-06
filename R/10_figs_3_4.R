library(tidyverse)
library(ggpubr)


#make figs3 ----

bout_slicemod_abs_20 <- readRDS(here::here("data/data-derived/prepAnalysis/bout_slicemod_abs_20.rds"))
group_20 <-readRDS(here::here("data/data-derived/prepAnalysis/20_year_groups.rds"))

group20_years <- group_20 %>% 
  mutate(group = as.integer(group20)) %>% 
  dplyr::select(group, start, end) %>% 
  dplyr::distinct()


sampleSizes <- bout_slicemod_abs_20 %>% 
  dplyr::group_by(group20) %>% 
  dplyr::mutate(n = dplyr::n()) %>% 
  dplyr::select(group20, n) %>% 
  dplyr::distinct()%>% 
  dplyr::left_join(group20_years, by = c("group20" = "group")) %>% 
  dplyr::rename(group = group20) %>% 
  dplyr::arrange(group)


event_sizes <- bout_slicemod_abs_20 %>% dplyr::group_by(event, group20) %>% dplyr::select(event, group20) %>% 
  dplyr::mutate(n = dplyr::n()) %>% distinct() %>% arrange(-group20) %>% 
  dplyr::left_join(group_20 %>% 
                     dplyr::mutate(group20 = as.integer(group20)),
                   by = c("group20" = "group20")) %>% 
  dplyr::mutate(event = factor(event, levels = c(0,1), labels = c("Persistence", "Disoccupation")))

fig3 <-
  ggplot()+
  geom_point(aes(x = (start + 10), y = n, color = event), data = event_sizes) + 
  geom_line(aes(x = (start + 10), y = n, color = event), data = event_sizes)+
  scale_color_manual(values = c("#E69F00", "#56B4E9"))+
  scale_x_continuous(name = "Year AD", breaks = seq(600,1300,100))+
  ylab("Number of Events") +
  xlab("Year AD")+
  theme_bw()+
  geom_vline( aes(xintercept = as.numeric(EndDate)),
              data = pecos_wide) +
  geom_text( aes(x = MidDate,
                 y = 2100,
                 label = pecos),
             vjust = -0.3,
             data = pecos_wide) +
  theme(
    legend.position = c(.02, .8),
    legend.justification = c("left", "top"),
    legend.title = element_blank()
  )



png(filename = "figures/fig3.png", units = "in", width = 6, height = 2.5, res = 300)
fig3
dev.off()


#make figure 4 ----
bouts <-  readRDS(here::here("data/data-derived/prepAnalysis/bouts_noInterbouts_broad.rds"))  %>% 
  dplyr::mutate(contains_center = factor(center, levels = c(0, 1), labels = c( "Hamlet", "Community Center")))


pecos_wide <- 
  tibble::tibble(
    pecos = c( "BMIII", "PI", "PII",  "PIII"), #Bocinsky 2016 exploration/Exploitation ages
    StartDate = c(600, 700, 890,  1145),
    EndDate = c(700, 890,  1145,  1285)) %>% 
  dplyr::mutate(pecos = factor(pecos,
                               levels = unique(pecos),
                               ordered = TRUE),
                MidDate = (EndDate+StartDate) / 2)

fig4 <-
  ggplot()+
  geom_errorbarh(data = bouts,
                 aes(xmin = start, xmax= end, y = boutLength, color = contains_center),
                 linewidth = 0.2)+
  theme_minimal() +
  scale_color_manual(values = c("#E69F00", "#56B4E9"))+
  xlab("Starting and Ending Year for each Occupation Span") +
    ylab("Occupation Span")+
  scale_y_continuous(breaks = c(8, 20, 50, 100, 150, 269)) +
  geom_vline( aes(xintercept = as.numeric(EndDate)),
              data = pecos_wide) +
  geom_text( aes(x = MidDate,
                 y = 270,
                 label = pecos),
             vjust = -0.3,
             data = pecos_wide) +
  # facet_wrap(vars(vepStart), nrow = 1, strip.position ="bottom" ) +
  theme(legend.position = "top",
        legend.title = element_blank(), 
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 10))

fig4

ggplot(data = bouts,
       aes(y = boutLength, x = contains_center))+
  geom_boxplot() +
  stat_compare_means()

bouts %>% group_by(contains_center) %>% 
  dplyr::mutate(n = dplyr::n(),
                median = median(boutLength),
                mean =mean(boutLength),
                min = min(boutLength),
                max = max(boutLength)) %>% 
  dplyr::select(contains_center, n, median, mean, min, max) %>% 
  distinct()



png(filename = "figures/fig4.png", units = "in", width = 6, height = 4, res = 300)
fig4
dev.off()
