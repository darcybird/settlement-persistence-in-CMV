library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(corrplot)
library(magrittr)
library(car)

modifiedAddRow <- function(data, addedRow, timeperiod, center){
  data %>% 
    dplyr::add_row(
      timeperiod = as.numeric(paste(timeperiod)),
      center = as.numeric(paste(center)),
      model = as.character(summary(addedRow)$call[2]),
      `adj R2` = round(summary(addedRow)$adj.r.squared, digits = 2),
      summary(addedRow)$coefficients %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "variable") %>% 
        dplyr::mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 6),
                      Estimate = round(Estimate, digits = 3)) %>% 
        nest_by(.key = "results"))
}

d <- readRDS(here::here("data/data-derived/bout_relative_scaled.rds")) 

d %>% ggplot(aes(x = Δ_std_regionalPop, y = Δ_std_fieldContention)) + 
  geom_point() + geom_smooth(method = "lm")

lm(Δ_std_fieldContention~Δ_std_regionalPop, d) %>% summary()



ggplot(d, aes(x = Δ_std_nHouse, fill = factor(vep)))+
  geom_histogram(binwidth = 0.2)+
  facet_wrap(vars(center), nrow = 2, scale = "free_y")+
  geom_vline(xintercept = 0)

# ph 6  ----
ph6_0 <- d %>% dplyr::filter(vep == 6, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph6_1 <- d %>% dplyr::filter(vep == 6, center == 1)%>% dplyr::select(-cell, -center, -vep)


#ph6 non-centers
# corrplot(cor(ph6_0, use="pairwise.complete.obs"), method = "number", type = "lower" )

ph6_0 %<>% dplyr::select(-Δ_greatkiva, -Δ_plaza, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_towers, -Δ_reservoir) 

# Fit the full model 
full.model.ph6_0 <- lm(log10_boutLength ~., data = ph6_0)
# Stepwise regression model
step.model.ph6_0 <- stepAIC(full.model.ph6_0, direction = "both", 
                      trace = FALSE, k = 3.8415)
summary(step.model.ph6_0)

car::vif(step.model.ph6_0)

stepwise_relative <- tibble::tibble(
  timeperiod = 6,
  center = 0,
  model = as.character(summary(step.model.ph6_0)$call[2]),
  `adj R2` = round(summary(step.model.ph6_0)$adj.r.squared, digits = 2),
  summary(step.model.ph6_0)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "variable") %>% 
    dplyr::mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 7),
                  Estimate = round(Estimate, digits = 3)) %>% 
    nest_by(.key = "results")
)


#ph6 centers
# corrplot(cor(ph6_1, use="pairwise.complete.obs"),method = "number", type = "lower" )

ph6_1 %<>% dplyr::select(-Δ_greatkiva, -Δ_plaza, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_towers, -Δ_reservoir) 


# Fit the full model 
full.model.ph6_1 <- lm(log10_boutLength ~., data = ph6_1)
# Stepwise regression model
step.model.ph6_1  <- stepAIC(full.model.ph6_1 , direction = "both", 
                      trace = FALSE, k = 3.8415)
summary(step.model.ph6_1)

car::vif(step.model.ph6_1)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph6_1, timeperiod = 6, center = 1)

# ph 7 ----

ph7_0 <- d %>% dplyr::filter(vep == 7, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph7_1 <- d %>% dplyr::filter(vep == 7, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph7 non-centers
# corrplot(cor(ph7_0, use="pairwise.complete.obs"), method = "number",type = "lower" )

ph7_0 %<>% dplyr::select(-Δ_greatkiva, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_towers, -Δ_reservoir) 

# Fit the full model 
full.model.ph7_0 <- lm(log10_boutLength ~., data = ph7_0)
# Stepwise regression model
step.model.ph7_0 <- stepAIC(full.model.ph7_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph7_0)

car::vif(step.model.ph7_0)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph7_0, timeperiod = 7, center = 0)



#ph7 centers
# corrplot(cor(ph7_1, use="pairwise.complete.obs"),method = "number", type = "lower" )

ph7_1 %<>% dplyr::select(-Δ_greatkiva, -Δ_plaza, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_reservoir) 


# Fit the full model 
full.model.ph7_1 <- lm(log10_boutLength ~., data = ph7_1)
# Stepwise regression model
step.model.ph7_1  <- stepAIC(full.model.ph7_1 , direction = "both", 
                             trace = FALSE, k = log(nrow(ph7_1)) )
summary(step.model.ph7_1)

car::vif(step.model.ph7_1)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph7_1, timeperiod = 7, center = 1)


# ph 8 ----

ph8_0 <- d %>% dplyr::filter(vep == 8, center == 0) %>% dplyr::select(-cell, -center,-vep)
ph8_1 <- d %>% dplyr::filter(vep == 8, center == 1)%>% dplyr::select(-cell, -center,-vep)

#ph8 non-centers
# corrplot(cor(ph8_0, use="pairwise.complete.obs"),method = "number",type = "lower" )

ph8_0 %<>%  dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_reservoir)

# Fit the full model 
full.model.ph8_0 <- lm(log10_boutLength ~., data = ph8_0)
# Stepwise regression model
step.model.ph8_0 <- stepAIC(full.model.ph8_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph8_0)

car::vif(step.model.ph8_0)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph8_0, timeperiod = 8, center = 0)



#ph8 centers
# corrplot(cor(ph8_1, use="pairwise.complete.obs"), method = "number", type = "lower" )

ph8_1 %<>% dplyr::select(-Δ_oversizepit, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_reservoir) 

# Fit the full model 
full.model.ph8_1 <- lm(log10_boutLength ~., data = ph8_1)
# Stepwise regression model
step.model.ph8_1  <- stepAIC(full.model.ph8_1 , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_1)

car::vif(step.model.ph8_1)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph8_1, timeperiod = 8, center = 1)

# ph 9 ----

ph9_0 <- d %>% dplyr::filter(vep == 9, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph9_1 <- d %>% dplyr::filter(vep == 9, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph9 non-centers
# corrplot(cor(ph9_0, use="pairwise.complete.obs"),method = "number", type = "lower" )

ph9_0 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_towers, -Δ_reservoir) 

# Fit the full model 
full.model.ph9_0 <- lm(log10_boutLength ~., data = ph9_0)
# Stepwise regression model
step.model.ph9_0 <- stepAIC(full.model.ph9_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph9_0)

car::vif(step.model.ph9_0)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph9_0, timeperiod = 9, center = 0)



#ph9 centers
# corrplot(cor(ph9_1, use="pairwise.complete.obs"), method = "number", type = "lower" )

ph9_1 %<>% dplyr::select(-Δ_greatkiva, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_towers, -Δ_reservoir) 

# Fit the full model 
full.model.ph9_1 <- lm(log10_boutLength ~., data = ph9_1)
# Stepwise regression model
step.model.ph9_1  <- stepAIC(full.model.ph9_1 , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_1)

car::vif(step.model.ph9_1)

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph9_1, timeperiod = 9, center = 1)

# ph 10 ----

ph10_0 <- d %>% dplyr::filter(vep == 10, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph10_1 <- d %>% dplyr::filter(vep == 10, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph10 non-centers
# corrplot(cor(ph10_0, use="pairwise.complete.obs"),  method = "number",   type = "lower" )

ph10_0 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall)


# Fit the full model 
full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0)
# Stepwise regression model
step.model.ph10_0 <- stepAIC(full.model.ph10_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph10_0)

car::vif(step.model.ph10_0)

# remove

full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion))

step.model.ph10_0a <- stepAIC(full.model.ph10_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph10_0a)

car::vif(step.model.ph10_0a)

# compare

full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0 %>% dplyr::select(-Δ_std_localCatchmentQuality))

step.model.ph10_0b <- stepAIC(full.model.ph10_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph10_0b)

car::vif(step.model.ph10_0b)

#b is simpler. has plaza instead of greatkiva + oversizepit

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph10_0b, timeperiod = 10, center = 0)



#ph10 centers
# corrplot(cor(ph10_1, use="pairwise.complete.obs"),method = "number", type = "lower" )

ph10_1 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall, -Δ_reservoir)

# Fit the full model 
full.model.ph10_1 <- lm(log10_boutLength ~., data = ph10_1) 
# Stepwise regression model
step.model.ph10_1  <- stepAIC(full.model.ph10_1  , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph10_1)

car::vif(step.model.ph10_1)

# remove

full.model.ph10_1a <- lm(log10_boutLength ~., data = ph10_1 %>% dplyr::select(-Δ_std_localCatchmentQuality))

step.model.ph10_1a <- stepAIC(full.model.ph10_1a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_1a) #just nhouse and field contention, 0.3455

car::vif(step.model.ph10_1a)

#check the other
full.model.ph10_1b <- lm(log10_boutLength ~., data = ph10_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion))

step.model.ph10_1b <- stepAIC(full.model.ph10_1b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_1b)

car::vif(step.model.ph10_1b)

#they're the same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph10_1a, timeperiod = 10, center = 1)

# ph 11 ----

ph11_0 <- d %>% dplyr::filter(vep == 11, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph11_1 <- d %>% dplyr::filter(vep == 11, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph11 non-centers
# corrplot(cor(ph11_0, use="pairwise.complete.obs"),  method = "number",  type = "lower" )

ph11_0 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall)

# Fit the full model 
full.model.ph11_0 <- lm(log10_boutLength ~., data = ph11_0)
# Stepwise regression model
step.model.ph11_0 <- stepAIC(full.model.ph11_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph11_0)

car::vif(step.model.ph11_0)


stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph11_0, timeperiod = 11, center = 0)



#ph11 centers
# corrplot(cor(ph11_1, use="pairwise.complete.obs"), method = "number",  type = "lower" )


ph11_1 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_encwall)

# Fit the full model 
full.model.ph11_1 <- lm(log10_boutLength ~., data = ph11_1 ) 
# Stepwise regression model
step.model.ph11_1  <- stepAIC(full.model.ph11_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_1)

car::vif(step.model.ph11_1)

#remove high vif
full.model.ph11_1a <- lm(log10_boutLength ~., data = ph11_1 %>% dplyr::select(-Δ_std_localCatchmentQuality    )) 
# Stepwise regression model
step.model.ph11_1a  <- stepAIC(full.model.ph11_1a  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_1a)

car::vif(step.model.ph11_1a)


full.model.ph11_1b <- lm(log10_boutLength ~., data = ph11_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph11_1b  <- stepAIC(full.model.ph11_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph11_1b)

car::vif(step.model.ph11_1b)

#they're the same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph11_1a, timeperiod = 11, center = 1)


# ph 12 ----
ph12_0 <- d %>% dplyr::filter(vep == 12, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph12_1 <- d %>% dplyr::filter(vep == 12, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph12 non-centers
# corrplot(cor(ph12_0, use="pairwise.complete.obs"), method = "number", type = "lower" )

ph12_0 %<>% dplyr::select(-Δ_plaza, -Δ_greathouse, -Δ_multiwalls, -Δ_encwall)

# Fit the full model 
full.model.ph12_0 <- lm(log10_boutLength ~., data = ph12_0 ) 
# Stepwise regression model
step.model.ph12_0 <- stepAIC(full.model.ph12_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph12_0)

car::vif(step.model.ph12_0)

# remove high vif

full.model.ph12_0a <- lm(log10_boutLength ~., data = ph12_0 %>% dplyr::select(-Δ_std_localCatchmentQuality))
# Stepwise regression model
step.model.ph12_0a <- stepAIC(full.model.ph12_0a, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph12_0a)

car::vif(step.model.ph12_0a)

#remove other vif

full.model.ph12_0b <- lm(log10_boutLength ~., data = ph12_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph12_0b <- stepAIC(full.model.ph12_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_0b)

car::vif(step.model.ph12_0b)
#nearly identical, with very similar adj R^2, but b is simpler. same institutions!

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph12_0b, timeperiod = 12, center = 0)



#ph12 centers
# corrplot(cor(ph12_1, use="pairwise.complete.obs"), method = "number",  type = "lower" )

ph12_1 %<>% dplyr::select(-Δ_plaza, -Δ_multiwalls, -Δ_encwall, -Δ_towers)

# Fit the full model 
full.model.ph12_1 <- lm(log10_boutLength ~., data = ph12_1)
# Stepwise regression model
step.model.ph12_1  <- stepAIC(full.model.ph12_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_1)

car::vif(step.model.ph12_1)

#remove high vif
full.model.ph12_1a <- lm(log10_boutLength ~., data = ph12_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion ))
# Stepwise regression model
step.model.ph12_1a  <- stepAIC(full.model.ph12_1a  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_1a)

car::vif(step.model.ph12_1a)# just nhouse and regional pop, r^2 0.8


# only the one above 5
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph12_1a, timeperiod = 12, center = 1)

# ph 13 ----

ph13_0 <- d %>% dplyr::filter(vep == 13, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph13_1 <- d %>% dplyr::filter(vep == 13, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph13 non-centers
# corrplot(cor(ph13_0, use="pairwise.complete.obs"),  method = "number",  type = "lower" )

ph13_0 %<>% dplyr::select(-Δ_oversizepit, -Δ_multiwalls, -Δ_encwall,    -Δ_reservoir)

# Fit the full model 
full.model.ph13_0 <- lm(log10_boutLength ~., data = ph13_0 ) 
# Stepwise regression model
step.model.ph13_0 <- stepAIC(full.model.ph13_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph13_0)

car::vif(step.model.ph13_0)

# remove high vif
full.model.ph13_0a <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-Δ_std_localCatchmentQuality))
# Stepwise regression model
step.model.ph13_0a <- stepAIC(full.model.ph13_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0a)#0.4514

car::vif(step.model.ph13_0a)

#remove other vif

full.model.ph13_0b <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph13_0b <- stepAIC(full.model.ph13_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0b)#0.4417

car::vif(step.model.ph13_0b)
#add the one with higher adj r^2
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph13_0a, timeperiod = 13, center = 0)



#ph13 centers
# corrplot(cor(ph13_1, use="pairwise.complete.obs"),  method = "number",  type = "lower" )

ph13_1 %<>% dplyr::select(-Δ_oversizepit, -Δ_multiwalls, -Δ_encwall ,-Δ_towers)

# Fit the full model 
full.model.ph13_1 <- lm(log10_boutLength ~., data = ph13_1) 
# Stepwise regression model
step.model.ph13_1  <- stepAIC(full.model.ph13_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_1)

car::vif(step.model.ph13_1)
# 4 high vifs: local catchment quality, catchment relative to region, regional pop

# remove highest

full.model.ph13_1a <- lm(log10_boutLength ~., data = ph13_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph13_1a  <- stepAIC(full.model.ph13_1a  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_1a) #just nhouse, r^2 = 0.3315

#next
full.model.ph13_1b <- lm(log10_boutLength ~., data = ph13_1 %>% dplyr::select(-Δ_std_localCatchmentQuality)) 
# Stepwise regression model
step.model.ph13_1b  <- stepAIC(full.model.ph13_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph13_1b) #just nhouse, r^2 = 0.3315

#next
full.model.ph13_1c <- lm(log10_boutLength ~., data = ph13_1 %>% dplyr::select(-Δ_std_regionalPop )) 
# Stepwise regression model
step.model.ph13_1c  <- stepAIC(full.model.ph13_1c  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph13_1c) #just nhouse, r^2 = 0.3315


#all of them are the same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph13_1a, timeperiod = 13, center = 1)



# ph 14 ----

ph14_0 <- d %>% dplyr::filter(vep == 14, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph14_1 <- d %>% dplyr::filter(vep == 14, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph14 non-centers
# corrplot(cor(ph14_0, use="pairwise.complete.obs"), method = "number", type = "lower" )

ph14_0 %<>% dplyr::select(-Δ_multiwalls)

# Fit the full model 
full.model.ph14_0 <- lm(log10_boutLength ~., data = ph14_0 ) 
# Stepwise regression model
step.model.ph14_0 <- stepAIC(full.model.ph14_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph14_0)

car::vif(step.model.ph14_0)

# remove high vif

full.model.ph14_0a <- lm(log10_boutLength ~., data = ph14_0 %>% dplyr::select(-Δ_std_localCatchmentQuality))
# Stepwise regression model
step.model.ph14_0a <- stepAIC(full.model.ph14_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_0a)#0.4402

car::vif(step.model.ph14_0a)#none

#remove other vif

full.model.ph14_0b <- lm(log10_boutLength ~., data = ph14_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph14_0b <- stepAIC(full.model.ph14_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_0b)#0.3376

car::vif(step.model.ph14_0b)

#a is much better (0.07 higher adj R^2), and same inst
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph14_0a, timeperiod = 14, center = 0)



#ph14 centers
# corrplot(cor(ph14_1, use="pairwise.complete.obs"), method = "number",   type = "lower" )

ph14_1 %<>% dplyr::select(-Δ_greatkiva, -Δ_plaza, -Δ_multiwalls, -Δ_towers)

# Fit the full model 
full.model.ph14_1 <- lm(log10_boutLength ~., data = ph14_1)
# Stepwise regression model
step.model.ph14_1  <- stepAIC(full.model.ph14_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1)

car::vif(step.model.ph14_1) #catchments a problem

#remove highest VIF
full.model.ph14_1a <- lm(log10_boutLength ~., data = ph14_1 %>% dplyr::select(-Δ_std_localCatchmentQuality))
# Stepwise regression model
step.model.ph14_1a <- stepAIC(full.model.ph14_1a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1a)#0.3183

car::vif(step.model.ph14_1a)#none

#remove other vif

full.model.ph14_1b <- lm(log10_boutLength ~., data = ph14_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph14_1b <- stepAIC(full.model.ph14_1b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1b)#same

car::vif(step.model.ph14_1b)

#they're the same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph14_1a, timeperiod = 14, center = 1)

# ph 15 ----

ph15_0 <- d %>% dplyr::filter(vep == 15, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph15_1 <- d %>% dplyr::filter(vep == 15, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph15 non-centers
# corrplot(cor(ph15_0, use="pairwise.complete.obs"),         method = "number",  type = "lower" )

ph15_0 %<>% dplyr::select(-Δ_greathouse) 

# Fit the full model 
full.model.ph15_0 <- lm(log10_boutLength ~., data = ph15_0 ) 
# Stepwise regression model
step.model.ph15_0 <- stepAIC(full.model.ph15_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph15_0)

car::vif(step.model.ph15_0) #weirdly, it's relative catchment and regionalpopulation

# remove high vif

full.model.ph15_0a <- lm(log10_boutLength ~., data = ph15_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph15_0a <- stepAIC(full.model.ph15_0a, direction = "both", 
                              trace = FALSE, k = log(nrow(ph15_0)))
summary(step.model.ph15_0a) # adj R^2 = 0.4327, no inst

car::vif(step.model.ph15_0a) #no high

#remove other vif

full.model.ph15_0b <- lm(log10_boutLength ~., data = ph15_0 %>% dplyr::select(-Δ_std_regionalPop))
# Stepwise regression model
step.model.ph15_0b <- stepAIC(full.model.ph15_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph15_0b) # adj R^2 = 0.4206

car::vif(step.model.ph15_0b) #none

#a is larger R^2 and simpler
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph15_0a, timeperiod = 15, center = 0)



#ph15 centers
# corrplot(cor(ph15_1, use="pairwise.complete.obs"), method = "number",   type = "lower" )

ph15_1 %<>% dplyr::select(-Δ_greathouse, -Δ_multiwalls, -Δ_towers)

# Fit the full model 
full.model.ph15_1 <- lm(log10_boutLength ~., data = ph15_1)
# Stepwise regression model
step.model.ph15_1  <- stepAIC(full.model.ph15_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph15_1)

car::vif(step.model.ph15_1) # ok we've got Δ_std_catchmentRelToRegion, Δ_std_regionalPop, and Δ_std_localCatchmentQuality


# remove highest

full.model.ph15_1a <- lm(log10_boutLength ~., data = ph15_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph15_1a  <- stepAIC(full.model.ph15_1a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph15_1a) #R^2 0.3722, reservoir

car::vif(step.model.ph15_1a) #none

#next
full.model.ph15_1b <- lm(log10_boutLength ~., data = ph15_1 %>% dplyr::select(-Δ_std_regionalPop )) 
# Stepwise regression model
step.model.ph15_1b  <- stepAIC(full.model.ph15_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph15_1b) #R^2 0.2982,  encwall

car::vif(step.model.ph15_1b)

#next
full.model.ph15_1c <- lm(log10_boutLength ~., data = ph15_1 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph15_1c  <- stepAIC(full.model.ph15_1c  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph15_1c) # same as A

car::vif(step.model.ph15_1c)

# a and c are the same, with r^2 0.3722

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph15_1a, timeperiod = 15, center = 1)



# ph 16 ----
ph16_0 <- d %>% dplyr::filter(vep == 16, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph16_1 <- d %>% dplyr::filter(vep == 16, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph16 non-centers
# corrplot(cor(ph16_0, use="pairwise.complete.obs"), method = "number", type = "lower" )

# Fit the full model 
full.model.ph16_0 <- lm(log10_boutLength ~., data = ph16_0 ) 
# Stepwise regression model
step.model.ph16_0 <- stepAIC(full.model.ph16_0, direction = "both", 
                             trace = FALSE, k = 3.8416)
summary(step.model.ph16_0)

car::vif(step.model.ph16_0) #catchments and field contention

# remove highest

full.model.ph16_0a <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph16_0a  <- stepAIC(full.model.ph16_0a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0a) #R^2 0.8523, reservoir and multiwalls

car::vif(step.model.ph16_0a) #none

#next
full.model.ph16_0b <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph16_0b  <- stepAIC(full.model.ph16_0b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0b) #R^2 0.8508 same

car::vif(step.model.ph16_0b)

#next
full.model.ph16_0c <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-Δ_std_fieldContention )) 
# Stepwise regression model
step.model.ph16_0c  <- stepAIC(full.model.ph16_0c  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0c) # 0.6999

car::vif(step.model.ph16_0c) #catchments a problem



stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph16_0a, timeperiod = 16, center = 0)



#ph16 centers
# corrplot(cor(ph16_1, use="pairwise.complete.obs"),  method = "number", type = "lower" )


# Fit the full model 
full.model.ph16_1 <- lm(log10_boutLength ~., data = ph16_1)
# Stepwise regression model
step.model.ph16_1  <- stepAIC(full.model.ph16_1  , direction = "both", 
                              trace = FALSE, k = 3.8416)
summary(step.model.ph16_1)

car::vif(step.model.ph16_1) # ok we've got Δ_std_catchmentRelToRegion and Δ_std_localCatchmentQuality


# remove highest

full.model.ph16_1a <- lm(log10_boutLength ~., data = ph16_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph16_1a  <- stepAIC(full.model.ph16_1a  , direction = "both", 
                               trace = FALSE, k = 3.8416)
summary(step.model.ph16_1a) #greatkiva, R^2 0.7512

car::vif(step.model.ph16_1a) #none

#next
full.model.ph16_1b <- lm(log10_boutLength ~., data = ph16_1 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph16_1b  <- stepAIC(full.model.ph16_1b  , direction = "both", 
                               trace = FALSE, k = 3.8416)
summary(step.model.ph16_1b)

car::vif(step.model.ph16_1b)#they're the same

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph16_1b, timeperiod = 16, center = 1)



# ph 17 ----

ph17_0 <- d %>% dplyr::filter(vep == 17, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph17_1 <- d %>% dplyr::filter(vep == 17, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph17 non-centers
# corrplot(cor(ph17_0, use="pairwise.complete.obs"),  method = "number",  type = "lower" )

# Fit the full model 
full.model.ph17_0 <- lm(log10_boutLength ~., data = ph17_0 ) 
# Stepwise regression model
step.model.ph17_0 <- stepAIC(full.model.ph17_0, direction = "both", 
                             trace = FALSE, k = 3.8417)
summary(step.model.ph17_0) #0.8149

car::vif(step.model.ph17_0) #  Δ_std_catchmentRelToRegion and Δ_std_localCatchmentQuality


# remove highest
full.model.ph17_0a <- lm(log10_boutLength ~., data = ph17_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph17_0a  <- stepAIC(full.model.ph17_0a  , direction = "both", 
                               trace = FALSE, k = 3.8417)
summary(step.model.ph17_0a) # R^2 0.7945 greatkiva, greathouse, multiwalls, reservoir

car::vif(step.model.ph17_0a)

#next
full.model.ph17_0b <- lm(log10_boutLength ~., data = ph17_0 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph17_0b  <- stepAIC(full.model.ph17_0b  , direction = "both", 
                               trace = FALSE, k = 3.8417)
summary(step.model.ph17_0b) #  0.7876 just greatkiva and greathouse.

car::vif(step.model.ph17_0b)

#pretty much the same but b is simpler

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph17_0b, timeperiod = 17, center = 0)



#ph17 centers
# corrplot(cor(ph17_1, use="pairwise.complete.obs"), method = "number",  type = "lower" )

ph17_1 %<>% dplyr::select(-Δ_encwall, -Δ_towers)

# Fit the full model 
full.model.ph17_1 <- lm(log10_boutLength ~., data = ph17_1)
# Stepwise regression model
step.model.ph17_1  <- stepAIC(full.model.ph17_1  , direction = "both", 
                              trace = FALSE, k = 3.8417)
summary(step.model.ph17_1) #R^2 0.8465,

car::vif(step.model.ph17_1) # ok we've got  Δ_std_localCatchmentQuality and Δ_std_catchmentRelToRegion


# remove highest

full.model.ph17_1a <- lm(log10_boutLength ~., data = ph17_1 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph17_1a  <- stepAIC(full.model.ph17_1a  , direction = "both", 
                               trace = FALSE, k = 3.8417)
summary(step.model.ph17_1a)#adj R^2 0.7801, greathouse, Δ_multiwalls

car::vif(step.model.ph17_1a) #none

#next
full.model.ph17_1b <- lm(log10_boutLength ~., data = ph17_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph17_1b  <- stepAIC(full.model.ph17_1b  , direction = "both", 
                               trace = FALSE, k = 3.8417)
summary(step.model.ph17_1b) #same

car::vif(step.model.ph17_1b)

#same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph17_1b, timeperiod = 17, center = 1)


# ph 18 ----
ph18_0 <- d %>% dplyr::filter(vep == 18, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph18_1 <- d %>% dplyr::filter(vep == 18, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph18 non-centers
# corrplot(cor(ph18_0, use="pairwise.complete.obs"),  method = "number",  type = "lower" )

ph18_0 %<>% dplyr::select(-Δ_towers) 

# Fit the full model 
full.model.ph18_0 <- lm(log10_boutLength ~., data = ph18_0 ) 
# Stepwise regression model
step.model.ph18_0 <- stepAIC(full.model.ph18_0, direction = "both", 
                             trace = FALSE, k = 3.8418)
summary(step.model.ph18_0)

car::vif(step.model.ph18_0) #  Δ_std_catchmentRelToRegion and Δ_std_localCatchmentQuality


# remove highest
full.model.ph18_0a <- lm(log10_boutLength ~., data = ph18_0 %>% dplyr::select(-Δ_std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph18_0a  <- stepAIC(full.model.ph18_0a  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_0a)#0.7874, with greatkiva and multiwalls

car::vif(step.model.ph18_0a)#none

#next
full.model.ph18_0b <- lm(log10_boutLength ~., data = ph18_0 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph18_0b  <- stepAIC(full.model.ph18_0b  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_0b) #0.7896 with Δ_greathouse and multiwalls

car::vif(step.model.ph18_0b) 

#pretty much the same but a is simpler

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph18_0a, timeperiod = 18, center = 0)



#ph18 centers
# corrplot(cor(ph18_1, use="pairwise.complete.obs"),  method = "number",  type = "lower" ) # there are so many correlations omg

ph18_1 %<>% dplyr::select( -Δ_towers)

# Fit the full model 
full.model.ph18_1 <- lm(log10_boutLength ~., data = ph18_1)
# Stepwise regression model
step.model.ph18_1  <- stepAIC(full.model.ph18_1  , direction = "both", 
                              trace = FALSE, k = 3.8418)
summary(step.model.ph18_1)

car::vif(step.model.ph18_1) # ok we've got  Δ_std_localCatchmentQuality  Δ_std_catchmentRelToRegion and Δ_greathouse and Δ_oversizepit


# remove highest

full.model.ph18_1a <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph18_1a  <- stepAIC(full.model.ph18_1a  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1a) #0.6829

car::vif(step.model.ph18_1a) # greathouse and oversizepit high

#next
full.model.ph18_1b <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph18_1b  <- stepAIC(full.model.ph18_1b  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1b) #0.6665

car::vif(step.model.ph18_1b)#no issues

#b has no issues
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph18_1b, timeperiod = 18, center = 1)

# ph 19 ----

ph19_0 <- d %>% dplyr::filter(vep == 19, center == 0) %>% dplyr::select(-cell, -center, -vep)
ph19_1 <- d %>% dplyr::filter(vep == 19, center == 1)%>% dplyr::select(-cell, -center, -vep)

#ph19 non-centers
# corrplot(cor(ph19_0, use="pairwise.complete.obs"),  method = "number",       type = "lower" )

ph19_0 %<>% dplyr::select(-Δ_plaza, -Δ_towers, -Δ_reservoir) 

# Fit the full model 
full.model.ph19_0 <- lm(log10_boutLength ~., data = ph19_0 ) 
# Stepwise regression model
step.model.ph19_0 <- stepAIC(full.model.ph19_0, direction = "both", 
                             trace = FALSE, k = 3.8419)
summary(step.model.ph19_0)

car::vif(step.model.ph19_0) #  all good

stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph19_0, timeperiod = 19, center = 0)



#ph19 centers
# corrplot(cor(ph19_1, use="pairwise.complete.obs"), method = "number",  type = "lower" ) # there are so many correlations omg

ph19_1 %<>% dplyr::select( -Δ_plaza, -Δ_oversizepit, -Δ_towers)

# Fit the full model 
full.model.ph19_1 <- lm(log10_boutLength ~., data = ph19_1)
# Stepwise regression model
step.model.ph19_1  <- stepAIC(full.model.ph19_1  , direction = "both", 
                              trace = FALSE, k = 3.8419)
summary(step.model.ph19_1)

car::vif(step.model.ph19_1) # ok we've got Δ_std_catchmentRelToRegion,  Δ_std_localCatchmentQuality


# remove highest

full.model.ph19_1a <- lm(log10_boutLength ~., data = ph19_1 %>% dplyr::select(-Δ_std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph19_1a  <- stepAIC(full.model.ph19_1a  , direction = "both", 
                               trace = FALSE, k = 3.8419)
summary(step.model.ph19_1a)# 0.8421 greatkiva and greathouse

car::vif(step.model.ph19_1a) #none

#next
full.model.ph19_1b <- lm(log10_boutLength ~., data = ph19_1 %>% dplyr::select(-Δ_std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph19_1b  <- stepAIC(full.model.ph19_1b  , direction = "both", 
                               trace = FALSE, k = 3.8419)
summary(step.model.ph19_1b) #same

car::vif(step.model.ph19_1b)


# they're the same
stepwise_relative %<>% modifiedAddRow(addedRow = step.model.ph19_1a, timeperiod = 19, center = 1)



#
#make wide and export ----
stepwise_relative %>% 
  saveRDS(here::here("data/data-derived/LMresults/stepwise_relative.rds"))



stepwise_relative %>% 
  # dplyr::filter(timeperiod == 7) %>% 
  tidyr::unnest(cols = results) %>% 
  dplyr::rename(Pvalue = `Pr(>|t|)`) %>% 
  dplyr::select(timeperiod, center, model, `adj R2`, variable, Estimate, Pvalue) %>% 
  dplyr::mutate(signif = case_when(Pvalue > 0.1 ~ "N.S.",
                                   Pvalue > 0.05 & Pvalue <= 0.1 ~ ".",
                                   Pvalue > 0.01 & Pvalue <= 0.05 ~"*",
                                   Pvalue > 0.001 & Pvalue <= 0.01 ~"**",
                                   Pvalue >= 0 & Pvalue <= 0.001 ~"***"
                                   
  )) %>% 
  dplyr::mutate(Estimate = stringr::str_c(round(Estimate, 2), " ", signif)) %>% 
  dplyr::select(-model, -Pvalue, -signif) %>% 
  tidyr::pivot_wider(names_from = variable, values_from = Estimate) %>% 
  dplyr::rename(intercept = `(Intercept)` ) %>% 
  dplyr::left_join(d %>% dplyr::group_by(vep, center) %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::ungroup(), 
                   by = c("timeperiod" = "vep", "center" = "center")) %>% 
  dplyr::relocate(timeperiod, center, n , `adj R2`, 
                  intercept,  Δ_std_nHouse,Δ_std_fieldContention,	Δ_std_regionalPop	,
                  Δ_std_localCatchmentQuality,	Δ_std_catchmentRelToRegion,
                  Δ_greatkiva,	Δ_plaza,	Δ_oversizepit,	Δ_greathouse,	Δ_multiwalls,	Δ_towers,	Δ_reservoir) %>% 
  dplyr::arrange(center) %>% 
  dplyr::left_join( robinson2020::vep_demography %>% dplyr::filter(`Study Area` == "CMV") %>% dplyr::select(Period, Start, End),
                    by = c("timeperiod" = "Period")) %>% 
  dplyr::mutate(center = factor(center, levels = c("0", "1"), labels = c("non-center", "center")),
                timeperiod = paste0(timeperiod, " (", Start, "-", End, ")")) %>% 
  dplyr::rename(`period (years CE)` = timeperiod) %>% 
  dplyr::select(-Start, -End) %>% 
  readr::write_csv(here::here("data/data-derived/LMresults/stepwise_relative.csv"),
                   na = "")

# make fig 4 ----

stepwise_relative <- readRDS(here::here("data/data-derived/LMresults/stepwise_relative.rds"))

relative_long <- stepwise_relative %>% 
  # dplyr::filter(timeperiod == 7) %>% 
  tidyr::unnest(cols = results) %>% 
  dplyr::rename(Pvalue = `Pr(>|t|)`) %>% 
  dplyr::select(timeperiod, center, model, `adj R2`, variable, Estimate, Pvalue) %>% 
  dplyr::select(-model, -Pvalue, -`adj R2`) %>% 
  dplyr::mutate(center = factor(center, levels = c("0", "1"), labels = c("non-centers", "centers"))) %>%
  dplyr::left_join(robinson2020::vep_demography %>% 
                     dplyr::filter(`Study Area` == "CMV") %>% 
                     dplyr::select(Period, Start, End) %>% 
                     dplyr::rowwise() %>%
                     dplyr::mutate(year = list(Start:(End - 1))) %>%
                     tidyr::unnest(year) %>%
                     dplyr::rename(timeperiod = Period) %>% 
                     dplyr::select(timeperiod, year),
                   by = c("timeperiod" = "timeperiod")) %>% 
  dplyr::filter(variable != "(Intercept)")

A <- relative_long %>% 
  dplyr::filter(variable %in% 
                  c( "Δ_oversizepit",
                    "Δ_std_fieldContention",
                    "Δ_std_regionalPop")) %>% 
  dplyr::mutate(variable =
                  factor(variable, 
                         levels = c(
                           "Δ_oversizepit",
                           "Δ_std_fieldContention",
                           "Δ_std_regionalPop"),
                         labels = c(
                           "oversized pithouse",
                           "field contention",
                           "regional population"
                         ))) %>%
  ggplot(aes(x = year, y = Estimate, color = factor(variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
  scale_color_manual(values = c("#00ff00", "#56B4E9",  "#E69F00"))+
  scale_x_continuous(breaks = seq(600, 1300, 100))+
  scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1,1.1))+
  facet_wrap(vars(center), nrow =2)+
  theme_bw()+
  theme(#legend.position = "top",
        legend.title = element_blank(),
         axis.title.x =element_blank(),
        axis.text.x = element_blank()
        )+ 
  guides(colour = guide_legend(override.aes = list(size=4)))


B <- relative_long %>% 
  dplyr::filter(variable %in% 
                  c("Δ_std_nHouse", 
                    "Δ_std_localCatchmentQuality",
                    "Δ_std_catchmentRelToRegion" )) %>% 
  dplyr::mutate(variable = factor(variable,
                                  levels =  c("Δ_std_nHouse", 
                                              "Δ_std_localCatchmentQuality",
                                              "Δ_std_catchmentRelToRegion" ),
                                  labels = c("mean occupied household count",
                                             "local cachment quality",
                                             "relative catchment quality"))) %>% 
  ggplot(aes(x = year, y = Estimate, color = factor(variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
  scale_color_manual(values = c("#009E73", "#0072B2",  "#CC79A7"))+
  scale_x_continuous(breaks = seq(600, 1300, 100))+
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-0.5,0.5))+
  facet_wrap(vars(center), nrow =2)+
  theme_bw()+
  theme(#legend.position = "top",
        legend.title = element_blank(),
        axis.title.x =element_blank(),
        axis.text.x = element_blank()
        )+ 
  xlab("Year AD")+
  guides(colour = guide_legend(override.aes = list(size=4)))


C <- relative_long %>% 
  dplyr::filter(variable %in% 
                  c( "Δ_greatkiva", 
                     "Δ_plaza", 
                     "Δ_greathouse", 
                     "Δ_multiwalls", 
                     "Δ_encwall", 
                     "Δ_towers", 
                     "Δ_reservoir")) %>% 
  dplyr::mutate(variable = factor(variable, 
                                  levels = c( "Δ_greatkiva", 
                                              "Δ_plaza",
                                              "Δ_greathouse", 
                                              "Δ_multiwalls", 
                                              "Δ_encwall", 
                                              "Δ_towers", 
                                              "Δ_reservoir"),
                                  labels = c("great kivas", 
                                             "plazas",
                                             "great houses", 
                                             "multi-walled structure", 
                                             "enclosing walls", 
                                             "towers", 
                                             "reservoirs"))) %>% 
  ggplot(aes(x = year, y = Estimate, color = factor(variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
  scale_color_manual(values = c("#ff0000",  "#ffd700", "#ffb6c1", "#006400", "#00ffff", "#ff00ff", "#191970"))+
  scale_x_continuous(limits = c(600, 1300), breaks = seq(600, 1300, 100))+
  scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-0.7,0.5))+
  facet_wrap(vars(center), nrow =2) +
  theme_bw()+
  theme(#legend.position = "top",
    legend.title = element_blank())+ 
  guides(colour = guide_legend(override.aes = list(size=4)))


library(patchwork)
A / B / C

png(filename = "fig4_relative.png", units = "in", width = 10, height = 12, res = 300)
A / B / C + plot_annotation(tag_levels = 'A')
dev.off()




#combine absolute and relative ----
stepwise_absolute <-   readRDS(here::here("data/data-derived/LMresults/stepwise_absolute.rds"))
stepwise_relative <-   readRDS(here::here("data/data-derived/LMresults/stepwise_relative.rds"))

stepwise_absolute %>% 
  dplyr::select(timeperiod, center, `adj R2`) %>% 
  dplyr::rename(absR2 = `adj R2`) %>% 
  dplyr::left_join(stepwise_relative%>% dplyr::select(timeperiod, center, `adj R2`) %>% 
                     dplyr::rename(relR2 = `adj R2`),
                   by = c("timeperiod" = "timeperiod", "center" = "center")) %>% 
  tidyr::pivot_longer(cols = c(relR2, absR2)) %>% 
  dplyr::mutate(center = factor(center, levels = c("0", "1"), labels = c("non-center", "center")),
                name = factor(name, levels = c("absR2", "relR2"), labels = c("absolute model R2", "relative model R2"))) %>% 
  ggplot(aes(x = timeperiod, y = value, color = name))+
  geom_point(size= 5)+
  geom_line()+
  ylab("R2 value")+
  scale_y_continuous(breaks = seq(0.3,0.9, 0.1))+
  scale_x_continuous(breaks = seq(6, 19, 1))+
  facet_wrap(vars(center), nrow = 2)+
  theme_bw()+
  theme(legend.position = "top",
        legend.title = element_blank())



stepwise_absolute %>% 
  dplyr::select(timeperiod, center, `adj R2`) %>% 
  dplyr::rename(absR2 = `adj R2`) %>% 
  dplyr::left_join(stepwise_relative%>% dplyr::select(timeperiod, center, `adj R2`) %>% 
                     dplyr::rename(relR2 = `adj R2`),
                   by = c("timeperiod" = "timeperiod", "center" = "center")) %>% 
  dplyr::mutate(whichBigger = ifelse(absR2 > relR2, "abs", "rel"),
                theBigger = ifelse(absR2 > relR2, absR2, relR2)) %>%
  ggplot(aes(x=timeperiod, y = theBigger, color = whichBigger))+
  geom_point(size= 5)+
  facet_wrap(vars(center), nrow = 2)
  