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


d <- read_csv(here::here("data/data-derived/prepAnalysis/bout_absVals.csv")) 



# ph 6  ----
ph6_0 <- d %>% dplyr::filter(vep == 6, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph6_1 <- d %>% dplyr::filter(vep == 6, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)


#ph6 non-centers
corrplot(cor(ph6_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph6_0 %<>% dplyr::select(-greatkiva, -plaza, -greathouse, -multiwalls, -encwall, -reservoir) 

# Fit the full model 
full.model.ph6_0 <- lm(log10_boutLength ~., data = ph6_0)
# Stepwise regression model
step.model.ph6_0 <- stepAIC(full.model.ph6_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph6_0)

car::vif(step.model.ph6_0) #the two catchments

#remove highest
full.model.ph6_0a <- lm(log10_boutLength ~., data = ph6_0 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph6_0a <- stepAIC(full.model.ph6_0a, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph6_0a)

car::vif(step.model.ph6_0a) # all good

#other
full.model.ph6_0b <- lm(log10_boutLength ~., data = ph6_0 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph6_0b <- stepAIC(full.model.ph6_0b, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph6_0b) #they're the same

car::vif(step.model.ph6_0b)

stepwise_absolute <- tibble::tibble(
  timeperiod = 6,
  center = 0,
  model = as.character(summary(step.model.ph6_0a)$call[2]),
  `adj R2` = round(summary(step.model.ph6_0a)$adj.r.squared, digits = 2),
  summary(step.model.ph6_0a)$coefficients %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "variable") %>% 
    dplyr::mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 7),
                  Estimate = round(Estimate, digits = 3)) %>% 
    nest_by(.key = "results")
)


#ph6 centers
corrplot(cor(ph6_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph6_1 %<>% dplyr::select(-greatkiva, -plaza, -greathouse, -multiwalls, -encwall, -reservoir) 


# Fit the full model 
full.model.ph6_1 <- lm(log10_boutLength ~., data = ph6_1 %>% dplyr::mutate(std_fieldContention))
# Stepwise regression model
step.model.ph6_1  <- stepAIC(full.model.ph6_1 , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph6_1)

car::vif(step.model.ph6_1) #all good

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph6_1, timeperiod = 6, center = 1)

# ph 7 ----

ph7_0 <- d %>% dplyr::filter(vep == 7, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph7_1 <- d %>% dplyr::filter(vep == 7, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph7 non-centers
corrplot(cor(ph7_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph7_0 %<>% dplyr::select(-greathouse, -multiwalls, -encwall, -reservoir) 

# Fit the full model 
full.model.ph7_0 <- lm(log10_boutLength ~., data = ph7_0)
# Stepwise regression model
step.model.ph7_0 <- stepAIC(full.model.ph7_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph7_0)

car::vif(step.model.ph7_0) #all good

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph7_0, timeperiod = 7, center = 0)



#ph7 centers
corrplot(cor(ph7_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph7_1 %<>% dplyr::select( -greathouse, -multiwalls, -encwall, -towers, -reservoir) 


# Fit the full model 
full.model.ph7_1 <- lm(log10_boutLength ~., data = ph7_1 %>% dplyr::select(-log10_boutNo))
# Stepwise regression model
step.model.ph7_1  <- stepAIC(full.model.ph7_1 , direction = "both", 
                             trace = FALSE, k = log(nrow(ph7_1)) )
summary(step.model.ph7_1)

car::vif(step.model.ph7_1) # all good

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph7_1, timeperiod = 7, center = 1)


# ph 8 ----

ph8_0 <- d %>% dplyr::filter(vep == 8, center == 0) %>% dplyr::select(-dummyCell, -center,-vep)
ph8_1 <- d %>% dplyr::filter(vep == 8, center == 1)%>% dplyr::select(-dummyCell, -center,-vep)

#ph8 non-centers
corrplot(cor(ph8_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph8_0 %<>%  dplyr::select(-greathouse, -multiwalls, -encwall, -reservoir)

# Fit the full model 
full.model.ph8_0 <- lm(log10_boutLength ~., data = ph8_0)
step.model.ph8_0 <- stepAIC(full.model.ph8_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph8_0)
car::vif(step.model.ph8_0) #catchments are problems 

#remove highest
full.model.ph8_0a <- lm(log10_boutLength ~., data = ph8_0 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph8_0a <- stepAIC(full.model.ph8_0a, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_0a)
car::vif(step.model.ph8_0a) 

#next
full.model.ph8_0b <- lm(log10_boutLength ~., data = ph8_0 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph8_0b <- stepAIC(full.model.ph8_0b, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_0b)
car::vif(step.model.ph8_0b)  

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph8_0a, timeperiod = 8, center = 0)


#ph8 centers
corrplot(cor(ph8_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph8_1 %<>% dplyr::select(-greathouse, -multiwalls, -encwall, -reservoir) 

# Fit the full model 
full.model.ph8_1 <- lm(log10_boutLength ~., data = ph8_1)
step.model.ph8_1  <- stepAIC(full.model.ph8_1 , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_1)
car::vif(step.model.ph8_1) #again with the catchments

#remove highest
full.model.ph8_1a <- lm(log10_boutLength ~., data = ph8_1 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph8_1a <- stepAIC(full.model.ph8_1a, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_1a)
car::vif(step.model.ph8_1a) 

#next
full.model.ph8_1b <- lm(log10_boutLength ~., data = ph8_1 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph8_1b <- stepAIC(full.model.ph8_1b, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph8_1b)
car::vif(step.model.ph8_1b)  

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph8_1a, timeperiod = 8, center = 1)

# ph 9 ----

ph9_0 <- d %>% dplyr::filter(vep == 9, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph9_1 <- d %>% dplyr::filter(vep == 9, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph9 non-centers
corrplot(cor(ph9_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph9_0 %<>% dplyr::select(-greathouse, -multiwalls, -encwall, -reservoir) 



# Fit the full model 
full.model.ph9_0 <- lm(log10_boutLength ~., data = ph9_0)
# Stepwise regression model
step.model.ph9_0 <- stepAIC(full.model.ph9_0, direction = "both", 
                            trace = FALSE, k = 3.8415)
summary(step.model.ph9_0)
car::vif(step.model.ph9_0) #catchments

#remove highest
full.model.ph9_0a <- lm(log10_boutLength ~., data = ph9_0 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph9_0a <- stepAIC(full.model.ph9_0a, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_0a)
car::vif(step.model.ph9_0a) 

#next
full.model.ph9_0b <- lm(log10_boutLength ~., data = ph9_0 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph9_0b <- stepAIC(full.model.ph9_0b, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_0b)
car::vif(step.model.ph9_0b)  

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph9_0a, timeperiod = 9, center = 0)

#ph9 centers
corrplot(cor(ph9_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph9_1 %<>% dplyr::select( -greathouse, -multiwalls, -encwall, -towers, -reservoir) 

# Fit the full model 
full.model.ph9_1 <- lm(log10_boutLength ~., data = ph9_1)
step.model.ph9_1  <- stepAIC(full.model.ph9_1 , direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_1)
car::vif(step.model.ph9_1) #catchments

#remove highest
full.model.ph9_1a <- lm(log10_boutLength ~., data = ph9_1 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph9_1a <- stepAIC(full.model.ph9_1a, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_1a)
car::vif(step.model.ph9_1a) 

#next
full.model.ph9_1b <- lm(log10_boutLength ~., data = ph9_1 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph9_1b <- stepAIC(full.model.ph9_1b, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph9_1b)
car::vif(step.model.ph9_1b)  

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph9_1a, timeperiod = 9, center = 1)

# ph 10 ----

ph10_0 <- d %>% dplyr::filter(vep == 10, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph10_1 <- d %>% dplyr::filter(vep == 10, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph10 non-centers
corrplot(cor(ph10_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph10_0 %<>% dplyr::select(-greathouse, -multiwalls, -encwall)


# Fit the full model 
full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0)
# Stepwise regression model
step.model.ph10_0 <- stepAIC(full.model.ph10_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph10_0)
car::vif(step.model.ph10_0) #catchments

# remove
full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0 %>% dplyr::select(-std_catchmentRelToRegion))

step.model.ph10_0a <- stepAIC(full.model.ph10_0, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_0a)
car::vif(step.model.ph10_0a)

# compare
full.model.ph10_0 <- lm(log10_boutLength ~., data = ph10_0 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph10_0b <- stepAIC(full.model.ph10_0, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_0b)
car::vif(step.model.ph10_0b)

# they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph10_0b, timeperiod = 10, center = 0)


#ph10 centers
corrplot(cor(ph10_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph10_1 %<>% dplyr::select(-greathouse, -multiwalls, -encwall, -reservoir)

# Fit the full model 
full.model.ph10_1 <- lm(log10_boutLength ~., data = ph10_1) 
# Stepwise regression model
step.model.ph10_1  <- stepAIC(full.model.ph10_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_1)
car::vif(step.model.ph10_1) #catchments per usual

# remove
full.model.ph10_1a <- lm(log10_boutLength ~., data = ph10_1 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph10_1a <- stepAIC(full.model.ph10_1a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_1a)
car::vif(step.model.ph10_1a)

#check the other
full.model.ph10_1b <- lm(log10_boutLength ~., data = ph10_1 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph10_1b <- stepAIC(full.model.ph10_1b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph10_1b)
car::vif(step.model.ph10_1b)

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph10_1a, timeperiod = 10, center = 1)

# ph 11 ----

ph11_0 <- d %>% dplyr::filter(vep == 11, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph11_1 <- d %>% dplyr::filter(vep == 11, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph11 non-centers
corrplot(cor(ph11_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph11_0 %<>% dplyr::select(-greathouse, -multiwalls, -encwall)

# Fit the full model 
full.model.ph11_0 <- lm(log10_boutLength ~., data = ph11_0)
# Stepwise regression model
step.model.ph11_0 <- stepAIC(full.model.ph11_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph11_0) 
car::vif(step.model.ph11_0) #everything but nhouse is problem

# remove
full.model.ph11_0a <- lm(log10_boutLength ~., data = ph11_0 %>% dplyr::select(-std_localCatchmentQuality))
step.model.ph11_0a <- stepAIC(full.model.ph11_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_0a) 
car::vif(step.model.ph11_0a) #field contention and regional pop still probs, 0.6633

# and the next
full.model.ph11_0b <- lm(log10_boutLength ~., data = ph11_0 %>% dplyr::select(-std_catchmentRelToRegion))
step.model.ph11_0b <- stepAIC(full.model.ph11_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_0b)#field contention and regional pop still probs, 0.6633
car::vif(step.model.ph11_0b)

# and the next
full.model.ph11_0c <- lm(log10_boutLength ~., data = ph11_0 %>% dplyr::select(-std_fieldContention))
step.model.ph11_0c <- stepAIC(full.model.ph11_0c, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_0c) #0.5817
car::vif(step.model.ph11_0c) #but this is ok

#check the other
full.model.ph11_0d <- lm(log10_boutLength ~., data = ph11_0 %>% dplyr::select(-std_regionalPop))
step.model.ph11_0d <- stepAIC(full.model.ph11_0d, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_0d) # just nhouse and field contention,  0.6489
car::vif(step.model.ph11_0d) # good

# c hsa the highest r2 and no issues
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph11_0c, timeperiod = 11, center = 0)



#ph11 centers
corrplot(cor(ph11_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )


ph11_1 %<>% dplyr::select(-greathouse, -multiwalls, -encwall)

# Fit the full model 
full.model.ph11_1 <- lm(log10_boutLength ~., data = ph11_1 ) 
# Stepwise regression model
step.model.ph11_1  <- stepAIC(full.model.ph11_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph11_1)

car::vif(step.model.ph11_1) #catchments are a problem

#remove high vif
full.model.ph11_1a <- lm(log10_boutLength ~., data = ph11_1 %>% dplyr::select(-std_localCatchmentQuality    )) 
# Stepwise regression model
step.model.ph11_1a  <- stepAIC(full.model.ph11_1a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph11_1a) # lots fo things 0.653

car::vif(step.model.ph11_1a) # all good


full.model.ph11_1b <- lm(log10_boutLength ~., data = ph11_1 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph11_1b  <- stepAIC(full.model.ph11_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph11_1b)

car::vif(step.model.ph11_1b)

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph11_1a, timeperiod = 11, center = 1)


# ph 12 ----
ph12_0 <- d %>% dplyr::filter(vep == 12, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph12_1 <- d %>% dplyr::filter(vep == 12, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph12 non-centers
corrplot(cor(ph12_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph12_0 %<>% dplyr::select(-plaza, -greathouse, -multiwalls, -encwall, -reservoir)

# Fit the full model 
full.model.ph12_0 <- lm(log10_boutLength ~., data = ph12_0 ) 
# Stepwise regression model
step.model.ph12_0 <- stepAIC(full.model.ph12_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph12_0)

car::vif(step.model.ph12_0)#catchments and regional pop

# remove high vif
full.model.ph12_0a <- lm(log10_boutLength ~., data = ph12_0 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph12_0a <- stepAIC(full.model.ph12_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_0a)# field contention and great kiva, 0.742
car::vif(step.model.ph12_0a)

#remove other vif
full.model.ph12_0b <- lm(log10_boutLength ~., data = ph12_0 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph12_0b <- stepAIC(full.model.ph12_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_0b)# field contention and great kiva, 0.742

car::vif(step.model.ph12_0b)

#last one
full.model.ph12_0c <- lm(log10_boutLength ~., data = ph12_0 %>% dplyr::select(-std_regionalPop))
# Stepwise regression model
step.model.ph12_0c <- stepAIC(full.model.ph12_0c, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_0c)

car::vif(step.model.ph12_0c) #higher R^2 at 0.7476, but both cathcments are still problem

#a & b are the same and have no issues
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph12_0b, timeperiod = 12, center = 0)



#ph12 centers
corrplot(cor(ph12_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph12_1 %<>% dplyr::select(-plaza, -multiwalls, -encwall, -towers)

# Fit the full model 
full.model.ph12_1 <- lm(log10_boutLength ~., data = ph12_1)
# Stepwise regression model
step.model.ph12_1  <- stepAIC(full.model.ph12_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_1)

car::vif(step.model.ph12_1) #catchments and regionalpop issues

#remove high vif
full.model.ph12_1a <- lm(log10_boutLength ~., data = ph12_1 %>% dplyr::select(-std_catchmentRelToRegion ))
# Stepwise regression model
step.model.ph12_1a  <- stepAIC(full.model.ph12_1a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph12_1a) #field contention and regional pop, 0.76

car::vif(step.model.ph12_1a)

#remove other vif
full.model.ph12_1b <- lm(log10_boutLength ~., data = ph12_1 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph12_1b  <- stepAIC(full.model.ph12_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph12_1b) #field contention and regional pop, 0.76

car::vif(step.model.ph12_1b)

#last one
full.model.ph12_1c <- lm(log10_boutLength ~., data = ph12_1 %>% dplyr::select(-std_regionalPop))
# Stepwise regression model
step.model.ph12_1c <- stepAIC(full.model.ph12_1c, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph12_1c) #field contention and catchments. higher R^2, but both catchments still problem

car::vif(step.model.ph12_1c)  #catchments still problems
#a & b are better
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph12_1a, timeperiod = 12, center = 1)

# ph 13 ----

ph13_0 <- d %>% dplyr::filter(vep == 13, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph13_1 <- d %>% dplyr::filter(vep == 13, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph13 non-centers
corrplot(cor(ph13_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph13_0 %<>% dplyr::select( -reservoir)

# Fit the full model 
full.model.ph13_0 <- lm(log10_boutLength ~., data = ph13_0 ) 
# Stepwise regression model
step.model.ph13_0 <- stepAIC(full.model.ph13_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph13_0)

car::vif(step.model.ph13_0) #catchments, greatkiva, and oversizepit problems

# remove high vif
full.model.ph13_0a <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph13_0a <- stepAIC(full.model.ph13_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0a)

car::vif(step.model.ph13_0a) #oversizepit and greatkiva still probs and their numbers haven't changed

#remove other vif

full.model.ph13_0b <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph13_0b <- stepAIC(full.model.ph13_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0b)

car::vif(step.model.ph13_0b)#oversizepit and greatkiva still probs and their numbers haven't changed


#let's remvoe two
full.model.ph13_0c <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_localCatchmentQuality, -greatkiva))
# Stepwise regression model
step.model.ph13_0c <- stepAIC(full.model.ph13_0c, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0c) #maxhouse, fieldcontention, regionalpop, r2 = 0.2382

car::vif(step.model.ph13_0c) # no issues

#let's remvoe two
full.model.ph13_0d <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_localCatchmentQuality, -oversizepit))
# Stepwise regression model
step.model.ph13_0d <- stepAIC(full.model.ph13_0d, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0d) #maxhouse, field contention, regional pop, greatkiva, and NOW MULTIWALLS

car::vif(step.model.ph13_0d) #greatkiva and multiwalls are a problem

#sanity check
full.model.ph13_0e <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_localCatchmentQuality, -oversizepit, -multiwalls))
# Stepwise regression model
step.model.ph13_0e <- stepAIC(full.model.ph13_0e, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0e) #maxhouse, field contention, regional pop, greatkiva, r2 = 0.2436 

car::vif(step.model.ph13_0e) # no issues

#sanity check
full.model.ph13_0f <- lm(log10_boutLength ~., data = ph13_0 %>% dplyr::select(-std_localCatchmentQuality, -oversizepit, -greatkiva))
# Stepwise regression model
step.model.ph13_0f <- stepAIC(full.model.ph13_0f, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_0f) #maxhouse, fieldcontention, regionalpop, r2 = 0.2382

car::vif(step.model.ph13_0f) # no issues


#c and f are less problematic and simpler and within 0.01
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph13_0c, timeperiod = 13, center = 0)



#ph13 centers
corrplot(cor(ph13_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

# ph13_1 %<>% dplyr::select(-towers)

# Fit the full model 
full.model.ph13_1 <- lm(log10_boutLength ~., data = ph13_1) 
# Stepwise regression model
step.model.ph13_1  <- stepAIC(full.model.ph13_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph13_1)

car::vif(step.model.ph13_1) #catchments high

# remove highest
full.model.ph13_1a <- lm(log10_boutLength ~., data = ph13_1 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph13_1a  <- stepAIC(full.model.ph13_1a  , direction = "both", 
                               trace = FALSE, k = 5)
summary(step.model.ph13_1a) # r^2 = 0.6649
car::vif(step.model.ph13_1a) # no issues

#next
full.model.ph13_1b <- lm(log10_boutLength ~., data = ph13_1 %>% dplyr::select(-std_localCatchmentQuality)) 
# Stepwise regression model
step.model.ph13_1b  <- stepAIC(full.model.ph13_1b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph13_1b) #same model


#they are the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph13_1b, timeperiod = 13, center = 1)
# stepwise_absolute %<>%slice_head(n = 15)


# ph 14 ----

ph14_0 <- d %>% dplyr::filter(vep == 14, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph14_1 <- d %>% dplyr::filter(vep == 14, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph14 non-centers
corrplot(cor(ph14_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

# ph14_0 %<>% dplyr::select(-multiwalls)

# Fit the full model 
full.model.ph14_0 <- lm(log10_boutLength ~., data = ph14_0 ) 
# Stepwise regression model
step.model.ph14_0 <- stepAIC(full.model.ph14_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph14_0)

car::vif(step.model.ph14_0) #catchments are apparently almsot always a problem

# remove high vif

full.model.ph14_0a <- lm(log10_boutLength ~., data = ph14_0 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph14_0a <- stepAIC(full.model.ph14_0a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_0a) # 0.427, lots of things

car::vif(step.model.ph14_0a) #no issues

#remove other vif

full.model.ph14_0b <- lm(log10_boutLength ~., data = ph14_0 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph14_0b <- stepAIC(full.model.ph14_0b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_0b) #0.4237  lot sof things

car::vif(step.model.ph14_0b) # no issues

# b is simpler
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph14_0b, timeperiod = 14, center = 0)



#ph14 centers
corrplot(cor(ph14_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph14_1 %<>% dplyr::select( -towers)

# Fit the full model 
full.model.ph14_1 <- lm(log10_boutLength ~., data = ph14_1)
# Stepwise regression model
step.model.ph14_1  <- stepAIC(full.model.ph14_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1)

car::vif(step.model.ph14_1) #catchments issues

# remove high vif
full.model.ph14_1a <- lm(log10_boutLength ~., data = ph14_1 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph14_1a <- stepAIC(full.model.ph14_1a, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1a) # 0.6912, lots of things
car::vif(step.model.ph14_1a) #no issues

#remove other vif
full.model.ph14_1b <- lm(log10_boutLength ~., data = ph14_1 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph14_1b <- stepAIC(full.model.ph14_1b, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph14_1b) #same
car::vif(step.model.ph14_1b) 

#they're the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph14_1a, timeperiod = 14, center = 1)

# ph 15 ----

ph15_0 <- d %>% dplyr::filter(vep == 15, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph15_1 <- d %>% dplyr::filter(vep == 15, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph15 non-centers
corrplot(cor(ph15_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

# ph15_0 %<>% dplyr::select(-greathouse) 

# Fit the full model 
full.model.ph15_0 <- lm(log10_boutLength ~., data = ph15_0 ) 
# Stepwise regression model
step.model.ph15_0 <- stepAIC(full.model.ph15_0, direction = "both", 
                             trace = FALSE, k = 3.8415)
summary(step.model.ph15_0)

car::vif(step.model.ph15_0) #catchments and regional pop issues

# remove high vif
full.model.ph15_0a <- lm(log10_boutLength ~., data = ph15_0 %>% dplyr::select(-std_localCatchmentQuality))
# Stepwise regression model
step.model.ph15_0a <- stepAIC(full.model.ph15_0a, direction = "both", 
                              trace = FALSE, k = log(nrow(ph15_0)))
summary(step.model.ph15_0a) # adj R^2 = 0.4527
car::vif(step.model.ph15_0a) #no high

# remove high vif
full.model.ph15_0b <- lm(log10_boutLength ~., data = ph15_0 %>% dplyr::select(-std_catchmentRelToRegion))
# Stepwise regression model
step.model.ph15_0b <- stepAIC(full.model.ph15_0b, direction = "both", 
                              trace = FALSE, k = log(nrow(ph15_0)))
summary(step.model.ph15_0b) # same
car::vif(step.model.ph15_0b) #no high

#remove other vif
full.model.ph15_0c <- lm(log10_boutLength ~., data = ph15_0 %>% dplyr::select(-std_regionalPop))
# Stepwise regression model
step.model.ph15_0c <- stepAIC(full.model.ph15_0c, direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph15_0c) # same!
car::vif(step.model.ph15_0c) #none

#all the same
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph15_0a, timeperiod = 15, center = 0)



#ph15 centers
corrplot(cor(ph15_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph15_1 %<>% dplyr::select( -towers)

# Fit the full model 
full.model.ph15_1 <- lm(log10_boutLength ~., data = ph15_1)
# Stepwise regression model
step.model.ph15_1  <- stepAIC(full.model.ph15_1  , direction = "both", 
                              trace = FALSE, k = 3.8415)
summary(step.model.ph15_1)

car::vif(step.model.ph15_1) #  catchments

# remove highest
full.model.ph15_1a <- lm(log10_boutLength ~., data = ph15_1 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph15_1a  <- stepAIC(full.model.ph15_1a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph15_1a) #R^2 0.4917
car::vif(step.model.ph15_1a) #none


#next
full.model.ph15_1c <- lm(log10_boutLength ~., data = ph15_1 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph15_1c  <- stepAIC(full.model.ph15_1c  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph15_1c) # same as A

car::vif(step.model.ph15_1c)

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph15_1a, timeperiod = 15, center = 1)



# ph 16 ----
ph16_0 <- d %>% dplyr::filter(vep == 16, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph16_1 <- d %>% dplyr::filter(vep == 16, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph16 non-centers
corrplot(cor(ph16_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

# Fit the full model 
full.model.ph16_0 <- lm(log10_boutLength ~., data = ph16_0 ) 
# Stepwise regression model
step.model.ph16_0 <- stepAIC(full.model.ph16_0, direction = "both", 
                             trace = FALSE, k = 3.8416)
summary(step.model.ph16_0)

car::vif(step.model.ph16_0) #catchments and field contention

# remove highest
full.model.ph16_0a <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph16_0a  <- stepAIC(full.model.ph16_0a  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0a) #R^2 0.8351 
car::vif(step.model.ph16_0a) #none

#next
full.model.ph16_0b <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph16_0b  <- stepAIC(full.model.ph16_0b  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0b) # same as A

car::vif(step.model.ph16_0b)

#next
full.model.ph16_0c <- lm(log10_boutLength ~., data = ph16_0 %>% dplyr::select(-std_fieldContention )) 
# Stepwise regression model
step.model.ph16_0c  <- stepAIC(full.model.ph16_0c  , direction = "both", 
                               trace = FALSE, k = 3.8415)
summary(step.model.ph16_0c) #0.5883
car::vif(step.model.ph16_0c) #catchments still issue and it's lower

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph16_0a, timeperiod = 16, center = 0)



#ph16 centers
corrplot(cor(ph16_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph16_1 %<>% dplyr::select(-greatkiva, -towers)

# Fit the full model 
full.model.ph16_1 <- lm(log10_boutLength ~., data = ph16_1)
# Stepwise regression model
step.model.ph16_1  <- stepAIC(full.model.ph16_1  , direction = "both", 
                              trace = FALSE, k = 3.8416)
summary(step.model.ph16_1)

car::vif(step.model.ph16_1) # ok we've got std_catchmentRelToRegion and std_localCatchmentQuality


# remove highest
full.model.ph16_1a <- lm(log10_boutLength ~., data = ph16_1 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph16_1a  <- stepAIC(full.model.ph16_1a  , direction = "both", 
                               trace = FALSE, k = 3.8416)
summary(step.model.ph16_1a) #field contention and regional pop, R^2 0.7589
car::vif(step.model.ph16_1a) #none

#next
full.model.ph16_1b <- lm(log10_boutLength ~., data = ph16_1 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph16_1b  <- stepAIC(full.model.ph16_1b  , direction = "both", 
                               trace = FALSE, k = 3.8416)
summary(step.model.ph16_1b) # 0.7772

car::vif(step.model.ph16_1b) #no issues WOW

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph16_1b, timeperiod = 16, center = 1)



# ph 17 ----

ph17_0 <- d %>% dplyr::filter(vep == 17, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph17_1 <- d %>% dplyr::filter(vep == 17, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph17 non-centers
corrplot(cor(ph17_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph17_0 %<>% dplyr::select(-towers)

# Fit the full model 
full.model.ph17_0 <- lm(log10_boutLength ~., data = ph17_0 ) 
# Stepwise regression model
step.model.ph17_0 <- stepAIC(full.model.ph17_0, direction = "both", 
                             trace = FALSE, k = 3.8417)
summary(step.model.ph17_0) #0.7863

car::vif(step.model.ph17_0) #  no issues WOW


stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph17_0, timeperiod = 17, center = 0)



#ph17 centers
corrplot(cor(ph17_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph17_1 %<>% dplyr::select( -towers)

# Fit the full model 
full.model.ph17_1 <- lm(log10_boutLength ~., data = ph17_1)
# Stepwise regression model
step.model.ph17_1  <- stepAIC(full.model.ph17_1  , direction = "both", 
                              trace = FALSE, k = 3.8417)
summary(step.model.ph17_1) #R^2 0.7796

car::vif(step.model.ph17_1) # none 

# stepwise_absolute %<>% slice_head(n = 13)
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph17_1, timeperiod = 17, center = 1)


# ph 18 ----
ph18_0 <- d %>% dplyr::filter(vep == 18, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph18_1 <- d %>% dplyr::filter(vep == 18, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph18 non-centers
corrplot(cor(ph18_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph18_0 %<>% dplyr::select(-towers) 

# Fit the full model 
full.model.ph18_0 <- lm(log10_boutLength ~., data = ph18_0 ) 
# Stepwise regression model
step.model.ph18_0 <- stepAIC(full.model.ph18_0, direction = "both", 
                             trace = FALSE, k = 3.8418)
summary(step.model.ph18_0)

car::vif(step.model.ph18_0) #  std_catchmentRelToRegion and std_localCatchmentQuality


# remove highest
full.model.ph18_0a <- lm(log10_boutLength ~., data = ph18_0 %>% dplyr::select(-std_catchmentRelToRegion)) 
# Stepwise regression model
step.model.ph18_0a  <- stepAIC(full.model.ph18_0a  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_0a) #0.8521
car::vif(step.model.ph18_0a) # no issues

#next
full.model.ph18_0b <- lm(log10_boutLength ~., data = ph18_0 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph18_0b  <- stepAIC(full.model.ph18_0b  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_0b) #0.8507

car::vif(step.model.ph18_0b) 

# b is simpler
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph18_0b, timeperiod = 18, center = 0)



#ph18 centers
corrplot(cor(ph18_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" ) # there are so many correlations omg

ph18_1 %<>% dplyr::select(-towers,  -encwall)

# Fit the full model 
full.model.ph18_1 <- lm(log10_boutLength ~., data = ph18_1)
# Stepwise regression model
step.model.ph18_1  <- stepAIC(full.model.ph18_1  , direction = "both", 
                              trace = FALSE, k = 3.8418)
summary(step.model.ph18_1) #0.9316

car::vif(step.model.ph18_1) # ok we've got  std_localCatchmentQuality and std_catchmentRelToRegion, greathouse, greatkiva


# remove highest

full.model.ph18_1a <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph18_1a  <- stepAIC(full.model.ph18_1a  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1a) #0.9157

car::vif(step.model.ph18_1a) # greathouse and greatkiva still issues

#next
full.model.ph18_1b <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph18_1b  <- stepAIC(full.model.ph18_1b  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1b)

car::vif(step.model.ph18_1b)#identical


full.model.ph18_1c <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-std_localCatchmentQuality, -greathouse )) 
# Stepwise regression model
step.model.ph18_1c  <- stepAIC(full.model.ph18_1c  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1c) #0.8838

car::vif(step.model.ph18_1c) # no issue

#next
full.model.ph18_1d <- lm(log10_boutLength ~., data = ph18_1 %>% dplyr::select(-std_localCatchmentQuality, -greatkiva )) 
# Stepwise regression model
step.model.ph18_1d  <- stepAIC(full.model.ph18_1d  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph18_1d) #same

car::vif(step.model.ph18_1d)#no issue 

# c & d have no issues
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph18_1c, timeperiod = 18, center = 1)

# ph 19 ----

ph19_0 <- d %>% dplyr::filter(vep == 19, center == 0) %>% dplyr::select(-dummyCell, -center, -vep)
ph19_1 <- d %>% dplyr::filter(vep == 19, center == 1)%>% dplyr::select(-dummyCell, -center, -vep)

#ph19 non-centers
corrplot(cor(ph19_0, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" )

ph19_0 %<>% dplyr::select( -towers) 

# Fit the full model 
full.model.ph19_0 <- lm(log10_boutLength ~., data = ph19_0 ) 
# Stepwise regression model
step.model.ph19_0 <- stepAIC(full.model.ph19_0, direction = "both", 
                             trace = FALSE, k = 3.8419)
summary(step.model.ph19_0)

car::vif(step.model.ph19_0) #  catchment issues

#remove highest
full.model.ph19_0a <- lm(log10_boutLength ~., data = ph19_0 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph19_0a  <- stepAIC(full.model.ph19_0a  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph19_0a) #0.8643

car::vif(step.model.ph19_0a) # all good

#next
full.model.ph19_0b <- lm(log10_boutLength ~., data = ph19_0 %>% dplyr::select(-std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph19_0b  <- stepAIC(full.model.ph19_0b  , direction = "both", 
                               trace = FALSE, k = 3.8418)
summary(step.model.ph19_0b)

car::vif(step.model.ph19_0b)#identical

stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph19_0a, timeperiod = 19, center = 0)



#ph19 centers
corrplot(cor(ph19_1, use="pairwise.complete.obs"),
         method = "number",
         type = "lower" ) # there are so many correlations omg

ph19_1 %<>% dplyr::select( -plaza, -encwall, -towers)

# Fit the full model 
full.model.ph19_1 <- lm(log10_boutLength ~., data = ph19_1)
# Stepwise regression model
step.model.ph19_1  <- stepAIC(full.model.ph19_1  , direction = "both", 
                              trace = FALSE, k = 3.8419)
summary(step.model.ph19_1)

car::vif(step.model.ph19_1) # ok we've got std_catchmentRelToRegion,  std_localCatchmentQuality, and std_regionalPop


# remove highest

full.model.ph19_1a <- lm(log10_boutLength ~., data = ph19_1 %>% dplyr::select(-std_catchmentRelToRegion )) 
# Stepwise regression model
step.model.ph19_1a  <- stepAIC(full.model.ph19_1a  , direction = "both", 
                               trace = FALSE, k = 3.8419)
summary(step.model.ph19_1a) #0.7422

car::vif(step.model.ph19_1a) # no issues

#next
full.model.ph19_1b <- lm(log10_boutLength ~., data = ph19_1 %>% dplyr::select(-std_localCatchmentQuality )) 
# Stepwise regression model
step.model.ph19_1b  <- stepAIC(full.model.ph19_1b  , direction = "both", 
                               trace = FALSE, k = 3.8419)
summary(step.model.ph19_1b) #same

car::vif(step.model.ph19_1b) #no issue

#next
full.model.ph19_1c <- lm(log10_boutLength ~., data = ph19_1 %>% dplyr::select(-std_regionalPop )) 
# Stepwise regression model
step.model.ph19_1c  <- stepAIC(full.model.ph19_1c  , direction = "both", 
                               trace = FALSE, k = 3.8419)
summary(step.model.ph19_1c)

car::vif(step.model.ph19_1c)# still catchment issues


# they're the same when catchment removed
stepwise_absolute %<>% modifiedAddRow(addedRow = step.model.ph19_1b, timeperiod = 19, center = 1)



#
#make wide and export ----
stepwise_absolute %>% 
  saveRDS(here::here("data/data-derived/LMresults/stepwise_absolute.rds"))

stepwise_absolute %>% 
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
                  intercept,log10_boutNo, std_nHouse,std_fieldContention,	std_regionalPop	,
                  std_catchmentRelToRegion,
                  greatkiva,	plaza,	oversizepit,	greathouse,	multiwalls,	towers, encwall,	reservoir) %>% 
  dplyr::arrange(center) %>% 
  dplyr::left_join( robinson2020::vep_demography %>% dplyr::filter(`Study Area` == "CMV") %>% dplyr::select(Period, Start, End),
                    by = c("timeperiod" = "Period")) %>% 
  dplyr::mutate(center = factor(center, levels = c("0", "1"), labels = c("non-center", "center")),
                timeperiod = paste0(timeperiod, " (", Start, "-", End, ")")) %>% 
  dplyr::rename(`period (years CE)` = timeperiod)
dplyr::select(-Start, -End) %>% 
  readr::write_csv(here::here("data/data-derived/LMresults/stepwise_absolute.csv"),
                   na = "")

# make fig S3 ----

stepwise_absolute <- readRDS(here::here("data/data-derived/LMresults/stepwise_absolute.rds"))

absolute_long <- stepwise_absolute %>% 
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
                   by = c("timeperiod" = "timeperiod"))

A <-
  absolute_long %>% 
  dplyr::filter(variable %in% 
                  c( 
                     "std_fieldContention",
                     "std_regionalPop"
                     )) %>% 
  ggplot(aes(x = year, y = Estimate, color = factor(variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
  scale_color_manual(values = c("#009E73",  "#D55E00" ))+
  scale_x_continuous(breaks = seq(600, 1300, 100))+
  # scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1,1))+
  facet_wrap(vars(center), nrow =2)+
  theme_bw()+
  theme(#legend.position = "top",
    legend.title = element_blank(),
    axis.title.x =element_blank(),
    axis.text.x = element_blank())+ 
  guides(colour = guide_legend(override.aes = list(size=4)))


B <- 
  absolute_long %>% 
  dplyr::filter(variable %in% 
                  c("std_nHouse", 
                    "log10_boutNo", 
                    # "std_localCatchmentQuality",
                    "std_catchmentRelToRegion" )) %>% 
  ggplot(aes(x = year, y = Estimate, color = factor(variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
  scale_color_manual(values = c("#56B4E9", "#CC79A7",  "#E69F00"))+
  scale_x_continuous(breaks = seq(600, 1300, 100))+
  # scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1,1))+
  facet_wrap(vars(center), nrow =2)+
  theme_bw()+
  theme(#legend.position = "top",
    legend.title = element_blank(),
    axis.title.x =element_blank(),
    axis.text.x = element_blank())+ 
  xlab("Year AD")+
  guides(colour = guide_legend(override.aes = list(size=4)))


C <-
  absolute_long %>% 
  dplyr::filter(variable %in% 
                  c( "greatkiva", 
                     "plaza", 
                     "oversizepit",
                     "greathouse", 
                     "multiwalls", 
                     "encwall", 
                     "towers", 
                     "reservoir")) %>% 
    dplyr::mutate(variable = factor(variable, 
                  levels = c( "greatkiva", 
                              "plaza", 
                              "oversizepit",
                              "greathouse", 
                              "multiwalls", 
                              "encwall", 
                              "towers", 
                              "reservoir"))) %>% 
  ggplot(aes(x = year, y = Estimate, color = (variable)))+
  geom_hline(yintercept = 0, linewidth = 0.3)+
  geom_point(size = .08)+
    scale_color_manual(values = c("#ff0000",  "#ffd700", "#00ff00", "#ffb6c1", "#006400", "#00ffff", "#ff00ff", "#191970"))+
    # scale_color_viridis_d(option = "H")+
  scale_x_continuous(breaks = seq(600, 1300, 100), limits = c(600, 1300))+
  # scale_y_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1,1))+
  facet_wrap(vars(center), nrow =2)+
  theme_bw()+
  theme(#legend.position = "top",
    legend.title = element_blank())+ 
  guides(colour = guide_legend(override.aes = list(size=4)))


library(patchwork)
A / B / C

png(filename = "figS3_absolute.png", units = "in", width = 10, height = 12, res = 300)
A / B / C + plot_annotation(tag_levels = 'A')
dev.off()

