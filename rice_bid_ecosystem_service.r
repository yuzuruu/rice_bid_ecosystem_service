

# ---- load.library ----
library(tidyverse)
library(dplyr)
library(GGally)

# ---- read.data ----
# read data
rice.bid <- readxl::read_excel("rice_bid_ecosystem_service.xlsx")
# 
rice.bid.01 <- 
  rice.bid %>% 
  dplyr::mutate(choice = as.factor(choice),
                bid = as.factor(bid)
  )

# ---- pairs ----
# by choice (1 or 0)
rice.bid.01.pairs.01 <- 
  rice.bid.01 %>% 
  ggpairs(columns = 3:ncol(rice.bid.01),
          aes(colour = choice,
              alpha = 0.5)
  )
# by bid (from 1 to 5)
rice.bid.01.pairs.02 <- 
  rice.bid.01 %>% 
  ggpairs(columns = 3:ncol(rice.bid.01),
          aes(colour = bid,
              alpha = 0.5
          )
  )

# save the figures
ggsave("rice.bid.01.pdf", 
       plot = rice.bid.01.pairs.01,
       width = 100,
       height = 100,
       units = "cm"
)

ggsave("rice.bid.02.pdf", 
       plot = rice.bid.01.pairs.02,
       width = 100,
       height = 100,
       units = "cm"
)

#
## --- END ---

# ---- logistic.regression ----
# 
# regression
rice.bit.logit <- 
  glm(as.numeric(choice) ~ 
        age + gender + married + education + 
        knowledge + income + donation + 
        environment + effect + forcon + forimp + forabil + 
        tourist + graduate + highschool + primary + anydona + 
        poorper + khanhan + khanhlam,
      family = binomial,
      data = rice.bid
  )
summary(rice.bit.logit)

# # combination of models
# # WARNING!!
# # The computation period tends to be long. 
# library(MuMIn)
# options(na.action = "na.fail")
# hogehoge <- dredge(rice.bit.logit, rank = "AIC")
#
#


# plot the regression results
rice.bit.logit.01.plot <- 
  ggplot(rice.bid, 
         aes(x=age + gender + married + education + 
               knowledge + income + donation + 
               environment + effect + forcon + forimp + forabil + 
               tourist + graduate + highschool + primary + anydona + 
               poorper + khanhan + khanhlam, 
             y=as.numeric(choice))
  ) + 
  geom_point() + 
  stat_smooth(method="glm", 
              method.args=list(family="binomial"), 
              se=TRUE
  ) +
  theme_classic()
print(rice.bit.logit.01.plot)

#
## --- END ---
