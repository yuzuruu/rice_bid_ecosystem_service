# Ecosystem service etimation using rice bid
# Est.: 01st. Nov. 2019


# ---- load.library ----
library(tidyverse)
library(dplyr)
library(GGally)
library(MASS)

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

# # save the figures
# ggsave("rice.bid.01.pdf", 
#        plot = rice.bid.01.pairs.01,
#        width = 100,
#        height = 100,
#        units = "cm"
# )
# 
# ggsave("rice.bid.02.pdf", 
#        plot = rice.bid.01.pairs.02,
#        width = 100,
#        height = 100,
#        units = "cm"
# )

#
## --- END ---

# ---- logistic.regression ----
# 
# logistic regression
# including all valuables
# The results indicate some valuables need not to be considered.
# It, however, should be noted that structure
# among valuables should be considered.
rice.bit.logit <- 
  glm(as.numeric(choice) ~ 
        bid + age + gender + married + education + 
        knowledge + income + donation + 
        environment + effect + forcon + forimp + forabil + 
        tourist + graduate + highschool + primary + anydona + 
        poorper + khanhan + khanhlam,
      family = binomial,
      data = rice.bid
  )
summary(rice.bit.logit)

# plot the regression results
# NOTE:
# The figure shows that probability to be 1 (choice = 1)
# declines slightly as sum of right side of model increases, 
# suggesting effect of bid and other valuables do not work
# no matter which option people might chose.
# Besides, range of coefficient intervals is too wide. 
#
# In concluding, the model should be modified while
# considering descriptive statistics of each valuable!!
# i.e. nesting
rice.bit.logit.01.plot <- 
  ggplot(rice.bid, 
         aes(x=bid + age + gender + married + education + 
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


