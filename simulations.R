pacman::p_load(tidyverse, simr, lme4)
data <- read_csv("data_clean.csv") %>%
  select(Child.ID,Visit, Diagnosis, types_CHI) %>%
  drop_na %>%
  mutate(Child.ID = as.factor(Child.ID))

m = lme4::lmer(types_CHI ~ Visit + Diagnosis + (1 + Visit | Child.ID), data, REML = FALSE)

# estimated effect size
powerV <- simr::powerSim(m,test = simr::fixed("Visit"),nsim = 50)
powerD <- simr::powerSim(m,simr::fixed("Diagnosis"),nsim = 50)

# minimum effect size
fixef(m)["Visit"] <- 10
fixef(m)["DiagnosisTD"] <- 30
powerCurveV = powerCurve(m,fixed("Visit"),along="Child.ID", nsim=50)
powerCurveD = powerCurve(m,fixed("Diagnosis"),along="Child.ID", nsim=161)
plot(powerCurveV)
plot(powerCurveD)

m2 <- extend(m, along="Child.Id", n = 161)
