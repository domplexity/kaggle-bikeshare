# AGENDA
# 1 simple models, simple features
# 2 simple models, all features
# 3 simple models, feature engineering
# 4 complex models

# todo: split train set, cross-validate

# 1
# try and compare a bunch of different (simple) models on the same featureset
# used features: houroftheday, season, weather, workingday
# predict: count


#linear regression

m1.glm <- glm(formula <- count ~ houroftheday * season * weather * workingday,
              family = "gaussian",
              data = train)

summary(m1.glm)

m1.glm$aic
