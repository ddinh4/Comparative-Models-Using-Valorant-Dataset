library(easypackages)
libraries("shiny","shinythemes","tidyverse", "readxl","MASS", "AUC", "caret",
          "car", "glmnet", "pls", "mda","mlbench")

df1 = read_excel("G:/My Drive/SPRING 2024/EXST7152/Midterm Project/Data/DD_CompetitiveCareer_02_25_2023.xlsx")
df1 = df1 %>%
  mutate(across(c("Agent", "Map","Results","Placement", "AverageRankofMatch"), as.factor)) 
df1 = df1[,-which(names(df1)=="Match Score")]
df1 =  df1[df1$Results != "D", ]
df1$Results <- droplevels(df1$Results)
df1$Results <- ifelse(df1$Results == "W", 1, 0)
df1$Results = as.factor(df1$Results)
df1$Date <- as.Date(df1$Date)
df2 = df1[c("Results", setdiff(names(df1), "Results"))]

dummy.cols = c("Agent", "Map", "Placement", "AverageRankofMatch")
dummy.df3 = model.matrix(~ . -1, data = df2[, dummy.cols])
df3 = cbind(df2, dummy.df3)
df3 = df3[,-which(names(df3) %in% c("Agent", "Map", "Placement", "AverageRankofMatch"))]

set.seed(1)
n = length(df3)
N = nrow(df3)
train <- sample(1:N, N*.8, replace = F)
test <- (-train)
y = df3$Results
y.test <- y[test]
x.test = df3[test, 3:n]
x.train = df3[train, 3:n]
y.train = y[train]
train.df = data.frame(df3[train,])
test.df = data.frame(df3[test,])

mars.fit <- mars(x.train,y.train)

saveRDS(mars.fit, "mars_model.RDS")
