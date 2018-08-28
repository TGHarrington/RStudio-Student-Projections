library("tidyverse")
ds.obj <- read.csv("student-mat.csv", header = TRUE, sep = ";")

for(colnum in (1 : ncol(ds.obj))){
  print(paste("Class of", colnames(ds.obj)[colnum], "is",
              class(ds.obj[,colnum])))
}
summary(ds.obj)

ds.obj$Medu <- factor(ds.obj$Medu)
ds.obj$Fedu <- factor(ds.obj$Fedu)
ds.obj$traveltime <- factor(ds.obj$traveltime)
ds.obj$studytime <- factor(ds.obj$studytime)
ds.obj$failures <- factor(ds.obj$failures)
for(colnum in (1 : ncol(ds.obj))){
  print(paste("Class of", colnames(ds.obj)[colnum], "is",
              class(ds.obj[,colnum])))
}

summary(ds.obj)
for(colnum in (1 : ncol(ds.obj))){
  print(paste("Class of", colnames(ds.obj)[colnum], "is",
              class(ds.obj[,colnum])))}
  
lm.1 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob,
           data = ds.obj)
summary(lm.1)

lm.2 <- lm(G3 ~ reason + guardian + traveltime + studytime + failures + schoolsup + famsup + paid + activities + nursery,
           data = ds.obj)
summary(lm.2)

lm.3 <- lm(G3 ~ higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + absences,
           data = ds.obj)
summary(lm.3)

lm.4 <- lm(G3 ~ failures + internet + romantic + famrel + freetime + goout + reason + age,
           data = ds.obj)
summary(lm.4)

lm.5 <- lm(G3 ~ failures + romantic + goout + reason,
           data = ds.obj)
summary(lm.5)

ds2.obj <- read.csv("student-por.csv", header = TRUE, sep = ";")
summary(ds2.obj)
for(colnum in (1 : ncol(ds2.obj))){
  print(paste("Class of", colnames(ds2.obj)[colnum], "is",
              class(ds2.obj[,colnum])))
}

ds2.obj$Medu <- factor(ds2.obj$Medu)
ds2.obj$Fedu <- factor(ds2.obj$Fedu)
ds2.obj$traveltime <- factor(ds2.obj$traveltime)
ds2.obj$studytime <- factor(ds2.obj$studytime)
ds2.obj$failures <- factor(ds2.obj$failures)
for(colnum in (1 : ncol(ds2.obj))){
  print(paste("Class of", colnames(ds2.obj)[colnum], "is",
              class(ds2.obj[,colnum])))
}

lm.1 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob,
           data = ds2.obj)
summary(lm.1)

lm.2 <- lm(G3 ~ reason + guardian + traveltime + studytime + failures + schoolsup + famsup + paid + activities + nursery,
           data = ds2.obj)
summary(lm.2)

lm.3 <- lm(G3 ~ higher + internet + romantic + famrel + freetime + goout + Dalc + Walc + health + absences,
           data = ds2.obj)
summary(lm.3)

lm.4 <- lm(G3 ~ school + sex + studytime + failures + higher + internet,
           data = ds2.obj)
summary(lm.4)

lm.5 <- lm(G3 ~ school + studytime + failures + higher,
           data = ds2.obj)
summary(lm.5)


ds.mat <- read.csv("student-mat.csv", sep = ";", header = TRUE) 
ds.port <- read.csv("student-por.csv", sep = ";", header = TRUE) 
ds.merge2 <- merge(ds.mat, ds.port, by=c("school","sex","age","address", "famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")) 

for(colnum in (1 : ncol(merge2.obj))){
  print(paste("Class of", colnames(merge2.obj)[colnum], "is",
              class(merge2.obj[,colnum])))
}

for(colnum in (1 : ncol(ds2.obj))){
  print(paste("Class of", colnames(ds2.obj)[colnum], "is",
              class(ds2.obj[,colnum])))
}


summary(ds.mat)
summary(ds.port)
summary(ds.merge2)


lm.5 <- lm(G3 ~ school + studytime + failures + higher,
           data = merge2.obj)
summary(lm.5)

