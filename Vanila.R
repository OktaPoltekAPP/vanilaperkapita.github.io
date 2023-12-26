setwd("D:/METOPEL UAS/OKTA METOPEL")
library(readxl)
library(tidyverse)
library(kableExtra)
read_excel("vanila2.xlsx")
dat <- read_excel("vanila2.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Plot 
plot(dat$lx,dat$ly,xlab="nilai kurs tengah",ylab="FOB Vanila")
abline(h=0)

# regresi
reg1<-lm(lgdpp~lfob++lgdp+lkurs,data=dat)
summary(reg1)
# Pengaruh Ekspor vanilla terhadap pertumbuhan ekonomi Indonesia.

 #Y = FOB, SS = Harga per ton, X = Kurs, SD = Harga/ton (Rp.)

# Membuat error
dat$u<-resid(reg1)

# plot eror

dat$m<-resid(reg1)
plot(dat$lfob,dat$m,xlab="FOB Vanilla",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$lgdp,dat$m,xlab="GDP",ylab="error")
abline(h=0)

dat$m<-resid(reg1)
plot(dat$lgdpp,dat$m,xlab="GDP perkapita",ylab="error")
abline(h=0)

dat$m<-resid(reg1)
plot(dat$lkurs,dat$m,xlab="Nilai Tukar IDR/USD",ylab="error")
abline(h=0)
