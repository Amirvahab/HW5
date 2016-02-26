##################
# Amirvahab Fakhfouri #
# Assignment 4 ###########################################
### Q0
print("Amirvahab Fakhfouri")
print(1505020)
print("afakhfou@ucsc.edu")
##############################
install.packages("ggplot2")
library(ggplot2)
############################# Question 1
## Part a
Q1.a <- ggplot(diamonds,
            aes(x=x*y*z, y=price))
Q1.a +geom_point(aes(size=carat,colour=clarity)) + scale_x_log10() + scale_y_log10()
##############
## part b
Q1.b<-ggplot(diamonds, aes(carat,..density..,fill=clarity))
Q1.b + geom_histogram(bins=30)+ facet_grid(cut ~ .)
##############
## part c
Q1.c<- ggplot(diamonds, aes(y = price, x = cut))  
Q1.c+ geom_violin(alpha=1)+geom_jitter(alpha=0.01)
##############
############################ Question 3
## part a
library("foreign")
df <-read.dta("C:\\Users\\VAIO-VAIO\\Desktop\\UCSC\\Q2\\Metric2\\Assignment\\HW1\\org_example.dta")
library(dplyr)
library(ggplot2)
df1<- df %>%
  mutate(
    date = paste(year,month,"01",sep="-"),
    date = as.Date(date,format = "%Y-%m-%d")
  ) %>%
  group_by(date) %>%
  summarise(
    rw.median = median(rw,na.rm=T),
    first.Q =quantile(rw,prob=c(0.25),na.rm=T),
    third.Q = quantile(rw,prob=c(0.75),na.rm=T),
    
    first.D = quantile(rw,prob=c(0.1),na.rm=T),
    ninth.D = quantile(rw,prob=c(0.9),na.rm=T)
  )

ggplot(
  data = df1 ,
  aes(x=date,y = rw.median)
) +
  geom_line(alpha=1) + ylim(0, 50)+geom_ribbon(ymin=df1$first.Q,ymax=df1$third.Q,fill="black",alpha=0.5)+
  geom_ribbon(ymin=df1$first.D,ymax=df1$ninth.D,fill="black",alpha=0.3)


#################
## part b

Q3.b<- df %>%
  mutate(
    date = paste(year,month,"01",sep="-"),
    date = as.Date(date,format = "%Y-%m-%d")
  ) %>%
  group_by(date,educ) %>%
  summarise(
    rw.median = median(rw,na.rm=T)
  )

ggplot(
  data = Q3.b ,
  aes(x=date,y = rw.median,colour=educ)
) +
  geom_line()

#######################################################
    ################# End ####################




















































