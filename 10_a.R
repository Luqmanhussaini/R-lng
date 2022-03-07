10.Parametric tests
 a) paired t-test
 
 
 #paired t-test for two-means- SST
x<-c(45,	73,	46,	124,	33,	57,	83,	34,	26,	17)
y<-c(36,	60,	44,	119,	35,	51,	77,	29,	24,	11)
print("enter the level of significance")
alpha<-scan()
d<-x-y
n<-length(d)
dbar<-mean(d)
std<-sd(d)
values<-c(dbar,std)
print("values of dbar and standard deviation of differences")
print(round(values,digits=4))
t<-t.test(x,y,paired=TRUE)
print(t)
print("Table value for two-tailed test:")
tablevalue<-qt(1-alpha/2, df=n-1)
print(round(tablevalue,digits=4))
print("Table value for one-tailed test:")
tablevalue<-qt(1-alpha, df=n-1)
print(round(tablevalue,digits=3))


