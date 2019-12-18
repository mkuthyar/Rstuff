
fgdata = read.csv("FGdat.csv")
head(fgdata)
summary(fgdata)

fg13 = subset(fgdata, Season == "2013")
fg14 = subset(fgdata, Season == "2014")
summary(fg13)

babipavg_13 <- mean(fg13$BABIP)
babipavg_14 <- mean(fg14$BABIP)

print(babipavg_13)
print(babipavg_14)

fgtrout = subset(fgdata, Name =="Mike Trout")
fgtrout