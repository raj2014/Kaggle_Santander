<<<<<<< HEAD
# ensemble mixer

e1<-read.csv(file="submission_3.csv",header=TRUE)
e2<-read.csv(file="submission.csv",header=TRUE)
e3<-read.csv(file="submission_4.csv",header=TRUE)

e1<-e1[order(e1$ID),]
e2<-e2[order(e1$ID),]
e3<-e3[order(e1$ID),]

e_final<-data.frame(ID=e1$ID,TARGET=0.8*e1$TARGET+0.7*e2$TARGET)

write.csv(e_final,"submission_e5.csv", row.names = FALSE)
=======
# ensemble mixer

e1<-read.csv(file="submission_3.csv",header=TRUE)
e2<-read.csv(file="submission.csv",header=TRUE)
e3<-read.csv(file="submission_4.csv",header=TRUE)

e1<-e1[order(e1$ID),]
e2<-e2[order(e1$ID),]
e3<-e3[order(e1$ID),]

e_final<-data.frame(ID=e1$ID,TARGET=0.45*e1$TARGET+0.45*e2$TARGET+0.1*e3$TARGET)

write.csv(e_final,"submission_e2.csv", row.names = FALSE)
>>>>>>> origin/master
