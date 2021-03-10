###Pre-processing
NBA_info.df=read.csv("/Users/alston/Desktop/Final Project_G9/NBA_dataset1.csv")

NBA.df_new = NBA_info.df[,c(2,9,5,11,12,14,15,17,18,21,22,26,27,61,65)]

EHR = NBA_info.df[,26]/NBA_info.df[,27]
RW = ifelse(NBA.df_new$W_PCT>=0.6,1,0)
NBA.df_new=cbind(NBA.df_new,EHR,RW)
attach(NBA.df_new)

NBA.SG=NBA.df_new[which(NBA.df_new$POSITION=='SG'| NBA.df_new$POSITION=='PG'),]
NBA.PG=NBA.df_new[which(NBA.df_new$POSITION=='PG'),]
NBA.PF=NBA.df_new[which(NBA.df_new$POSITION=='PF'),]
NBA.SF=NBA.df_new[which(NBA.df_new$POSITION=='SF' | NBA.df_new$POSITION=='PF'),]
NBA.C=NBA.df_new[which(NBA.df_new$POSITION=='C'),]

###Correlation degree between variables
library(gpairs)
gpairs(NBA.df_new[c(2:14,16)])
cor(NBA.df_new[c(2:14,16)])


###PG
###Correlation degree between variables
gpairs(NBA.PG[c(2:14,16)])
cor(NBA.PG[c(2:14,16)])

##split data as training and validation
set.seed(123) 
train.index = sample(c(1:24), 19, replace=FALSE)  
train.PG.df = NBA.PG[train.index,]
valid.PG.df = NBA.PG[-train.index,]

###linear regression 
PG_lm = lm(W_PCT ~ AGE + OFF_RATING + DEF_RATING + AST_TO + 
             OREB_PCT +  PTS + EFG_PCT + TS_PCT + FGM + 
             FGA , data=train.PG.df)
summary(PG_lm)

library(forecast)
PG.pred.in=fitted(PG_lm)
accuracy(PG.pred.in,train.PG.df$W_PCT)

PG.lm.pred = predict(PG_lm, valid.PG.df) 
accuracy(PG.lm.pred,valid.PG.df$W_PCT)


###SG
###Correlation degree between variables
gpairs(NBA.SG[c(2:14,16)])
cor(NBA.SG[c(2:14,16)])

##split data as training and validation
set.seed(123)  
train.index = sample(c(1:34), 24, replace=FALSE)  
train.SG.df = NBA.SG[train.index,]
valid.SG.df = NBA.SG[-train.index,]

###linear regression 
SG_lm = lm(W_PCT ~ OFF_RATING + DEF_RATING + 
             OREB_PCT  + EFG_PCT + TS_PCT + FGM + 
             FGA + EHR, data=train.SG.df)
summary(SG_lm)z

SG.pred.in=fitted(SG_lm)
accuracy(SG.pred.in,train.SG.df$W_PCT)

SG.lm.pred = predict(SG_lm, valid.SG.df) 
accuracy(SG.lm.pred,valid.SG.df$W_PCT)

###PF
###Correlation degree between variables
gpairs(NBA.PF[c(2:14,16)])
cor(NBA.PF[c(2:14,16)])

##split data as training and validation
set.seed(123)  
train.index = sample(c(1:21), 15, replace=FALSE)  
train.PF.df = NBA.PF[train.index,]
valid.PF.df = NBA.PF[-train.index,]

###linear regression
PF_lm = lm(W_PCT ~ AGE + OFF_RATING + DEF_RATING + 
             OREB_PCT + DREB_PCT + PTS + EFG_PCT + 
             TS_PCT , data=train.PF.df)
summary(PF_lm)

PF.pred.in=fitted(PF_lm)
accuracy(PF.pred.in,train.PF.df$W_PCT)

PF.lm.pred = predict(PF_lm, valid.PF.df) 
accuracy(PF.lm.pred,valid.PF.df$W_PCT)

###SF
###Correlation degree between variables
gpairs(NBA.SF[c(2:14,16)])
cor(NBA.SF[c(2:14,16)])

##split data as training and validation
set.seed(123)  
train.index = sample(c(1:35), 25, replace=FALSE)  
train.SF.df = NBA.SF[train.index,]
valid.SF.df = NBA.SF[-train.index,]

###linear regression
SF_lm = lm(W_PCT ~ AGE + OFF_RATING + DEF_RATING + AST_PCT + AST_TO + 
             OREB_PCT + DREB_PCT + PTS + EFG_PCT + TS_PCT + EHR, data=train.SF.df)
summary(SF_lm)

SF.pred.in=fitted(SF_lm)
accuracy(SF.pred.in,train.SF.df$W_PCT)

SF.lm.pred = predict(SF_lm, valid.SF.df) 
accuracy(SF.lm.pred,valid.SF.df$W_PCT)


###C
###Correlation degree between variables
gpairs(NBA.C[c(2:14,16)])
cor(NBA.C[2:14])
##split data as training and validation
set.seed(123)  
train.index = sample(c(1:31), 22, replace=FALSE)  
train.C.df = NBA.C[train.index,]
valid.C.df = NBA.C[-train.index,]

###linear regression 
C_lm = lm(W_PCT ~ AGE + OFF_RATING + DEF_RATING + 
             PTS +  FGM + FGA + EHR, data=NBA.C)
summary(C_lm)

C.pred.in=fitted(C_lm)
accuracy(C.pred.in,train.C.df$W_PCT)

C.lm.pred = predict(C_lm, valid.C.df) 
accuracy(C.lm.pred,valid.C.df$W_PCT)


###1.2 Decide who is the higher performance player in each position
NBA.SG.NEW = NBA.SG[order(NBA.SG$W_PCT, decreasing = TRUE),c(1,2)]
NBA.PG.NEW = NBA.PG[order(NBA.PG$W_PCT, decreasing = TRUE),c(1,2)]
NBA.PF.NEW = NBA.PF[order(NBA.PF$W_PCT, decreasing = TRUE),c(1,2)]
NBA.SF.NEW = NBA.SF[order(NBA.SF$W_PCT, decreasing = TRUE),c(1,2)]
NBA.C.NEW = NBA.C[order(NBA.C$W_PCT, decreasing = TRUE),c(1,2)]

###1.2 Export to .csv
write.table(NBA.SG.NEW,file="/Users/alston/Desktop/Final Project_G9/SG_performance.csv",sep=",",row.names=F, na = "NA")
write.table(NBA.PG.NEW,file="/Users/alston/Desktop/Final Project_G9/PG_performance.csv",sep=",",row.names=F, na = "NA")
write.table(NBA.PF.NEW,file="/Users/alston/Desktop/Final Project_G9/PF_performance.csv",sep=",",row.names=F, na = "NA")
write.table(NBA.SF.NEW,file="/Users/alston/Desktop/Final Project_G9/SF_performance.csv",sep=",",row.names=F, na = "NA")
write.table(NBA.C.NEW,file="/Users/alston/Desktop/Final Project_G9/C_performance.csv",sep=",",row.names=F, na = "NA")





