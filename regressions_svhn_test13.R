require(faraway)
require(data.table)
require(ggplot2)
require(ggthemes)
require(car)
require(foreach)
setwd("/Users/hansroggeman/Rscripts/")


list.files()
ensembleData = fread("dwnld_ensemble_regdata_svhn.csv")
ensembleData[,Layer:=NULL]

ensembleData2 = fread("dwnld_ensemble_regdata2_svhn.csv")
ensembleData2$Type = ifelse(ensembleData2$NumOfA==0, ensembleData2$TypeB, ensembleData2$TypeA)
ensembleData2$NumOfModels =  ifelse(ensembleData2$NumOfA==0, ensembleData2$NumOfB, ensembleData2$NumOfA)
ensembleData2[,Layer:=NULL]
ensembleData2[,TypeA:=NULL]
ensembleData2[,TypeB:=NULL]
ensembleData2[,NumOfA:=NULL]
ensembleData2[,NumOfB:=NULL]
ensembleData2[,RepC:=NULL]

singleTypeEnsemble = rbind(ensembleData,ensembleData2, fill=TRUE)[,.(Type,NumOfModels, Acc)]
singleTypeEnsemble$logNumOfModels = log(singleTypeEnsemble$NumOfModels)
singleTypeEnsemble$sqLogNumOfModels = log(singleTypeEnsemble$NumOfModels)*log(singleTypeEnsemble$NumOfModels)

regEnsemble = lm(data=singleTypeEnsemble , Acc ~ NumOfModels + as.factor(Type) )
summary(regEnsemble)
regEnsemble2 = lm(data=singleTypeEnsemble, Acc ~ logNumOfModels + sqLogNumOfModels + as.factor(Type) )
summary(regEnsemble2)
newdatadf = rbindlist( foreach (modType = unique(singleTypeEnsemble$Type)) %do%
{
  return (data.table("logNumOfModels"= log(2:40), "sqLogNumOfModels"= log(2:40)*log(2:40), "Type" = modType) )  
} )

newdatadf$predicted = predict(regEnsemble2, newdatadf)

library(RColorBrewer)
myColors <- brewer.pal(length(unique(singleTypeEnsemble$Type)),"Set1")
names(myColors) <- unique(singleTypeEnsemble$Type)
colScale <- scale_colour_manual(name = "modelType",values = myColors)

ggplot(singleTypeEnsemble, aes(x=NumOfModels,y=Acc, fill=Type, color=Type)) + geom_point()

ggplot(singleTypeEnsemble, aes(x=NumOfModels,y=Acc, fill=Type, color=Type)) + 
  geom_point() + colScale



regEnsemble2$coefficients["logNumOfModels"]

ggplot(singleTypeEnsemble, 
       aes(x=regEnsemble2$coefficients["logNumOfModels"]*logNumOfModels+
             regEnsemble2$coefficients["sqLogNumOfModels"]*sqLogNumOfModels,y=Acc, fill=Type, color=Type)) + 
  geom_point() + xlab("t(NumOfModels)")


plot(regEnsemble)




