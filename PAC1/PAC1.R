# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')

#titanic <- read.csv("C:/Users/xavim/Rprojects/MD-PAC1/titanic.csv")
totalData <- read.csv("C:/Users/xavim/Rprojects/DataMining/PAC1/titanic.csv", stringsAsFactors = FALSE)
filas=dim(totalData)[1]
totalData_crew=subset(totalData, totalData$class=="engineering crew")

#Here we get the structure of the data frame
str(totalData)
#Here we get the basic stats
summary(totalData)
#Here we get the empty values
colSums(is.na(totalData))
colSums(totalData=="")

#We set the empty country values to "Desconegut"
totalData$country[is.na(totalData$country)] <- "Desconegut"

#We set the empty age values to the mean value
totalData$age[is.na(totalData$age)] <- mean(totalData$age,na.rm=T)

#Discretitzem sobre les edats per obtenir informació d'intervals amb una columna anomenada "segment_edat"
summary(totalData[,"age"])
totalData["segment_edat"] <- cut(totalData$age, breaks = c(0,10,20,30,40,50,60,70,100), labels = c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79"))

#We get the first rows of the data frame object called 'totalData'
head(totalData)

#Here we create the elements by the field 'segment_edat' and then we create a plot with this information
plot(totalData$segment_edat)

#Discretitzem de nou i generem el plot per segments d'edats per la tripulació
totalData_crew["segment_edat"] <- cut(totalData_crew$age, breaks = c(0,10,20,30,40,50,60,70,100), labels = c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79"))
plot(totalData_crew$segment_edat)

#Visualització de la relació entre les variables 'geneder' i 'survived'
ggplot(data=totalData[1:filas,],aes(x=gender,fill=survived))+geom_bar()

#Relació entre 'survived' i 'Embarked'. Relació dels que van sobreviure i on van embarcar.
#Al posar geom_bar(position="fill") és un valor que treballa entre 0 i 1. Probabilitats i no numeros de vius/morts
#La gent que va embarcar al port 'C'va tenir un factor de supervivencia molt més alt que els altres. (56%)
ggplot(data = totalData[1:filas,],aes(x=embarked,fill=survived))+geom_bar(position="fill")+ylab("Frequència") 

#Generem una taula que separ la frequencia de si i no supervivencia, respecte on s'ha embarcat 
t<-table(totalData[1:filas,]$embarked,totalData[1:filas,]$survived)
for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
}

#La taula 't' ens servirà per generar un gràfica de freqüències que ens permet relacionar
#les variables 'Embarked', 'Survived' i 'Class'.
#La següent gràfica ens permet veure si hi ha relació entre la class comprada, el lloc d'embarcament
#i la possibilitat de supervivència. 
#Tenien més diners al port C?
ggplot(data = totalData[1:filas,],aes(x=embarked,fill=survived))+geom_bar(position="fill")+facet_wrap(~class)


#Realitzem dues noves comparatives. Survived/SibSp i Survived/Parch
#SibSp -> Indica si hi ha germans o conjugues abord
#Parch -> Indica el nombre de pares/fills hi ha abord
#A partir d'aquestes grafiques podem veure si hi ha relació entre la freqüència
#de supervivència i el fet de que hi havia familiars abord. 
ggplot(data = totalData[1:filas,],aes(x=sibsp,fill=survived))+geom_bar()
ggplot(data = totalData[1:filas,],aes(x=parch,fill=survived))+geom_bar()

#Generem una nova varaible anomenada 'Grandaria de familia'.
#A través d'aquesta nova dada i gràfica (FamilySize/Survived)
#podem veure que majoritariament es viatjava en familia, pero això no que no fa augmentar la possibilitat de supervivència.
totalData$FamilySize <- totalData$sibsp + totalData$parch + 1;
totalData1<-totalData[1:filas,]
ggplot(data = totalData1[!is.na(totalData[1:filas,]$FamilySize),],aes(x=FamilySize,fill=survived))+geom_histogram(binwidth =1,position="fill")+ylab("Freqüència")

#Analitzem la relacio Age/Survived. Utilitzem position="fill" per obtenir la proporció acumulada d'un atribut dins l'altre
#A través d'aquests veiem i amb la proporció acumulada, podem observar que els nens van tenir majors possibilitats de salvar-se.
ggplot(data = totalData1[!(is.na(totalData[1:filas,]$age)),],aes(x=age,fill=survived))+geom_histogram(binwidth =3)
ggplot(data = totalData1[!is.na(totalData[1:filas,]$age),],aes(x=age,fill=survived))+geom_histogram(binwidth = 3,position="fill")+ylab("Freqüència")



#EXERCICI 2

adult <- read.csv("C:/Users/xavim/Rprojects/DataMining/PAC1/adult.data", stringsAsFactors = FALSE, header=FALSE)
rows=dim(adult)[1]
names(adult) <- c("age", "workclass", "fnlwgt","education", "education-num", "marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","earnings")
