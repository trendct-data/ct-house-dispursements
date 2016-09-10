library(dplyr)
library(ctnamecleaner)

larsonoe <- read.csv("larson-operating-expenditures.csv", stringsAsFactors=FALSE)
larsonoe2 <- read.csv("larson-other-dispursements.csv", stringsAsFactors=FALSE)
larson.cat <- read.csv("larson-cat.csv", stringsAsFactors=FALSE)

larsonoe <- rbind(larsonoe, larsonoe2)
larsonoe <- left_join(larsonoe, larson.cat)
larsonoe$Candidate <- "Larson, John"

larsonoe$City <- gsub("EADT HARTFORD", "EAST HARTFORD", larsonoe$City)
larsonoe$Amount <- gsub("\\$", "", larsonoe$Amount)
larsonoe$Amount <- as.numeric(as.character(larsonoe$Amount))

larson_purpose <- larsonoe %>%
  group_by(Purpose) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(larson_purpose, "larson_purpose.csv")

larson_payee <- larsonoe %>%
  group_by(Payee.Name) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(larson_payee, "larson_payee.csv")

larson_state <- larsonoe %>%
  group_by(State) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

colnames(larson_state) <- c("State", "Larson")

write.csv(larson_state, "larson_state.csv")

larson_town <- larsonoe %>%
  filter(State=="CT") 

larson_clean <- ctnamecleaner(City, larson_town)

larson_town <- larson_clean %>%
  group_by(real.town.name) %>%
  summarise(Larson=sum(Amount))%>%
  arrange(-Larson)

##

himesoe <- read.csv("himes-operating-expenditures.csv", stringsAsFactors=FALSE)
himesoe2 <- read.csv("himes-other-dispursements.csv", stringsAsFactors=FALSE)
himes.cat <- read.csv("himes-cat.csv", stringsAsFactors=FALSE)

himesoe <- rbind(himesoe, himesoe2)
himesoe <- left_join(himesoe, himes.cat)
himesoe$Candidate <- "Himes, James"

himesoe$Amount <- gsub("\\$", "", himesoe$Amount)
himesoe$Amount <- as.numeric(as.character(himesoe$Amount))

himes_purpose <- himesoe %>%
  group_by(Purpose) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(himes_purpose, "himes_purpose.csv")

himes_payee <- himesoe %>%
  group_by(Payee.Name) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(himes_payee, "himes_payee.csv")

himes_state <-himesoe %>%
  group_by(State) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)
colnames(himes_state) <- c("State", "Himes")

write.csv(himes_state, "himes_state.csv")

himes_town <- himesoe %>%
  filter(State=="CT") 

himes_clean <- ctnamecleaner(City, himes_town)

himes_town <- himes_clean %>%
  group_by(real.town.name) %>%
  summarise(Himes=sum(Amount))%>%
  arrange(-Himes)

##
all <- rbind(larsonoe, himesoe)
write.csv(all, "all.csv")
##

estyoe <- read.csv("esty-operating-expenditures.csv", stringsAsFactors=FALSE)
estyoe2 <- read.csv("esty-other-dispursements.csv", stringsAsFactors=FALSE)
esty.cat <- read.csv("esty-cat.csv", stringsAsFactors=FALSE)

estyoe <- rbind(estyoe, estyoe2)
estyoe <- left_join(estyoe, esty.cat)

estyoe$Candidate <- "Esty, Elizabeth"


estyoe$Amount <- gsub("\\$", "", estyoe$Amount)
estyoe$Amount <- as.numeric(as.character(estyoe$Amount))

esty_purpose <- estyoe %>%
  group_by(Purpose) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(esty_purpose, "esty_purpose.csv")


esty_payee <- estyoe %>%
  group_by(Payee.Name) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(esty_payee, "esty_payee.csv")

esty_state <- estyoe %>%
  group_by(State) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)
colnames(esty_state) <- c("State", "Esty")

write.csv(esty_state, "esty_state.csv")

esty_town <- estyoe %>%
  filter(State=="CT") 

esty_clean <- ctnamecleaner(City, esty_town)

esty_town <- esty_clean %>%
  group_by(real.town.name) %>%
  summarise(Esty=sum(Amount))%>%
  arrange(-Esty)

##

delaruooe <- read.csv("delaruo-operating-expenditures.csv", stringsAsFactors=FALSE)
delaruooe2 <- read.csv("delaruo-other-dispursements.csv", stringsAsFactors=FALSE)
delaruo.cat <- read.csv("delaruo-cat.csv", stringsAsFactors=FALSE)

delaruooe <- rbind(delaruooe, delaruooe2)
delaruooe <- left_join(delaruooe, delaruo.cat)

delaruooe$Candidate <- "DeLaruo, Rosa"


delaruooe$Amount <- gsub("\\$", "", delaruooe$Amount)
delaruooe$Amount <- as.numeric(as.character(delaruooe$Amount))

delaruo_purpose <- delaruooe %>%
  group_by(Purpose) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(delaruo_purpose, "delaruo_purpose.csv")

delaruo_payee <- delaruooe %>%
  group_by(Payee.Name) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(delaruo_payee, "delaruo_payee.csv")

delaruo_state <- delaruooe %>%
  group_by(State) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)
colnames(delaruo_state) <- c("State", "Delaruo")

write.csv(delaruo_state, "delaruo_state.csv")


delaruo_town <- delaruooe %>%
  filter(State=="CT") 

delaruo_clean <- ctnamecleaner(City, delaruo_town)

delaruo_town <- delaruo_clean %>%
  group_by(real.town.name) %>%
  summarise(Delaruo=sum(Amount))%>%
  arrange(-Delaruo)

##

courtneyoe <- read.csv("courtney-operating-expenditures.csv", stringsAsFactors=FALSE)
courtneyoe2 <- read.csv("courtney-other-dispursements.csv", stringsAsFactors=FALSE)
courtney.cat <- read.csv("courtney-cat.csv", stringsAsFactors=FALSE)


courtneyoe <- rbind(courtneyoe, courtneyoe2)
courtneyoe <- left_join(courtneyoe, courtney.cat)

courtneyoe$Candidate <- "Courtney, Joe"

courtneyoe$Amount <- gsub("\\$", "", courtneyoe$Amount)
courtneyoe$Amount <- as.numeric(as.character(courtneyoe$Amount))

courtney_purpose <- courtneyoe %>%
  group_by(Purpose) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(courtney_purpose, "courtney_purpose.csv")

courtney_payee <- courtneyoe %>%
  group_by(Payee.Name) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)

write.csv(courtney_payee, "courtney_payee.csv")

courtney_state <- courtneyoe %>%
  group_by(State) %>%
  summarise(Total=sum(Amount))%>%
  arrange(-Total)
colnames(courtney_state) <- c("State", "Courtney")

write.csv(courtney_state, "courtney_state.csv")

courtney_town <- courtneyoe %>%
  filter(State=="CT") 

courtney_clean <- ctnamecleaner(City, courtney_town)

courtney_town <- courtney_clean %>%
  group_by(real.town.name) %>%
  summarise(Courtney=sum(Amount))%>%
  arrange(-Courtney)

##


states <- read.csv("states.csv", stringsAsFactors=FALSE)
states <- left_join(states, courtney_state)
states <- left_join(states, delaruo_state)
states <- left_join(states, esty_state)
states <- left_join(states, himes_state)
states <- left_join(states, larson_state)
#write.csv(states, "states_house.csv")

towns <- read.csv("towns.csv", stringsAsFactors=FALSE)
towns <- left_join(towns, courtney_town)
towns <- left_join(towns, delaruo_town)
towns <- left_join(towns, esty_town)
towns <- left_join(towns, himes_town)
towns <- left_join(towns, larson_town)
#write.csv(towns, "towns_house.csv")

#bind all oe

oe_all <- rbind(courtneyoe, delaruooe, estyoe, himesoe, larsonoe)
cleaned <- read.csv("cleanthis.csv", stringsAsFactors=FALSE)
oe_all <- left_join(oe_all, cleaned)

library(ggplot2)
oe_all$Candidate <- gsub("DeLaruo, Rosa", "DeLauro, Rosa", oe_all$Candidate)
p <- ggplot(data=oe_all, aes(Candidate, Amount)) + geom_bar(position="stack", stat="identity") + coord_flip() + facet_wrap(~Cat) + theme_bw()
p +   ggtitle("2015 campaign spending by CT U.S. House members") 

p
  
  ggtitle(expression(atop("", atop(italic("By category as determined by CTMirror.org."), ""))))
  

p <- ggplot(data=oe_all, aes(Candidate, Amount)) + geom_bar(position="stack", stat="identity") + coord_flip() + facet_wrap(~Purpose)
p

cat_cand <- tapply(oe_all$Amount, )
library(tidyr)
cat_cand <- oe_all %>%
  group_by(Candidate, Cat) %>%
  summarise(Total=sum(Amount)) %>%
  spread(Candidate, Total)