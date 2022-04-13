#training credits questions
#remove special names and daily conversation words with no distinctive property in differentiation of sprecific questions.
#answer irrelevant questions in a proper way.

#Data Prep
setwd("D:/")
library(dplyr)
library(tm)
library(hunspell)
data_raw<-read.csv("son.csv",header = TRUE,skipNul = TRUE)

#data_raw$Call<-data_raw$detail

data_raw$Call <- paste(data_raw$summary," ",data_raw$detail) 

data_raw2<- transform(data_raw,id=as.numeric(factor(Call.Solutions)))

data_raw3<-data_raw2[,4:5]

colnames(data_raw3)[ncol(data_raw3)] <- "Call.Solutions"

data<-data_raw3

data2<-data_raw2[,c(3,5)]

colnames(data2)<-c("Cevap","Cevap.Kodu")

data2<-unique(data2)

data_test<-data2
colnames(data_test)<-c("Cevap","Call.Solutions")
data_test2<-left_join(data_raw3,data_test,by="Call.Solutions")
data_test2<-data_test2[,c(1,3)]
#----------------------------------------------------------
library(udpipe)
model <- udpipe_download_model(language = "turkish")
model <- udpipe_load_model(file = model$file_model)

#UDPIPE UYGULAMA

data$Call<-gsub('[[:punct:] ]+',' ',data$Call)

data$Call<-gsub(c("'")," ",data$Call)

data$Call<-gsub(c("”")," ",data$Call)

data$Call<-gsub(c("“")," ",data$Call)

data$Call<-gsub(c("‘’")," ",data$Call)

data$Call<-gsub(c("’’")," ",data$Call)

data$Call<-gsub(c("’")," ",data$Call)

data$Call<-gsub(c("‘")," ",data$Call)


#turkce karakter donusumu

to.plain <- function(s) {
  
  # 1 character substitutions
  
  old1 <- c("ÇĞŞÖÜİ")
  
  new1 <- c("çğşöüi")
  
  s1 <- chartr(old1, new1, s)
  s1
  
}

data$Call<-to.plain(data$Call)

data$Call<-tolower(data$Call)

data$Call<-removeNumbers(as.character(data$Call))

Sys.setenv(DICPATH = "D:/")

dict <- dictionary('tr-TR')


wrong<-c(" kulanılamıyor ")

right<-c(" kullanılamıyor ")

look<-data.frame(cbind(wrong,right))

library(textclean)

#data$Call<-mgsub(as.character(data$Call),look[,1],look[,2],trim=TRUE)
#data$Call<-mgsub((data$Call),as.character(look[,1]),as.character(look[,2]),trim=TRUE)

b<-c(" ")
data$Call<-mgsub(paste0(b,as.character(data$Call),b),as.character(look[,1]),as.character(look[,2]),leadspace = TRUE,trailspace = TRUE,fixed=TRUE,order.pattern=TRUE,trim =TRUE)


library(tm)
stopword<-c("rica")
data$Call<-removeWords(as.character(data$Call),stopword)


wrong2<-c(" wrong ")
right2<-c(" right ")

look2<-data.frame(cbind(wrong2,right2))

data$Call<-mgsub(paste0(b,as.character(data$Call),b),as.character(look2[,1]),as.character(look2[,2]),leadspace = TRUE,trailspace = TRUE,fixed=TRUE,order.pattern=TRUE,trim =TRUE)


data$Call<-factor(data$Call)

words_added<-c("gönderilememektedir")
mydict<-dictionary(lang = "tr-TR", affix = NULL, add_words = words_added,cache = TRUE)


hunspell(as.character(data$Call), dict =mydict)


#PART 2




#Stratified Sampling (codes below)

#----------------------------------------------------------------
library(caret)
library(splitstackshape)

library(prodlim)

set.seed(12)

#call_review_training<-stratified(data,group=c("Call.Solutions"),size=0.9)

#train_ind<-row.match(call_review_training,data)

###

#call_review_training<-stratified(data,group=c("Call.Solutions"),size=0.9,keep.rownames=TRUE)
#train_ind<-as.numeric(call_review_training$rn)

train_ind<-createDataPartition(data$Call.Solutions, p=1, list = FALSE)


#----------------


call_review_training <- data[train_ind, ]

class_number<-length(unique(call_review_training$Call.Solutions))

library(dplyr)

#call_review_training<-mutate(call_review_training,id=as.character(1:length(call_review_training$Call)))

call_review_training["id"]<-as.character(c(1:length(call_review_training$Call.Solutions)))

call_review_training$Call<-enc2utf8(as.character(call_review_training$Call))



library(udpipe)

library(Hmisc)

model <- udpipe_download_model(language = "turkish")

model <- udpipe_load_model(file = model$file_model)

x <- udpipe_annotate(model, x=call_review_training$Call,doc_id = call_review_training$id)

x <- as.data.frame(x)

x<- subset(x, upos %nin% c("PUNCT")& token %nin% stopword)


#x<- subset(x, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))


x<-x%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))

colnames(x)[1] <- "id"

x<-left_join(x,call_review_training,by="id")

x<-select(x,term,Call.Solutions)


library(quanteda)

tdm_call<-dfm(as.character(x$term),tolower=TRUE)


#tdm_call<-dfm(as.character(call_review_training$Call),tolower=TRUE,ngrams = c(1,2, 3))

tdm_call<- dfm_trim(tdm_call, min_termfreq = 2)


training_set_call <- as.matrix(tdm_call)

training_set_call<- cbind(training_set_call, x$Call.Solutions)

colnames(training_set_call)[ncol(training_set_call)] <- "Call.Solution"

training_set_call <- as.data.frame(training_set_call)

training_set_call$Call.Solution <- factor(training_set_call$Call.Solution)
#training_set_call$Call.Solution <- factor(make.names(training_set_call$Call.Solution))
#library(UBL)
#training_set_call<-RandOverClassif(Call.Solution~.,dat=training_set_call,C.perc = "balance")


rownames(training_set_call) <- c()

library(caret)
library(e1071)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5,
                     repeats = 5,
                     verboseIter = FALSE,
                     sampling = "up")


review_call_model_train <- train(Call.Solution ~., data = training_set_call, method = 'svmLinear3',trControl = ctrl)

review_call_model_svm <- svm(training_set_call[,1:(ncol(training_set_call)-1)],training_set_call[,ncol(training_set_call)],type="C-classification",cross=5,kernel="linear",cost=0.001,cachesize=100,probability=TRUE)


test_review_data <- data[-train_ind, ]

#test_review_data$Call<-gsub('[[:punct:] ]+',' ',test_review_data$Call)
#test_review_data$Call<-to.plain(test_review_data$Call)
#test_review_data$Call<-tolower(test_review_data$Call)


#test_review_data <-mutate(test_review_data,id=as.character(1:length(test_review_data$Call)))

test_review_data["id"]<-as.character(c(1:length(test_review_data$Call.Solutions)))

test_review_data$Call<-enc2utf8(as.character(test_review_data$Call))



library(udpipe)

model <- udpipe_download_model(language = "turkish")

model <- udpipe_load_model(file = model$file_model)

x_test<- udpipe_annotate(model, x=test_review_data$Call,doc_id = test_review_data$id)

x_test <- as.data.frame(x_test)

#x_test<- subset(x_test, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))


x_test<-x_test%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))

colnames(x_test)[1] <- "id"

x_test<-left_join(x_test,test_review_data,by="id")

x_test<-select(x_test,term,id,Call.Solutions)

test_tdm_previous<-dfm(as.character(x_test$term),tolower=TRUE)


#test_tdm_previous<-dfm(as.character(test_review_data$Call),tolower=TRUE)

test_tdm<-dfm_select(test_tdm_previous,tdm_call)

test_tdm <- as.matrix(test_tdm)

#Build the prediction 

model_call_result_train <- predict(review_call_model_train, newdata = test_tdm)

model_call_result_svm <- predict(review_call_model_svm, newdata = test_tdm,probability = TRUE)


as.numeric(as.character(model_call_result_train))
as.numeric(as.character(model_call_result_svm))

#test_review_data$Call.Solution

x_test$Call.Solutions


mean(ifelse(as.numeric(as.character(model_call_result_train))==x_test$Call.Solutions, 1, 0))

mean(ifelse(as.numeric(as.character(model_call_result_svm))==x_test$Call.Solutions, 1, 0))

#sum(ifelse(model_call_result== test_review_data$Call.Solution, 1, 0))/length(model_call_result)


#UDPIPE 



#FUNCTIONS

library(rapport)
library(rapportools)
library(ngram)

data_prep<-function(test_review_data){
  test_review_data<-gsub('[[:punct:] ]+',' ',test_review_data)
  test_review_data<-gsub(c("'")," ",test_review_data)
  test_review_data<-gsub(c("”")," ",test_review_data)
  test_review_data<-gsub(c("“")," ",test_review_data)
  test_review_data<-gsub(c("‘’")," ",test_review_data)
  test_review_data<-gsub(c("’’")," ",test_review_data)
  test_review_data<-gsub(c("’")," ",test_review_data)
  test_review_data<-gsub(c("‘")," ",test_review_data)
  test_review_data<-to.plain(test_review_data)
  test_review_data<-tolower(test_review_data)
  test_review_data<-removeNumbers(test_review_data)
  test_review_data<-mgsub(paste0(b,as.character(test_review_data),b),as.character(look[,1]),as.character(look[,2]),leadspace = TRUE,trailspace = TRUE,fixed=TRUE,order.pattern=TRUE,trim =TRUE)
  test_review_data<-str_c(unique(unlist(strsplit(test_review_data, " "))),collapse = " ")
  test_review_data<-mutate(as.data.frame(test_review_data),id=as.character(1:length(test_review_data)))
  
  colnames(test_review_data)<-c("Call","id")
  
  test_review_data$Call<-enc2utf8(as.character(test_review_data$Call))
  
  x_test<- udpipe_annotate(model, x=test_review_data$Call,doc_id = test_review_data$id)
  
  x_test <- as.data.frame(x_test)
  
  #x_test<- subset(x_test, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))
  
  x_test<-x_test%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))
  
  colnames(x_test)[1] <- "id"
  
  x_test<-left_join(x_test,test_review_data,by="id")
  
  x_test<-select(x_test,term,id)
  
  test_tdm_previous<-dfm(as.character(x_test$term),tolower=TRUE)
  
  test_tdm<-dfm_select(test_tdm_previous,tdm_call)
  
  test_tdm <- as.matrix(test_tdm)
  return(test_tdm)
}

#tdm_matrix<-data_prep(test_review_data)

pred_train<-function(tdm_matrix){
  test_tdm<-tdm_matrix
  #Build the prediction 
  model_call_result_train <- predict(review_call_model_train, newdata = test_tdm)
  return(model_call_result_train)
}

pred_svm<-function(tdm_matrix){
  test_tdm<-tdm_matrix
  #Build the prediction 
  model_call_result_svm <- predict(review_call_model_svm, newdata = test_tdm, probability = TRUE)
  return(model_call_result_svm)
}  



write_train<-function(tdm_matrix,model_result_train){
  
  test_tdm<-tdm_matrix
  
  #Build the prediction 
  model_call_result_train <-model_result_train
  
  if(sum(test_tdm[1,])<2){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>=2){
    #Build the prediction 
    return(as.numeric(as.character(model_call_result_train)))
  }}

#write_train(data_prep(testt),pred_train(data_prep(testt)))

write_svm<-function(tdm_matrix,model_result_svm){
  
  test_tdm<-tdm_matrix
  
  #Build the prediction 
  
  model_call_result_svm <-model_result_svm
  if(sum(test_tdm[1,])<2){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>=2){
    return(as.numeric(as.character(model_call_result_svm)))
  }}

#write_svm(data_prep(testt),pred_svm(data_prep(testt)))

prob_train<-function(tdm_matrix,model_result_svm,model_result_train){
  
  test_tdm<-tdm_matrix
  
  #Build the prediction 
  
  model_call_result_svm <-model_result_svm
  
  model_call_result_train <-model_result_train
  if(sum(test_tdm[1,])<2){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>=2){
    
    return(attr(model_call_result_svm,"probabilities")[,which(names(attr(model_call_result_svm,"probabilities")[1,])==as.numeric(as.character(model_call_result_train)))])
  }}

#prob_train(data_prep(testt),pred_svm(data_prep(testt)),pred_train(data_prep(testt)))
prob_svm<-function(tdm_matrix,model_result_svm){
  
  test_tdm<-tdm_matrix
  
  #Build the prediction 
  
  model_call_result_svm <-model_result_svm
  if(sum(test_tdm[1,])<2){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>=2){
    
    return(as.numeric(sort(attr(model_call_result_svm,"probabilities"),decreasing = TRUE)[1]))
  }}

#prob_svm(data_prep(testt),pred_svm(data_prep(testt)))
#prob_train(data_prep(testt),pred_svm(data_prep(testt)),pred_train(data_prep(testt)))

answer<-function(tdm_matrix,model_result_train,model_result_svm){
  
  callTextNum_train <- write_train(tdm_matrix,model_result_train)
  callTextNum_svm <-write_svm(tdm_matrix,model_result_svm)
  probability_train<-prob_train(tdm_matrix,model_result_svm,model_result_train)
  probability_svm<-prob_svm(tdm_matrix,model_result_svm)
  callTextNum<-c()
  if(callTextNum_train==c("irrelevant")&callTextNum_svm==c("irrelevant")){
    return(c("irrelevant"))}
  else if((callTextNum_train==callTextNum_svm)&&(probability_svm)>0.25){
    callTextNum=callTextNum_train
    return(toString(data2$Cevap[which(callTextNum==data2$Cevap.Kodu)]))
  }
  else if((callTextNum_train==callTextNum_svm)&&(probability_svm)<=0.25){
    callTextNum=callTextNum_train
    return(paste(c("Bir düþüneyim :) Sorunuzun yanýtý þu olabilir mi:" ),toString(data2$Cevap[which(callTextNum==data2$Cevap.Kodu)]),sep=" "))
  }
  else if(((probability_svm-probability_train)>0.62)&&(1-((1-probability_train)*(1-probability_svm)))>0.27||((1-((1-probability_train)*(1-probability_svm)))>0.27&&(probability_train<0.025))){
    callTextNum=callTextNum_svm
    return(toString(data2$Cevap[which(callTextNum==data2$Cevap.Kodu)]))
  }else if((1-(1-probability_train)*(1-probability_svm))>0.30&&probability_train>=0.025){
    callTextNum=callTextNum_train
    return(toString(data2$Cevap[which(callTextNum==data2$Cevap.Kodu)]))
  }
  else{
    return(c("Sorunuzu biraz daha detaylý yazabilir misiniz?"))
  }
  
}

#answer(data_prep(testt),pred_train(data_prep(testt)),pred_svm(data_prep(testt)))

library(xlsx)

data_test3<-data_test2[-train_ind,]

write.xlsx(data_test3,file="test.xlsx")

#PART 3


#Data Prep
setwd("C:/R")
library(dplyr)
data_raw_m<-read.csv("merhaba.csv",header = TRUE)

data_raw2_m<- transform(data_raw_m,id=as.numeric(factor(Call.Solutions)))

data_raw3_m<-data_raw2_m[,c(1,3)]

colnames(data_raw3_m)[ncol(data_raw3_m)] <- "Call.Solutions"

data_m<-data_raw3_m

data2_m<-data_raw2_m[,c(2,3)]
data2_m<-unique(data2_m)

colnames(data2_m)<-c("Cevap","Cevap.Kodu")



data_test_m<-data2_m
colnames(data_test_m)<-c("Cevap","Call.Solutions")
data_test2_m<-left_join(data_raw3_m,data_test_m,by="Call.Solutions")
data_test2_m<-data_test2_m[,c(1,3)]
#----------------------------------------------------------
library(udpipe)
model <- udpipe_download_model(language = "turkish")
model <- udpipe_load_model(file = model$file_model)

#UDPIPE UYGULAMA

data_m$Call<-gsub('[[:punct:] ]+',' ',data_m$Call)

data_m$Call<-gsub(c("'")," ",data_m$Call)

data_m$Call<-gsub(c("”")," ",data_m$Call)

data_m$Call<-gsub(c("“")," ",data_m$Call)

#turkce karakter donusumu

to.plain <- function(s) {
  
  # 1 character substitutions
  
  old1 <- c("ÇÐÞÝÖÜI")
  
  new1 <- c("çðþiöüý")
  
  s1 <- chartr(old1, new1, s)
  s1
  
}

data_m$Call<-to.plain(data_m$Call)

data_m$Call<-tolower(data_m$Call)

library(tm)

#datam$Call<-removeWords(as.character(datam$Call),stopword)

data_m$Call<-removeNumbers(as.character(data_m$Call))

library(textclean)

wrong_m<-c(" doðumgünü ")

right_m<-c(" doðum günü ")


look_m<-data.frame(cbind(wrong_m,right_m))

b<-c(" ")
data_m$Call<-mgsub(paste0(b,as.character(data_m$Call),b),as.character(look_m[,1]),as.character(look_m[,2]),leadspace = TRUE,trailspace = TRUE,fixed=TRUE,order.pattern=TRUE,trim =TRUE)



stopword_m<-c("yapabilirim")

data_m$Call<-removeWords(as.character(data_m$Call),stopword_m)

data_m$Call<-factor(data_m$Call)



#random sampling

#---------------------------------------------------------------

#---------------------------------------------------------------


#Stratified Sampling (codes below)

#----------------------------------------------------------------
library(caret)
library(splitstackshape)

library(prodlim)

set.seed(12)

#call_review_training<-stratified(datam,group=c("Call.Solutions"),size=0.9)

#train_ind<-row.match(call_review_training,datam)

###

#call_review_training<-stratified(datam,group=c("Call.Solutions"),size=0.9,keep.rownames=TRUE)
#train_ind<-as.numeric(call_review_training$rn)

train_ind_m<-createDataPartition(data_m$Call.Solutions, p=1, list = FALSE)


#----------------


call_review_training_m <- data_m[train_ind_m, ]

class_number_m<-length(unique(call_review_training_m$Call.Solutions))

library(dplyr)

#call_review_training<-mutate(call_review_training,id=as.character(1:length(call_review_training$Call)))

call_review_training_m["id"]<-as.character(c(1:length(call_review_training_m$Call.Solutions)))

call_review_training_m$Call<-enc2utf8(as.character(call_review_training_m$Call))



library(udpipe)

library(Hmisc)

model <- udpipe_download_model(language = "turkish")

model <- udpipe_load_model(file = model$file_model)

x_m <- udpipe_annotate(model, x=call_review_training_m$Call,doc_id = call_review_training_m$id)

x_m <- as.data.frame(x_m)

x_m<- subset(x_m, upos %nin% c("PUNCT")& token %nin% stopword_m )


x_m<-x_m%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))

colnames(x_m)[1] <- "id"

x_m<-left_join(x_m,call_review_training_m,by="id")

x_m<-select(x_m,term,Call.Solutions)


library(quanteda)

#tdm_call_m<-dfm(as.character(x_m$term),tolower=TRUE)
tdm_call_m<-dfm(as.character(x_m$term),tolower=TRUE,ngrams=c(1,2,3))


#tdm_call_m<-dfm(as.character(call_review_training$Call),tolower=TRUE,ngrams = c(1,2, 3))

tdm_call_m<- dfm_trim(tdm_call_m, min_termfreq = 1)


training_set_call_m <- as.matrix(tdm_call_m)

training_set_call_m<- cbind(training_set_call_m, x_m$Call.Solutions)

colnames(training_set_call_m)[ncol(training_set_call_m)] <- "Call.Solution"

training_set_call_m <- as.data.frame(training_set_call_m)

training_set_call_m$Call.Solution <- factor(training_set_call_m$Call.Solution)
#training_set_call_m$Call.Solution <- factor(make.names(training_set_call_m$Call.Solution))
#library(UBL)
#training_set_call_m<-RandOverClassif(Call.Solution~.,dat=training_set_call_m,C.perc = "balance")


rownames(training_set_call_m) <- c()

library(caret)
library(e1071)

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5,
                     repeats = 5,
                     verboseIter = FALSE,
                     sampling = "up")


review_call_model_train_m <- train(Call.Solution ~., data = training_set_call_m, method = 'svmLinear3',trControl = ctrl)

review_call_model_svm_m <- svm(training_set_call_m[,1:(ncol(training_set_call_m)-1)],training_set_call_m[,ncol(training_set_call_m)],type="C-classification",cross=5,kernel="linear",cost=0.5,cachesize=100,probability=TRUE)


test_review_data_m <- data_m[-train_ind_m, ]

#test_review_data_m$Call<-gsub('[[:punct:] ]+',' ',test_review_data_m$Call)
#test_review_data_m$Call<-to.plain(test_review_data_m$Call)
#test_review_data_m$Call<-tolower(test_review_data_m$Call)


#test_review_data_m <-mutate(test_review_data_m,id=as.character(1:length(test_review_data_m$Call)))

test_review_data_m["id"]<-as.character(c(1:length(test_review_data_m$Call.Solutions)))

test_review_data_m$Call<-enc2utf8(as.character(test_review_data_m$Call))



library(udpipe)

model <- udpipe_download_model(language = "turkish")

model <- udpipe_load_model(file = model$file_model)

x_test_m<- udpipe_annotate(model, x=test_review_data_m$Call,doc_id = test_review_data_m$id)

x_test_m <- as.data.frame(x_test_m)

#x_test_m<- subset(x_test_m, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))


x_test_m<-x_test_m%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))

colnames(x_test_m)[1] <- "id"

x_test_m<-left_join(x_test_m,test_review_data_m,by="id")

x_test_m<-select(x_test_m,term,id,Call.Solutions)

#test_tdm_previous_m<-dfm(as.character(x_test_m$term),tolower=TRUE)

test_tdm_previous_m<-dfm(as.character(x_test_m$term),tolower=TRUE,ngrams=c(1,2,3))

#test_tdm_previous_m<-dfm(as.character(test_review_data_m$Call),tolower=TRUE)

test_tdm_m<-dfm_select(test_tdm_previous_m,tdm_call_m)

test_tdm_m <- as.matrix(test_tdm_m)

#Build the prediction 

model_call_result_train_m <- predict(review_call_model_train_m, newdata = test_tdm_m)

model_call_result_svm_m <- predict(review_call_model_svm_m, newdata = test_tdm_m,probability = TRUE)


as.numeric(as.character(model_call_result_train_m))
as.numeric(as.character(model_call_result_svm_m))

#test_review_data_m$Call.Solution

x_test_m$Call.Solutions


mean(ifelse(as.numeric(as.character(model_call_result_train_m))==x_test_m$Call.Solutions, 1, 0))

mean(ifelse(as.numeric(as.character(model_call_result_svm_m))==x_test_m$Call.Solutions, 1, 0))


#UDPIPE UYGULAMA SON



#FUNCTIONS

library(rapport)
library(rapportools)
library(ngram)
library(tidyverse)

data_prep_m<-function(test_review_data){
  
  test_review_data<-gsub('[[:punct:] ]+',' ',test_review_data)
  test_review_data<-gsub(c("'")," ",test_review_data)
  test_review_data<-gsub(c("”")," ",test_review_data)
  test_review_data<-gsub(c("“")," ",test_review_data)
  test_review_data<-gsub(c("‘’")," ",test_review_data)
  test_review_data<-gsub(c("’’")," ",test_review_data)
  test_review_data<-gsub(c("’")," ",test_review_data)
  test_review_data<-gsub(c("‘")," ",test_review_data)
  test_review_data<-to.plain(test_review_data)
  test_review_data<-tolower(test_review_data)
  test_review_data<-removeNumbers(test_review_data)
  test_review_data<-mgsub(paste0(b,as.character(test_review_data),b),as.character(look_m[,1]),as.character(look_m[,2]),leadspace = TRUE,trailspace = TRUE,fixed=TRUE,order.pattern=TRUE,trim =TRUE)
  test_review_data<-str_c(unique(unlist(strsplit(test_review_data, " "))),collapse = " ")
  test_review_data<-mutate(as.data.frame(test_review_data),id=as.character(1:length(test_review_data)))
  
  colnames(test_review_data)<-c("Call","id")
  
  test_review_data$Call<-enc2utf8(as.character(test_review_data$Call))
  
  x_test<- udpipe_annotate(model, x=test_review_data$Call,doc_id = test_review_data$id)
  
  x_test <- as.data.frame(x_test)
  
  x_test<-x_test%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))
  
  colnames(x_test)[1] <- "id"
  
  x_test<-left_join(x_test,test_review_data,by="id")
  
  x_test<-select(x_test,term,id)
  
  test_tdm_previous<-dfm(as.character(x_test$term),tolower=TRUE,ngrams=c(1,2,3))
  
  test_tdm<-dfm_select(test_tdm_previous,tdm_call_m)
  
  test_tdm <- as.matrix(test_tdm)
  return(test_tdm)
}

#tdm_matrix_m<-data_prep_m(test_review_data)

pred_train_m<-function(tdm_matrix_m){
  test_tdm<-tdm_matrix_m
  #Build the prediction 
  model_call_result_train <- predict(review_call_model_train_m, newdata = test_tdm)
  return(model_call_result_train)
}

pred_svm_m<-function(tdm_matrix_m){
  test_tdm<-tdm_matrix_m
  #Build the prediction 
  model_call_result_svm <- predict(review_call_model_svm_m, newdata = test_tdm, probability = TRUE)
  return(model_call_result_svm)
}  


write_train_m<-function(tdm_matrix_m,model_result_train_m){
  
  test_tdm<-tdm_matrix_m
  model_call_result_train <-model_result_train_m
  
  if(sum(test_tdm[1,])==0){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>0){
    #Build the prediction 
    return(as.numeric(as.character(model_call_result_train)))
  }}



write_svm_m<-function(tdm_matrix_m,model_result_svm_m){
  
  test_tdm<-tdm_matrix_m
  
  model_call_result_svm <-model_result_svm_m
  if(sum(test_tdm[1,])==0){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>0){
    return(as.numeric(as.character(model_call_result_svm)))
  }}



prob_train_m<-function(tdm_matrix_m,model_result_svm_m,model_result_train_m){
  test_tdm<-tdm_matrix_m
  
  model_call_result_svm <-model_result_svm_m
  
  model_call_result_train <-model_result_train_m
  if(sum(test_tdm[1,])==0){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>0){
    
    return(attr(model_call_result_svm,"probabilities")[,which(names(attr(model_call_result_svm,"probabilities")[1,])==as.numeric(as.character(model_call_result_train)))])
  }}



prob_svm_m<-function(tdm_matrix_m,model_result_svm_m){
  test_tdm<-tdm_matrix_m
  
  model_call_result_svm <-model_result_svm_m
  if(sum(test_tdm[1,])==0){
    return(c("irrelevant"))}
  else if(sum(test_tdm[1,])>0){
    
    return(as.numeric(sort(attr(model_call_result_svm,"probabilities"),decreasing = TRUE)[1]))
  }}



answer_m<-function(tdm_matrix_m,model_result_train_m,model_result_svm_m){
  
  callTextNum_train <- write_train_m(tdm_matrix_m,model_result_train_m)
  callTextNum_svm <-write_svm_m(tdm_matrix_m,model_result_svm_m)
  probability_train<-prob_train_m(tdm_matrix_m,model_result_svm_m,model_result_train_m)
  probability_svm<-prob_svm_m(tdm_matrix_m,model_result_svm_m)
  callTextNum<-c()
  if(callTextNum_train==c("irrelevant")&callTextNum_svm==c("irrelevant")){
    return(c("irrelevant"))}
  else if((callTextNum_train==callTextNum_svm)&&(1-(1-probability_train)*(1-probability_svm))>0.25){
    callTextNum=callTextNum_train
    return(toString(data2_m$Cevap[which(callTextNum==data2_m$Cevap.Kodu)]))
  }
  else if((callTextNum_train==callTextNum_svm)&&(1-(1-probability_train)*(1-probability_svm))<=0.25){
    callTextNum=callTextNum_train
    return(paste(c("Bir düþüneyim :) Sorunuzun yanýtý þu olabilir mi:" ),toString(data2_m$Cevap[which(callTextNum==data2_m$Cevap.Kodu)]),sep=" "))
  }
  else if(((probability_svm-probability_train)>0.30)&&(1-((1-probability_train)*(1-probability_svm)))>0.25){
    callTextNum=callTextNum_svm
    return(toString(data2_m$Cevap[which(callTextNum==data2_m$Cevap.Kodu)]))
  }else if((1-(1-probability_train)*(1-probability_svm))>0.25){
    callTextNum=callTextNum_train
    return(toString(data2_m$Cevap[which(callTextNum==data2_m$Cevap.Kodu)]))
  }
  
  else{
    return(c("Sorunuzu biraz daha detaylý yazabilir misiniz?"))
  }
  
}




answer_choose<-function(callText){
  callText<-removeNumbers(callText)
  callText<-gsub('[[:punct:] ]+',' ',callText)
  callText<-to.plain(callText)
  callText<-tolower(callText)
  if(is.empty(callText)==TRUE){
    return(c("Lütfen Sorunuzu Yazýnýz."))}
  else {
    tdm_matrix_m<-data_prep_m(callText)
    tdm_matrix<-data_prep(callText)
    predict_train_m<-pred_train_m(tdm_matrix_m)
    predict_train<-pred_train(tdm_matrix)
    predict_svm_m<-pred_svm_m(tdm_matrix_m)
    predict_svm<-pred_svm(tdm_matrix)
    probability_svm_m<-prob_svm_m(tdm_matrix_m,predict_svm_m)
    probability_svm<-prob_svm(tdm_matrix,predict_svm)
    answerk<-answer(tdm_matrix,predict_train,predict_svm)
    answerm<-answer_m(tdm_matrix_m,predict_train_m,predict_svm_m)
    random_answer<-c(rep(c("Ne desem yalan olur :)"),10),rep(c("Üzgünüm, her konuda cevap veremiyorum"),10),rep(c("Maalesef bu konuda yardýmcý olamýyorum"),10),rep(c("Þimdilik sadece belirli konularda yardýmcý olabiliyorum."),10),rep(c("Bu konuda eðitilmedim :)"),10),rep(c("Bu konuda bir fikrim yok ama öðreniyorum :)"),10))
    if(answerk==c("irrelevant")&answerm==c("irrelevant")){return(sample(random_answer,1))}
    else if(answerm!=c("irrelevant")&answerk==c("irrelevant")){return(answerm)}
    else if(answerm==c("irrelevant")&answerk!=c("irrelevant")){return(answerk)}
    else if((probability_svm-probability_svm_m)>=0.02){return(answerk)}
    else if ((probability_svm-probability_svm_m)<0.02 & (probability_svm-probability_svm_m)>0){return(answerm)}
    else if((probability_svm_m-probability_svm)>=0){return(answerm)}
    else{return(answerk)}}}


#prob_train(data_prep(testt),pred_svm(data_prep(testt)),pred_train(data_prep(testt)))
#prob_svm(data_prep(testt),pred_svm(data_prep(testt)))


