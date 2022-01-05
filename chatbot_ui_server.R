#----------------------------------------------------------
library(udpipe)
model <- udpipe_download_model(language = "turkish")
model <- udpipe_load_model(file = model$file_model)

#UDPIPE UYGULAMA

setwd("D:/")

data<-read.csv("Chat_tr.csv",header = TRUE)
#data<-read.csv("Chatbot7.csv",header = TRUE)
#data<-read.csv("Chatbot7_highfreq.csv",header = TRUE)
#Chatbot7_highfreq data cevaplar
#data2<-read.csv("cevap.csv",header = TRUE)
#chat_tr data cevaplar
data2<-read.csv("cevap2.csv",header = TRUE)

data$Call<-gsub('[[:punct:] ]+',' ',data$Call)

#turkce karakter donusumu

to.plain <- function(s) {
  
  # 1 character substitutions
  
  old1 <- c("ÇÐÞÝÖÜI")
  
  new1 <- c("çðþiöüý")
  
  s1 <- chartr(old1, new1, s)
  s1
  
}

data$Call<-to.plain(data$Call)

data$Call<-tolower(data$Call)



#random sampling

#---------------------------------------------------------------
## 90% of the sample size

#smp_size <- floor(0.9* nrow(data))
## set the seed to make your partition reproducible
#set.seed(123)

#train_ind <- sample(seq_len(nrow(data)), size = smp_size)
#---------------------------------------------------------------


#Stratified Sampling (codes below)

#----------------------------------------------------------------

library(splitstackshape)

library(prodlim)

set.seed(1234)

call_review_training<-stratified(data,group=c("Call.Solutions"),size=0.9)

train_ind<-row.match(call_review_training,data)

#----------------


call_review_training <- data[train_ind, ]

library(dplyr)

call_review_training<-mutate(call_review_training,id=as.character(1:length(call_review_training$Call)))

call_review_training$Call<-enc2utf8(as.character(call_review_training$Call))



library(udpipe)

library(Hmisc)

model <- udpipe_download_model(language = "turkish")

model <- udpipe_load_model(file = model$file_model)

x <- udpipe_annotate(model, x=call_review_training$Call,doc_id = call_review_training$id)

x <- as.data.frame(x)

x<- subset(x, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))


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

training_set_call$Call.Solution <- as.factor(training_set_call$Call.Solution)

#rownames(training_set_call) <- c()

library(caret)

#trainControl fonksiyonunda classProbs=TRUE yapýlarak sýnýf olasýlýklarý verilebilir.

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, 
                     repeats = 5, 
                     verboseIter = FALSE,
                     sampling = "up")


review_call_model <- train(Call.Solution ~., data = training_set_call, method = 'svmLinear3',trControl = ctrl)

#review_call_model <- train(training_set_call[,1:(ncol(training_set_call)-1)],training_set_call[,ncol(training_set_call)], method = 'svmLinear3',trControl="ctrl")


test_review_data <- data[-train_ind, ]

#test_review_data$Call<-gsub('[[:punct:] ]+',' ',test_review_data$Call)
#test_review_data$Call<-to.plain(test_review_data$Call)
#test_review_data$Call<-tolower(test_review_data$Call)


test_review_data <-mutate(test_review_data,id=as.character(1:length(test_review_data$Call)))

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

model_call_result <- predict(review_call_model, newdata = test_tdm)



model_call_result

#test_review_data$Call.Solution

x_test$Call.Solutions


sum(ifelse(model_call_result== x_test$Call.Solutions, 1, 0))/length(model_call_result)

#sum(ifelse(model_call_result== test_review_data$Call.Solution, 1, 0))/length(model_call_result)


#UDPIPE UYGULAMA SON




#function giving the right answer

write<-function(test_review_data){
  test_review_data<-gsub('[[:punct:] ]+',' ',test_review_data)
  test_review_data<-to.plain(test_review_data)
  test_review_data<-tolower(test_review_data)
  
  test_review_data<-mutate(as.data.frame(test_review_data),id=as.character(1:length(test_review_data)))
  
  colnames(test_review_data)<-c("Call","id")
  
  test_review_data$Call<-enc2utf8(as.character(test_review_data$Call))
  
  
  library(udpipe)
  
  
  x_test<- udpipe_annotate(model, x=test_review_data$Call,doc_id = test_review_data$id)
  
  x_test <- as.data.frame(x_test)
  
  #x_test<- subset(x_test, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))
  
  
  x_test<-x_test%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))
  
  colnames(x_test)[1] <- "id"
  
  x_test<-left_join(x_test,test_review_data,by="id")
  
  x_test<-select(x_test,term,id)
  
  test_tdm_previous<-dfm(as.character(x_test$term),tolower=TRUE)
  
  
  #test_tdm_previous<-dfm(as.character(test_review_data$Call),tolower=TRUE)
  
  test_tdm<-dfm_select(test_tdm_previous,tdm_call)
  
  test_tdm <- as.matrix(test_tdm)
  
  #Build the prediction 
  
  model_call_result <- predict(review_call_model, newdata = test_tdm)
  return(model_call_result)
}


prob<-function(test_review_data){
  test_review_data<-gsub('[[:punct:] ]+',' ',test_review_data)
  test_review_data<-to.plain(test_review_data)
  test_review_data<-tolower(test_review_data)
  
  test_review_data<-mutate(as.data.frame(test_review_data),id=as.character(1:length(test_review_data)))
  
  colnames(test_review_data)<-c("Call","id")
  
  test_review_data$Call<-enc2utf8(as.character(test_review_data$Call))
  
  
  library(udpipe)
  
  
  x_test<- udpipe_annotate(model, x=test_review_data$Call,doc_id = test_review_data$id)
  
  x_test <- as.data.frame(x_test)
  
  #x_test<- subset(x_test, upos %nin% c("PUNCT","NUM","INTJ","PROPN","ADP"))
  
  
  x_test<-x_test%>%group_by(doc_id)%>%summarise(term=paste(token,collapse=" "))
  
  colnames(x_test)[1] <- "id"
  
  x_test<-left_join(x_test,test_review_data,by="id")
  
  x_test<-select(x_test,term,id)
  
  test_tdm_previous<-dfm(as.character(x_test$term),tolower=TRUE)
  
  
  #test_tdm_previous<-dfm(as.character(test_review_data$Call),tolower=TRUE)
  
  test_tdm<-dfm_select(test_tdm_previous,tdm_call)
  
  test_tdm <- as.matrix(test_tdm)
  
  #Build the prediction 
  
  model_call_result <- predict(review_call_model, newdata = test_tdm,probability = TRUE)
  
  return(sort(attr(model_call_result,"probabilities"))[ncol(attr(model_call_result,"probabilities"))]/sort(attr(model_call_result,"probabilities"))[ncol(attr(model_call_result,"probabilities"))-1]-1)
}


#answer<-function(callText){
#  callTextNum <- write(callText)
#  for(i in 1:41){
#    if(callTextNum==data2$Cevap.Kodu[i]){
#      return(toString(data2$Cevap[i]))
#    }}
#} 


library(rapport)
library(rapportools)

answer<-function(callText){
  if(is.empty(callText)==TRUE){
    return(c("Lütfen Sorunuzu Yazýnýz"))}
  else if((is.empty(callText)==FALSE)){
    callTextNum <- write(callText)
    for(i in 1:41){
      if(callTextNum==data2$Cevap.Kodu[i]){
        return(toString(data2$Cevap[i]))
      }}
    }}
    
 


#ui original codes

#library(shiny)

#shinyUI(fluidPage(
# titlePanel("Yardým Menüsü"),
#  sidebarLayout(
#    sidebarPanel(
#      textInput("test_review_data","Lütfen Sorunuzu Yazýnýz.",value = " "),      
#      submitButton("Submit")
      
#    ),
#    mainPanel(
#      h3("Cevap"),
#      textOutput("pred1")
#    )
#  )
#))


#server original codes

#library(shiny)


#shinyServer(function(input,output) {
  
#  model1pred<-reactive({
#    Input<-input$test_review_data
#    answer(Input)
#  })
  
  
#  output$pred1<-renderText({
#    model1pred()
#  })
  
#})

