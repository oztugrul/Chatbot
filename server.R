#review_call_model, datalar, answer fonksiyonu, write fonksiyonu, to.plain fonksiyonu Environment'te tanımlı olmalıdır.

library(shiny)


shinyServer(function(input,output) {
  
  model1pred<-reactive({
   Input<-input$test_review_data
   if(is.empty(Input)==TRUE){print(c("Lütfen Sorunuzu Yazınız."))}
   #svm fonk
   #else if(prob(Input)<0.8){print("Lütfen yaşadığınız sorun hakkında daha detaylı bilgi veriniz.")}else{answer(Input)}
   #bmrm fonk
   else if(prob(Input)<0){answer(Input)}
   else if(prob(Input)<2){print("Lütfen yaşadığınız sorun hakkında daha detaylı bilgi veriniz.")}else{answer(Input)}
    
  })
  

  output$pred1<-renderText({
    model1pred()
  })
  
})

