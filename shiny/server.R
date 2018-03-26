shinyServer(function(input, output) { 
  # You can access the value of the widget with input$text, e.g.
  # data("CreditCard")
  # bankcard <- subset(CreditCard, select = c(card, reports, age, income, owner,months))
  # bankcard$card <- ifelse(bankcard$card == "yes", 1, 0)
  # card_glm <- glm(formula = card ~ ., family = "binomial", data = bankcard)
  
  # new <- data.frame(reports = input$reports, age = input$age, income = input$income, owner = owner, months = input$months)
  # result <- predict(card_glm, newdata = new, type = "response")
  output$value <- renderPrint({ 
    new <- data.frame(reports = input$reports, age = input$age, income = input$income, owner = input$owner, months = input$months)
    result <- predict(card_glm, newdata = new, type = "response")
    list(reports = input$reports,age = input$age,income = input$income, owner=input$owner,months=input$months,result=result)
    
    })
  })