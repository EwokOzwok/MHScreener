#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import shinyalert
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  ScreenerData<-data.frame(matrix(NA, nrow=1,ncol = 16))
  colnames(ScreenerData)<-c("PHQ1","PHQ2","PHQ3","PHQ4","PHQ5","PHQ6","PHQ7","PHQ8","PHQ9","GAD1","GAD2","GAD3","GAD4","GAD5","GAD6","GAD7")


  observeEvent(input$Start, {
    updateF7Tabs(session = session, id = "tabs", selected = "GettingStartedTab")
  })

  observeEvent(input$Screenerprompt, {
    updateF7Tabs(session = session, id = "tabs", selected = "PHQ9tab")
  })


  observeEvent(input$StartOver, {
    updateF7Tabs(session = session, id = "tabs", selected = "WelcomeTab")
    updateF7Select(inputId="PHQitem1", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem2", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem3", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem4", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem5", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem6", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem7", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem8", selected = c("Choose a response"))
    updateF7Select(inputId="PHQitem9", selected = c("Choose a response"))
    output$datasummaryIntro<- renderUI({})


  })







  observeEvent(input$PHQ9Submit, {

    if(input$PHQitem1=="Choose a response" ||
       input$PHQitem2=="Choose a response" ||
       input$PHQitem3=="Choose a response" ||
       input$PHQitem4=="Choose a response" ||
       input$PHQitem5=="Choose a response" ||
       input$PHQitem6=="Choose a response" ||
       input$PHQitem7=="Choose a response" ||
       input$PHQitem8=="Choose a response" ||
       input$PHQitem9=="Choose a response")
    {
      shinyalert::shinyalert(title="Oops, you forgot to answer a question!",type="error")
    } else {
      rm(ScreenerData)
      ScreenerData<-data.frame(matrix(NA, nrow=1,ncol = 16))
      colnames(ScreenerData)<-c("PHQ1","PHQ2","PHQ3","PHQ4","PHQ5","PHQ6","PHQ7","PHQ8","PHQ9","GAD1","GAD2","GAD3","GAD4","GAD5","GAD6","GAD7")

      ScreenerData$PHQ1<-input$PHQitem1
      ScreenerData$PHQ2<-input$PHQitem2
      ScreenerData$PHQ3<-input$PHQitem3
      ScreenerData$PHQ4<-input$PHQitem4
      ScreenerData$PHQ5<-input$PHQitem5
      ScreenerData$PHQ6<-input$PHQitem6
      ScreenerData$PHQ7<-input$PHQitem7
      ScreenerData$PHQ8<-input$PHQitem8
      ScreenerData$PHQ9<-input$PHQitem9
      ScreenerData[,1:9] <- factor(unlist(ScreenerData), levels = c("Not at all","A little bit","Moderately","Quite a bit","Extremely"), labels = c(1, 2, 3,4,5))
      ScreenerData<-lapply(ScreenerData,as.numeric)
      ScreenerData<-as.data.frame(ScreenerData)
      ScreenerData$Total_PHQ9<-ScreenerData$PHQ1+ScreenerData$PHQ2+ScreenerData$PHQ3+ScreenerData$PHQ4+ScreenerData$PHQ5+ScreenerData$PHQ6+ScreenerData$PHQ7+ScreenerData$PHQ8+ScreenerData$PHQ9
      print(ScreenerData)
      updateF7Tabs(session = session, id = "tabs", selected = "GAD7tab")

    }

  })




  observeEvent(input$GAD7Submit, {

    if(input$GADitem1=="Choose a response" ||
       input$GADitem2=="Choose a response" ||
       input$GADitem3=="Choose a response" ||
       input$GADitem4=="Choose a response" ||
       input$GADitem5=="Choose a response" ||
       input$GADitem6=="Choose a response" ||
       input$GADitem7=="Choose a response")
    {
      shinyalert::shinyalert(title="Oops, you forgot to answer a question!",type="error")
    } else {
      rm(ScreenerData)
      ScreenerData<-data.frame(matrix(NA, nrow=1,ncol = 16))
      colnames(ScreenerData)<-c("PHQ1","PHQ2","PHQ3","PHQ4","PHQ5","PHQ6","PHQ7","PHQ8","PHQ9","GAD1","GAD2","GAD3","GAD4","GAD5","GAD6","GAD7")

      ScreenerData$GAD1<-input$GADitem1
      ScreenerData$GAD2<-input$GADitem2
      ScreenerData$GAD3<-input$GADitem3
      ScreenerData$GAD4<-input$GADitem4
      ScreenerData$GAD5<-input$GADitem5
      ScreenerData$GAD6<-input$GADitem6
      ScreenerData$GAD7<-input$GADitem7
      ScreenerData[,10:16] <- factor(unlist(ScreenerData), levels = c("Not at all","A little bit","Moderately","Quite a bit","Extremely"), labels = c(1, 2, 3, 4, 5))
      ScreenerData<-lapply(ScreenerData,as.numeric)
      ScreenerData<-as.data.frame(ScreenerData)
      ScreenerData$Total_GAD7<-ScreenerData$GAD1+ScreenerData$GAD2+ScreenerData$GAD3+ScreenerData$GAD4+ScreenerData$GAD5+ScreenerData$GAD6+ScreenerData$GAD7
      print(ScreenerData)
      updateF7Tabs(session = session, id = "tabs", selected = "DoneTab")

    }

  })


observeEvent(input$Login,{
  if(input$NavPassword == "access"){
  updateF7Tabs(session = session, id = "tabs", selected = "Output_tab")
  } else {
    shinyalert::shinyalert(title="Wrong Password!",type="error")
  }
})



      output$datasummaryIntro<- renderUI({
        tagList(
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Card(
              title = "",
              f7Align(h2("Your Mental Health Screener has been successfully completed"), side=c("center")),
              f7Align(h3("Please hand the tablet back to the Navigator"), side=c("center")),
              hairlines = F, strong = T, inset =
                F, tablet = FALSE)
          ),
          f7Card(
            f7Align(h3("Total PHQ9 score"), side=c("center")),
            f7Align(h3(c(ScreenerData$Total_PHQ9)), side=c("center")),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })


      print(ScreenerData$Total_PHQ9)




  output$StartButton<-renderUI({
    tagList(
      f7Button("Start", "Get Started!")
    )
  })

  output$PHQ9<-renderUI({
    tagList(
      f7Card(
        h4("PHQ Item 1"),
        f7Select("PHQitem1", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 2"),
        f7Select("PHQitem2", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 3"),
        f7Select("PHQitem3", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 4"),
        f7Select("PHQitem4", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 5"),
        f7Select("PHQitem5", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 6"),
        f7Select("PHQitem6", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 7"),
        f7Select("PHQitem7", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 8"),
        f7Select("PHQitem8", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("PHQ Item 9"),
        f7Select("PHQitem9", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Button("PHQ9Submit", "Continue")
    )
  })



  output$PHQ9<-renderUI({
    tagList(
      f7Card(
        h4("GAD Item 1"),
        f7Select("GADitem1", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 2"),
        f7Select("GADitem2", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 3"),
        f7Select("GADitem3", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 4"),
        f7Select("GADitem4", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 5"),
        f7Select("GADitem5", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 6"),
        f7Select("GADitem6", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Card(
        h4("GAD Item 7"),
        f7Select("GADitem7", NULL , choices = c("Choose a response", "Not at all","A little bit","Moderately","Quite a bit","Extremely"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),


      f7Button("GAD7Submit", "Finish")
    )
  })


}
