#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import dplyr
#' @import shinyalert
#' @import shinyjs
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic
  PHQ9Data<-data.frame(matrix(NA, nrow=1,ncol = 9))
  colnames(PHQ9Data)<-c("PHQ1","PHQ2","PHQ3","PHQ4","PHQ5","PHQ6","PHQ7","PHQ8","PHQ9")
  GAD7data<-data.frame(matrix(NA, nrow=1,ncol = 7))
  colnames(GAD7data)<-c("GAD1","GAD2","GAD3","GAD4","GAD5","GAD6","GAD7")


  observeEvent(input$Screenerprompt, {
    output$PHQ9Questions<-renderUI({
      tagList(
        f7Block(
          f7Shadow(
            intensity = 5,
            hover = TRUE,
            f7Card(
              f7Align(h2("Part 1 of 2"), side=c("center")),
              h3("INSTRUCTIONS:"),
              h4("Over the last 2 weeks, how often have you been bothered by any of the following problems?"),
              uiOutput("PHQ9"),
              footer = NULL,
              hairlines = F, strong = T, inset = F, tablet = FALSE)
          )
        )
      )
    })
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

    updateF7Tabs(session = session, id = "tabs", selected = "WelcomeTab")
    updateF7Select(inputId="GADitem1", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem2", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem3", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem4", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem5", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem6", selected = c("Choose a response"))
    updateF7Select(inputId="GADitem7", selected = c("Choose a response"))
    output$PHQ9summary<- renderUI({})
    output$GAD7summary<- renderUI({})
    updateF7Text("NavUsername", label = "Navigator ID: ", value="Enter Navigator ID number")
    output$PHQ9Questions<-renderUI({})
    output$GAD7Questions<- renderUI({})
    output$IDPASSWORD<- renderUI({})
  })







  observeEvent(input$PHQ9Submit, {
    output$GAD7Questions<- renderUI({
      tagList(
      f7Block(
        f7Shadow(
          intensity = 5,
          hover = TRUE,
          f7Card(
            f7Align(h2("Part 2 of 2"), side=c("center")),
            h3("INSTRUCTIONS:"),
            h4("Over the last two weeks, how often have you been bothered by the following problems?"),
            uiOutput("GAD7"),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)
          )
        )
      )
    })

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
      PHQ9Data<-data.frame(matrix(NA, nrow=1,ncol = 9))
      colnames(PHQ9Data)<-c("PHQ1","PHQ2","PHQ3","PHQ4","PHQ5","PHQ6","PHQ7","PHQ8","PHQ9")

      PHQ9Data$PHQ1<-input$PHQitem1
      PHQ9Data$PHQ2<-input$PHQitem2
      PHQ9Data$PHQ3<-input$PHQitem3
      PHQ9Data$PHQ4<-input$PHQitem4
      PHQ9Data$PHQ5<-input$PHQitem5
      PHQ9Data$PHQ6<-input$PHQitem6
      PHQ9Data$PHQ7<-input$PHQitem7
      PHQ9Data$PHQ8<-input$PHQitem8
      PHQ9Data$PHQ9<-input$PHQitem9
      PHQ9Data[] <- factor(unlist(PHQ9Data), levels = c("Not at all","Several Days","More than half the days","Nearly every day"), labels = c(0,1,2,3))
      PHQ9Data<-lapply(PHQ9Data,as.numeric)
      PHQ9Data<-as.data.frame(PHQ9Data)
      PHQ9Data$Total_PHQ9<-PHQ9Data$PHQ1+PHQ9Data$PHQ2+PHQ9Data$PHQ3+PHQ9Data$PHQ4+PHQ9Data$PHQ5+PHQ9Data$PHQ6+PHQ9Data$PHQ7+PHQ9Data$PHQ8+PHQ9Data$PHQ9
      print(PHQ9Data)
      updateF7Tabs(session = session, id = "tabs", selected = "GAD7tab")

      if(PHQ9Data$PHQ9==0){
        SuicideAlert<-c("No suicidal ideation in the past 2 weeks")
      }
      if(PHQ9Data$PHQ9==1){
        SuicideAlert<-c("Suicidal ideation for SEVERAL DAYS in the past 2 weeks!")
      }
      if(PHQ9Data$PHQ9==2){
        SuicideAlert<-c("Suicidal ideation for MORE THAN HALF OF THE DAYS in the past 2 weeks!")
      }
      if(PHQ9Data$PHQ9==3){
        SuicideAlert<-c("Suicidal ideation for NEARLY EVERY DAY in the past 2 weeks!")
      }

      if(PHQ9Data$Total_PHQ9 < 5){
        Severity<-c("Minimal Depression")
      }
      if(PHQ9Data$Total_PHQ9 > 4 && PHQ9Data$Total_PHQ9 < 10){
        Severity<-c("Mild Depression")
      }
      if(PHQ9Data$Total_PHQ9 > 9 && PHQ9Data$Total_PHQ9 < 15){
        Severity<-c("Moderate Depression")
      }
      if(PHQ9Data$Total_PHQ9 > 14 && PHQ9Data$Total_PHQ9 < 20){
        Severity<-c("Moderately Severe Depression")
      }
      if(PHQ9Data$Total_PHQ9 > 19){
        Severity<-c("Severe Depression")
      }

      if (PHQ9Data$PHQ1>1){
        Dflag1<-c("Little interest or pleasure in doing things")
      } else{Dflag1<-NULL}
      if (PHQ9Data$PHQ2>1){
        Dflag2<-c("Feeling down, depressed, or hopeless")
      } else{Dflag2<-NULL}
      if (PHQ9Data$PHQ3>1){
        Dflag3<-c("Trouble falling or staying asleep, or sleeping too much")
      } else{Dflag3<-NULL}
      if (PHQ9Data$PHQ4>1){
        Dflag4<-c("Feeling tired or having little energy")
      } else{Dflag4<-NULL}
      if (PHQ9Data$PHQ5>1){
        Dflag5<-c("Poor appetite or overeating")
      } else{Dflag5<-NULL}
      if (PHQ9Data$PHQ6>1){
        Dflag6<-c("Feeling bad about yourself or that you are a failure or have let yourself or your family down")
      } else{Dflag6<-NULL}
      if (PHQ9Data$PHQ7>1){
        Dflag7<-c("Trouble concentrating on things, such as reading the newspaper or watching television")
      } else{Dflag7<-NULL}
      if (PHQ9Data$PHQ8>1){
        Dflag8<-c("Moving or speaking so slowly that other people could have noticed. Or the opposite    being so figety or restless that you have been moving around a lot more than usual")
      } else{Dflag8<-NULL}

      if(is.null(Dflag1) == T && is.null(Dflag2) == T && is.null(Dflag3) == T && is.null(Dflag4) == T && is.null(Dflag5) == T && is.null(Dflag6) == T && is.null(Dflag7) == T && is.null(Dflag8) == T){
        DepressionFlagText<-c("No depression items were flagged")
      } else {DepressionFlagText<-c("Flagged Depression Items")}


      output$PHQ9summary<- renderUI({
        tagList(
          f7Card(title = f7Align(h2("Depression", side=c("center"))),
            f7Align(h3(paste("Severity: ", Severity)), side=c("left")),
            hr(),
            f7Align(h3(paste("Suicidality: ", SuicideAlert)), side=c("left")),
            hr(),
            f7Align(h3(paste("Total PHQ9 score: ", c(PHQ9Data$Total_PHQ9))), side=c("left")),
            hr(),
            f7Align(h3(DepressionFlagText), side=c("left")),
            h4(Dflag1),
            h4(Dflag2),
            h4(Dflag3),
            h4(Dflag4),
            h4(Dflag5),
            h4(Dflag6),
            h4(Dflag7),
            h4(Dflag8),
            hr(),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })
    }

  })




  observeEvent(input$GAD7Submit, {

    output$IDPASSWORD<- renderUI({
      tagList(
        f7AccordionItem(
          title="Enter Navigator ID and Password",
          f7Card(
            br(),
            f7Text("NavUsername", label = "Navigator ID: ", value=NULL, placeholder = "Enter Navigator ID number"),
            br(),
            br(),
            br(),
            f7Password("NavPassword", label = "Password : ", value = NULL, placeholder = "Enter Password"),
            br(),
            br(),
            br(),
            f7Button("Login", "Login", rounded = T, shadow=T, fill = T),
            hairlines = F, strong = T, inset =
              F, tablet = FALSE))

      )
    })



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
      GAD7data$GAD1<-input$GADitem1
      GAD7data$GAD2<-input$GADitem2
      GAD7data$GAD3<-input$GADitem3
      GAD7data$GAD4<-input$GADitem4
      GAD7data$GAD5<-input$GADitem5
      GAD7data$GAD6<-input$GADitem6
      GAD7data$GAD7<-input$GADitem7
      GAD7data[] <- factor(unlist(GAD7data), levels = c("Not at all","Several Days","More than half the days","Nearly every day"), labels = c(0, 1, 2, 3))
      GAD7data<-lapply(GAD7data,as.numeric)
      GAD7data<-as.data.frame(GAD7data)
      GAD7data$Total_GAD7<-GAD7data$GAD1+GAD7data$GAD2+GAD7data$GAD3+GAD7data$GAD4+GAD7data$GAD5+GAD7data$GAD6+GAD7data$GAD7
      print(GAD7data)
      updateF7Tabs(session = session, id = "tabs", selected = "DoneTab")

      if(GAD7data$Total_GAD7 < 5){
        ANXSeverity<-c("Minimal Anxiety")
      }
      if(GAD7data$Total_GAD7 > 4 && GAD7data$Total_GAD7 < 10){
        ANXSeverity<-c("Mild Anxiety")
      }
      if(GAD7data$Total_GAD7 > 9 && GAD7data$Total_GAD7 < 15){
        ANXSeverity<-c("Moderate Anxiety")
      }
      if(GAD7data$Total_GAD7 > 14){
        ANXSeverity<-c("Severe Anxiety")
      }

      if (GAD7data$GAD1 >1){
        Aflag1<-c("Feeling nervous, anxious, or on edge")
      } else {Aflag1<-NULL}
      if (GAD7data$GAD2 >1){
        Aflag2<-c("Not being able to stop or control worrying")
      } else {Aflag2<-NULL}
      if (GAD7data$GAD3 >1){
        Aflag3<-c("Worrying too much about different things")
      } else {Aflag3<-NULL}
      if (GAD7data$GAD4 >1){
        Aflag4<-c("Trouble relaxing")
      } else {Aflag4<-NULL}
      if (GAD7data$GAD5 >1){
        Aflag5<-c("Being so restless that it is hard to sit still")
      } else {Aflag5<-NULL}
      if (GAD7data$GAD6 >1){
        Aflag6<-c("Becoming easily annoyed or irritable")
      } else {Aflag6<-NULL}
      if (GAD7data$GAD7 >1){
        Aflag7<-c("Feeling afraid, as if something awful might happen")
      } else {Aflag7<-NULL}

      if (is.null(Aflag1) == T && is.null(Aflag2) == T && is.null(Aflag3) == T && is.null(Aflag4) == T && is.null(Aflag5) == T && is.null(Aflag6) == T && is.null(Aflag7) == T){
        AnxietyFlagText<-c("No anxiety items were flagged")
      } else {AnxietyFlagText<-c("Flagged Anxiety Items")}

      output$GAD7summary<- renderUI({
        tagList(
            f7Card(title = f7Align(h2("Anxiety", side=c("center"))),
            f7Align(h3(paste("Severity: ", ANXSeverity)), side=c("left")),
            hr(),
            f7Align(h3(paste("Total GAD7 score: ",c(GAD7data$Total_GAD7))), side=c("left")),
            hr(),
            f7Align(h3(AnxietyFlagText), side=c("left")),
            h4(Aflag1),
            h4(Aflag2),
            h4(Aflag3),
            h4(Aflag4),
            h4(Aflag5),
            h4(Aflag6),
            h4(Aflag7),
            hr(),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })





    }

  })



output$DONE<- renderUI({
  tagList(
    f7Shadow(
      intensity = 5,
      hover = TRUE,
      f7Card(
        title = "",
        f7Align(h1("Thank you for completing the Project ACCESS Mental Health Screener"), side=c("center")),
        f7Align(h2("Please hand the tablet back to the Navigator"), side=c("center")),
        hairlines = F, strong = T, inset =
          F, tablet = FALSE))

  )
})


observeEvent(input$Login,{
  if(input$NavPassword == "access"){
    updateF7Tabs(session = session, id = "tabs", selected = "Output_tab")
  } else {
    shinyalert::shinyalert(title="Wrong Password!",type="error")
  }
})



      output$ScreenerSummary<- renderUI({
        tagList(
            f7Card(
            f7Align(h3("Total PHQ9 score"), side=c("center")),
            f7Align(h3(c(PHQ9Data$Total_PHQ9)), side=c("center")),

            f7Align(h3("Total GAD7 score"), side=c("center")),
            f7Align(h3(c(GAD7data$Total_GAD7)), side=c("center")),
            footer = NULL,
            hairlines = F, strong = T, inset = F, tablet = FALSE)
        )
      })





  output$PHQ9<-renderUI({
    tagList(
      f7Card(
        h3("Little interest or pleasure in doing things"),
        f7Select("PHQitem1", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Feeling down, depressed, or hopeless"),
        f7Select("PHQitem2", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Trouble falling or staying asleep, or sleeping too much"),
        f7Select("PHQitem3", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Feeling tired or having little energy"),
        f7Select("PHQitem4", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Poor appetite or overeating"),
        f7Select("PHQitem5", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Feeling bad about yourself   or that you are a failure or have let yourself or your family down"),
        f7Select("PHQitem6", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Trouble concentrating on things, such as reading the newspaper or watching television"),
        f7Select("PHQitem7", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Moving or speaking so slowly that other people could have noticed. Or the opposite    being so figety or restless that you have been moving around a lot more than usual"),
        f7Select("PHQitem8", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Thoughts that you would be better off dead, or of hurting yourself"),
        f7Select("PHQitem9", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Button("PHQ9Submit", "Continue")
    )
  })



  output$GAD7<-renderUI({
    tagList(
      f7Card(
        h4("Feeling nervous, anxious, or on edge"),
        f7Select("GADitem1", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Not being able to stop or control worrying"),
        f7Select("GADitem2", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Worrying too much about different things"),
        f7Select("GADitem3", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Trouble relaxing"),
        f7Select("GADitem4", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Being so restless that it is hard to sit still"),
        f7Select("GADitem5", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Becoming easily annoyed or irritable"),
        f7Select("GADitem6", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Card(
        h4("Feeling afraid, as if something awful might happen"),
        f7Select("GADitem7", NULL , choices = c("Choose a response", "Not at all","Several Days","More than half the days","Nearly every day"), selected = NULL),
        footer = NULL,
        hairlines = F, strong = T, inset = F, tablet = FALSE),
        hr(),

      f7Button("GAD7Submit", "Finish")
    )
  })


}
