#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyMobile
#' @import htmltools
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    shinyMobile::f7Page(
      title = "Mental Health Screener",
      options = list(theme=c("auto"), dark=TRUE, preloader = F,  pullToRefresh=F),
      allowPWA=TRUE,
      f7TabLayout(
        # panels are not mandatory. These are similar to sidebars
        navbar = f7Navbar(
          title= "Project ACCESS Mental Health Screener"),
        # f7Tabs is a special toolbar with included navigation
        f7Tabs(
          animated = TRUE,
          id = "tabs",
          f7Tab(
            tabName = "WelcomeTab",
            icon = f7Icon("house_fill"),
            active = TRUE,
            hidden= T,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("Welcome to the Project ACCESS Mental Health Screener"),side=c("center")),
                  uiOutput("StartButton"),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)
              )
            )
          ),


          f7Tab(
            tabName = "GettingStartedTab",
            icon = f7Icon("house_fill"),
            active = F,
            hidden=T,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("To start the screener click the button below"),side=c("center")),
                  f7Button("Screenerprompt", "Start!"),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)
              )
            )

          ),


          f7Tab(
            tabName = "PHQ9tab",
            icon = f7Icon("search"),
            active = F,
            hidden=T,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("Part 1 of 2"), side=c("center")),
                  h3("INSTRUCTIONS:"),
                  h4("PHQ9 INSTRUCTIONS HERE!"),
                  uiOutput("PHQ9"),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)
              )
            )

          ),


          f7Tab(
            tabName = "GAD7tab",
            icon = f7Icon("search"),
            active = F,
            hidden=T,
            f7Block(
              f7Shadow(
                intensity = 5,
                hover = TRUE,
                f7Card(
                  f7Align(h2("Part 2 of 2"), side=c("center")),
                  h3("INSTRUCTIONS:"),
                  h4("GAD7 INSTRUCTIONS HERE!"),
                  uiOutput("GAD7"),
                  footer = NULL,
                  hairlines = F, strong = T, inset = F, tablet = FALSE)
              )
            )

          ),


          f7Tab(
            tabName = "DoneTab",
            icon = f7Icon("house_fill"),
            active = F,

            f7Shadow(
              intensity = 5,
              hover = TRUE,
              f7Card(
                title = "",
                h2("Enter Navigator ID and Password"),
                f7Text(NavUsername, label = "Navigator ID: ", value=NULL, placeholder = "Enter Navigator ID number"),
                f7Password(NavPassword, label = "Password : ", value = "", placeholder = NULL),
                f7Button("Login", "Login"),
                hairlines = F, strong = T, inset =
                  F, tablet = FALSE)
            )
          ),




          f7Tab(
            tabName = "Output_tab",
            icon = f7Icon("house_fill"),
            active = F,

            uiOutput("datasummaryIntro"),
            f7Shadow(
              intensity = 5,
              hover = TRUE,
              f7Card(
                title = "",
                f7Button("StartOver", "Start Over"),
                hairlines = F, strong = T, inset =
                  F, tablet = FALSE)
            )
          )






        )
      )
    )







  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  # add_resource_path(
  #   "www",
  #   app_sys("app/www")
  # )

  tags$head(
    includeCSS("www/framework7.bundle.min.css")
    # favicon(),
    # bundle_resources(
    #   path = app_sys("app/www"),
    #   app_title = "MHScreener"
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
