# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options("golem.app.prod" = TRUE,
         # whenever there is one account token found, use the cached token
         gargle_oauth_email = TRUE,
         # specify auth tokens should be stored in a hidden directory ".secrets"
         gargle_oauth_cache = "./.secrets")

MHScreener::run_app() # add parameters here (if any)

#
# googledrive::drive_auth()
# googlesheets4::gs4_auth()
# googlesheets4::gs4_create(name = "MHScreenerTest",
#                           # Create a sheet called main for all data to
#                           # go to the same place
#                           sheets = c("phq9","gad7"))
