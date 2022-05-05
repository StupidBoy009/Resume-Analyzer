
navbarPage(
  "Resume Analyzer",
  tabPanel(
    "Analyze your resume & jobs",
    fluidPage(
      wellPanel(h4("Please upload your resume(.txt file) and job list(.csv file) file."),
                br(),
                p("You can download the result if the progress bar finishes loading."),
                hr(),
                tags$i(p('Below is an example of the job list file.')),
                img(src = "job_list.png", height = 270, width = 500)
                ),
      
      fluidRow(
        column(
          downloadButton("download_occupations",
                         "Download occupations"),
          width = 2),
        ),
      
      fluidRow(
        br(),
        column(fileInput("uploaded_resume",
                         label = "Upload resume",
                         multiple = FALSE),
               width = 6),
        
        column(fileInput("uploaded_jobs",
                         label = "Upload job list",
                         multiple = FALSE),
               width = 6)
      ),
      
      fluidRow(
        column(
          downloadButton("download_top_jobs2",
                         "Download top jobs"),
          width = 4),
        
        column(
          downloadButton("download_bot_jobs2",
                         "Download bottom jobs"),
          width = 4),
        
        column(
          downloadButton("download_resume_job",
                         "Download jobs via resume"),
          width = 4)
        ),
      
      fluidRow(
        br(),  
        column(
          downloadButton("download_top_jobs",
                         "Download top jobs details",
                         class = "butt"),
          width = 4),
        
        column(
          downloadButton("download_bot_jobs",
                         "Download bottom jobs details"),
          width = 4)
      ),
      
      fluidRow(
        br(),
        tableOutput(outputId = 'jobs')
      )
      
    )
  ),
  collapsible = TRUE
)