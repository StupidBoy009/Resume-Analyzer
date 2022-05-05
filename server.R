# Load
library(tm)
library(Matrix)
library(readr)
library(SnowballC) 
#########Functions#########
cos_sim = function(matrix){
  numerator = matrix %*% t(matrix)
  A = sqrt(apply(matrix^2, 1, sum))
  denumerator = A %*% t(A)
  return(numerator / denumerator)
}

corpus_preprocess = function(corpus){
  #Replacing "/", "@" and "|" with space
  to_space <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus <- tm_map(corpus, to_space, "/")
  corpus <- tm_map(corpus, to_space, "@")
  corpus <- tm_map(corpus, to_space, "\\|")
  corpus <- tm_map(corpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  # Convert the text to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Remove english common stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Remove punctuations
  corpus <- tm_map(corpus, removePunctuation)
  # Eliminate extra white spaces
  corpus <- tm_map(corpus, stripWhitespace)
  # Text stemming - which reduces words to their root form
  corpus <- tm_map(corpus, stemDocument)
  print('Finish preprocessing..')
  return(corpus)
}

rjo_score = function(dataset,occupation_df){
  for(row in 1:nrow(dataset)) {
    job_df <- data.frame(job = dataset[row,1], description = dataset[row,2])
    occu_job <- rbind(job_df,occupation_df)
    names(occu_job)[1]="doc_id"
    names(occu_job)[2]="text"
    corpus = VCorpus(DataframeSource(occu_job))
    
    # Preprocessing
    corpus_preprocessed2 = corpus_preprocess(corpus)
    
    # Creating term matrix with TF-IDF weighting (normalized)
    tfidfm2 <- DocumentTermMatrix(corpus_preprocessed2, control = list(weighting = weightTfIdf))
    tfidfm_m2 = as.matrix(tfidfm2)
    
    # Calculate cosine similarity with TF-IDF Matrix
    cos_sim_m2 = cos_sim(tfidfm_m2)
    
    if (row==1){
      occu_job_res <- occu_job
      # Create a new column for similarity_score of dataframe
      occu_job_res[paste(dataset[row,1],row, sep = "-")] = cos_sim_m2[1:ncol(cos_sim_m2)]
      print('First row')
      print(row)
    }
    else if (row==nrow(dataset)){
      # Create a new column for similarity_score of dataframe
      occu_job_res[paste(dataset[row,1],row, sep = "-")] = cos_sim_m2[1:ncol(cos_sim_m2)]
      occu_job_res = occu_job_res[-1,]
      print('Last row')
      print(row)
    }
    else{
      # Create a new column for similarity_score of dataframe
      occu_job_res[paste(dataset[row,1],row, sep = "-")] = cos_sim_m2[1:ncol(cos_sim_m2)]
      print(row)
    }
  }
  return(occu_job_res)
}

rjo_cos_score = function(dataset,dataset_top){
  resume_job_m <- t(dataset[,3:ncol(dataset)])
  # Calculate cosine similarity score using jobs and resume vectors
  cos_sim_rj_m = cos_sim(resume_job_m)
  # Create a new row for cosine_similarity of dataframe
  dataset[nrow(dataset)+1,1:2] <- c('similarity_score(via resume)','N/A')
  dataset[nrow(dataset),3:ncol(dataset)] = dataset_top[1:nrow(dataset_top),3]
  dataset[nrow(dataset)+1,1:2] <- c(3,2)
  dataset[nrow(dataset),3:ncol(dataset)] <- cos_sim_rj_m[1,1:ncol(cos_sim_rj_m)]
  dataset = round_df(dataset,4)
  colnames(dataset)[1] <-"Occupation"
  colnames(dataset)[2] <-"Description"
  
  # Rank jobs based on cosine_similarity
  dataset_ordered <- t(dataset)
  dataset_ordered = dataset_ordered[order(dataset_ordered[,ncol(dataset_ordered)],decreasing=TRUE),]
  dataset_ordered <- t(dataset_ordered)
  dataset_ordered[nrow(dataset),1:2] <- c('similarity_score(via occupations)','N/A')
  dataset_ordered <- as.data.frame(dataset_ordered)
  dataset_ordered = dataset_ordered[nrow(dataset_ordered):1,]
  
  return(dataset_ordered)
}

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  return(df)
}

####################
occupation_df <- read.csv('O_NET JOBS.csv')

function(input, output, session){
  # Download occupations
  output$download_occupations <- downloadHandler(
    filename = function(){
      paste("Occupations ", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
        write.csv(occupation_df,file)
    })
  
  
  mydata1 <- reactive({
    # Progress bar
    withProgress(message = 'Making table', value = 0, {
    # Jobs
    inFile <- input$uploaded_jobs
    
    if (is.null(inFile))
      return(NULL)
    
    jobs_df <- read.csv(inFile$datapath)
    
    # Resume
    inFile2 <- input$uploaded_resume
    
    if (is.null(inFile2))
      return(NULL)
    
    resume <- read_file(inFile2$datapath)
    # Make resume content a dataframe
    resume_df <- data.frame(job = "You", description = resume)
    
    ############Resume & Jobs#####################
    # Combine resume and job description
    resume_job <- rbind(resume_df,jobs_df)
    # Change column name
    names(resume_job)[1]="doc_id"
    names(resume_job)[2]="text"
    
    # Load the data as a corpus
    corpus = VCorpus(DataframeSource(resume_job))
    #Preprocessing
    corpus_preprocessed = corpus_preprocess(corpus)
    # Build a term-document matrix
    dtm <- DocumentTermMatrix(corpus_preprocessed)
    dtm_m <- as.matrix(dtm)
    # Creating term matrix with TF-IDF weighting (normalized)
    tfidfm <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
    tfidfm_m = as.matrix(tfidfm)
    
    # Calculate tf-idf scores
    cos_sim_m = cos_sim(tfidfm_m)
    # Create a new column for similarity_score of dataframe
    resume_job["similarity_score"] = cos_sim_m[1:ncol(cos_sim_m)]
    # Sort dataframe based on similarity_score
    resume_job_sim = resume_job[order(-resume_job$similarity_score),]
    
    ############Resume & Jobs & Occupations#####################
    incProgress(2/10, detail = paste("Calculating bottom 5 jobs"))
    
    # Bot 5 jobs
    dataset_bot = rbind(head(resume_job_sim,1),tail(resume_job_sim,6))
    
    occu_job_bot = rjo_score(dataset_bot,occupation_df)
    occu_job_bot_ordered = rjo_cos_score(occu_job_bot,dataset_bot)
    occu_job_bot_ordered2 = occu_job_bot_ordered[1:2,3:ncol(occu_job_bot_ordered)]
    occu_job_bot_ordered2 <- t(occu_job_bot_ordered2)
    colnames(occu_job_bot_ordered2)[1] <-"similarity_score(via occupations)"
    colnames(occu_job_bot_ordered2)[2] <-"similarity_score(via resume)"
    
    incProgress(2/10, detail = paste("Calculating top 15 jobs"))
    
    # TOP 15 jobs 
    dataset_top = head(resume_job_sim,16)
    
    occu_job_top = rjo_score(dataset_top,occupation_df)
    occu_job_top_ordered = rjo_cos_score(occu_job_top,dataset_top)
    occu_job_top_ordered2 = occu_job_top_ordered[1:2,3:ncol(occu_job_top_ordered)]
    occu_job_top_ordered2 <- t(occu_job_top_ordered2)
    colnames(occu_job_top_ordered2)[1] <-"similarity_score(via occupations)"
    colnames(occu_job_top_ordered2)[2] <-"similarity_score(via resume)"

    incProgress(6/10, detail = paste("Finish"))
    #####################Download output############
    
    output$download_top_jobs2 <- downloadHandler(
      filename = function(){
        paste("Top jobs ranking ", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(occu_job_top_ordered2,file)
      })
    
    output$download_bot_jobs2 <- downloadHandler(
      filename = function(){
        paste("Bot jobs ranking ", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(occu_job_bot_ordered2,file)
      })
    
    output$download_resume_job <- downloadHandler(
      filename = function(){
        paste("Jobs ranking via resume ", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(resume_job_sim,file)
      })
    
    output$download_top_jobs <- downloadHandler(
      filename = function(){
        paste("Top jobs ranking details ", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(occu_job_top_ordered,file)
      })
    
    output$download_bot_jobs <- downloadHandler(
      filename = function(){
        paste("Bot jobs ranking details ", Sys.Date(), ".csv", sep="")
      },
      content = function(file){
        write.csv(occu_job_bot_ordered,file)
      })
    
    return(occu_job_top_ordered)
    })
    })
    
    output$jobs <- renderTable({
      mydata1()
    },
    striped = TRUE,
    hover = TRUE)

}

