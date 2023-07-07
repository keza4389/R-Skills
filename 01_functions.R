#############################################################
# Functions used in report     ##############################
#############################################################

paper.bgcolor <- "lightgray"
plot.bgcolor <- "lightgray"
default.height <- 600
default.width <- 800


#' Define a function to create a scatter plot
#'
#' @param data The data table containing the variables
#' @param x.variable The name of the variable for the x-axis
#' @param y.variable The name of the variable for the y-axis
#' @param color.variable The name of the variable for color grouping
#'
#' @return A plotly scatter plot figure
#' 
PlotScatter <- function(data, x.variable, y.variable, color.variable) {

  # Create the scatter plot using plot_ly
  fig <- plot_ly(
    data,
    x = ~get(x.variable),
    y = ~get(y.variable),
    color = ~get(color.variable),
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 8,
      line = list(color = "black", width = 0.5)
    ),
    # Set hovertemplate as text_info as defined before
    hovertemplate = data$text_info,
    height = default.height,
    width = default.width
  )
  
  # Customize the layout of the plot
  fig <- fig %>%
    layout(
      title = paste0("Scatter plot of ", x.variable, " vs. ", y.variable),
      xaxis = list(title = x.variable,
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff"),
      yaxis = list(title = y.variable,
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff"),
      plot_bgcolor = plot.bgcolor
    )
  
  return(fig)
}





#' Define a function to create a bar chart
#'
#' @param data The data table containing the variables
#' @param x.variable The name of the variable for the x-axis
#'
#' @return A plotly bar chart figure
#' 
PlotBar <- function(data, x.variable) {
  
  # Calculate the counts for each category of the x variable
  counts <- data[, .N ,by = x.variable]
  counts <- counts[, text_info:= paste0(x.variable," : ", get(x.variable),
                                        "<br>Frequency: ", N)]
  
  # Create the bar chart using plot_ly
  fig <- plot_ly(
    counts,
    x = ~get(x.variable),
    y = ~N,
    type = "bar",
    # Set hovertemplate as text_info as defined before
    hovertemplate = counts$text_info,
    text = ~N, textposition = 'auto',
    height = default.height,
    width = default.width
  )
  
  # Customize the layout of the chart
  fig <- fig %>%
    layout(
      title = paste0("Bar chart of ", x.variable, " frequencies"),
      xaxis = list(title = x.variable,
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff",
                   tickangle = -45),
      yaxis = list(title = "Frequency",
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff"),
      plot_bgcolor = plot.bgcolor
    )
  
  return(fig)
}


#' Define a function to create a box plot
#'
#' @param data The data table containing the variables
#' @param x.variable The name of the variable for the x-axis
#' @param y.variable The name of the variable for the y-axis
#'
#' @return A plotly box plot figure
#' 
PlotBox <- function(data, x.variable, y.variable) {
  
  # Calculate the upper and lower whiskers
  upper <- quantile(data[[y.variable]], 0.75) + 1.5 * IQR(data[[y.variable]])
  lower <- quantile(data[[y.variable]], 0.25) - 1.5 * IQR(data[[y.variable]])
  
  # Filter out the outliers
  data.filtered <- data[get(y.variable) <= upper & get(y.variable) >= lower]
  
  # Create the box plot using plot_ly
  fig <- plot_ly(
    data.filtered,
    x = ~get(x.variable),
    y = ~get(y.variable),
    type = "box",
    boxpoints = "all",
    jitter = 0.3,
    pointpos = -1.8,
    color = ~get(x.variable),
    height = default.height,
    width = default.width
  )
  
  # Customize the layout of the plot
  fig <- fig %>%
    layout(
      title = paste0("Box plot of ", y.variable, " by ", x.variable),
      xaxis = list(title = x.variable,
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff"),
      yaxis = list(title = y.variable,
                   zerolinecolor = "#ffff",
                   zerolinewidth = 2,
                   gridcolor = "ffff"),
      plot_bgcolor = plot.bgcolor
    )
  
  return(fig)
}



#' Title
#'
#' @param data The data table containing the variables
#' @param x.variable The name of the variable for the x-axis
#'
#' @return A plotly pie chart figure
#' 
PlotPie <- function(data, x.variable) {
  
  # Calculate the frequencies of each category in the variable
  counts <- data[, .N ,by = x.variable]
  counts <- counts[, text_info:= paste0(x.variable," : ", get(x.variable),
                                        "<br>Frequency: ", N)]
  
  # Create the pie chart using plot_ly
  fig <- plot_ly(
    counts,
    labels = ~get(x.variable),
    values = ~N,
    type = "pie",
    textinfo = "label+percent",
    textposition = "inside",
    hole = 0.4,
    height = default.height,
    width = default.width
  )
  
  # Customize the layout of the chart
  fig <- fig %>%
    layout(
      title = paste0("Pie chart of ", x.variable, " distribution"),
      showlegend = TRUE,
      paper_bgcolor = paper.bgcolor
    )
  
  return(fig)
}


# FUNCTION TAKEN FROM
# https://stackoverflow.com/a/58481176
#
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}







#' Perform k-fold cross-validation
#'
#' @param data Data table 
#' @param k The number of folds
#'
#' @return A list containing the confusion matrices
#' 
kfoldCrossValidation <- function(data, k) {
  # Create an empty list to store the confusion matrices for each fold
  confusion.matrices <- list()
  
  # Create an empty vector to store the predicted labels
  logit.pred <- rep('no', nrow(data))
  
  # Perform k-fold cross-validation
  folds <- sample(1:k, nrow(data), replace = TRUE)
  
  # Loop over each fold
  for (i in 1:k) {
    # Select the training and testing sets for the current fold
    train <- data[folds != i, ]
    test <- data[folds == i, ]
    
    # Fit the logistic regression model
    logit.model <- glm(y ~ ., data = train, family = "binomial")
    
    # Make predictions on the test data
    logit.prob <- predict(logit.model, newdata = test, type = "response")
    logit.pred <- rep('no',nrow(test))
    logit.pred[logit.prob>0.5] = 'yes'
    confusion.matrix<-table(logit.pred,test$y)
    
    # Store the confusion matrix in the list
    confusion.matrices[[i]] <- confusion.matrix
  }
  
  # Return the list of confusion matrices
  return(confusion.matrices)
}




#' Calculate average evaluation metrics
#'
#' This function calculates the average evaluation metrics (accuracy, precision,
#' recall, and F1 score) from a list of confusion matrices.
#'
#' @param confusion.matrices A list of confusion matrices.
#' @return A data frame containing the average evaluation metrics.
#' 
CalculateAverageMetrics <- function(confusion.matrices) {
  # Create empty vectors to store the sum of metrics
  sum.accuracy <- 0
  sum.precision <- 0
  sum.recall <- 0
  sum.f1.score <- 0
  
  # Loop over each confusion matrix
  for (i in 1:length(confusion.matrices)) {
    # Get the confusion matrix for the current fold
    confusion.matrix <- confusion.matrices[[i]]
    
    # Calculate the evaluation metrics
    accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
    precision <- confusion.matrix[2, 2] / sum(confusion.matrix[, 2])
    recall <- confusion.matrix[2, 2] / sum(confusion.matrix[2, ])
    f1.score <- 2 * (precision * recall) / (precision + recall)
    
    # Add the metrics to the sum
    sum.accuracy <- sum.accuracy + accuracy
    sum.precision <- sum.precision + precision
    sum.recall <- sum.recall + recall
    sum.f1.score <- sum.f1.score + f1.score
  }
  
  # Calculate the average metrics
  average.accuracy <- sum.accuracy / length(confusion.matrices)
  average.precision <- sum.precision / length(confusion.matrices)
  average.recall <- sum.recall / length(confusion.matrices)
  average.f1.score <- sum.f1.score / length(confusion.matrices)
  
  # Create a data table
  metrics.table <- data.table(Metric = c("Average Accuracy", "Average Precision", "Average Recall", "Average F1 Score"),
                              Value = c(average.accuracy, average.precision, average.recall, average.f1.score))
  
  return(metrics.table)
}









