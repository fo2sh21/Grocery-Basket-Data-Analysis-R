library(dplyr)
library(scales) 
library(arules)
library(ggplot2)
library(tidyr)
library(readr)
library(shiny)
library(DT) 
library(bslib)
library(ggrepel)

#--------------Cleaning and Sorting--------------#
CleanDS = function(data) {
  # Step 1: Print data structure
  cat("--- Data Structure ---\n")
  str(data)
  
  # Step 2: Remove duplicates
  cat("\n--- Removing Duplicates ---\n")
  data = data[!duplicated(data), ]
  cat("Number of rows after removing duplicates:", nrow(data), "\n")
  # Step 3: Remove rows with NA values
  cat("\n--- Removing Rows with NA Values ---\n")
  data = na.omit(data)
  cat("Number of rows after removing NA values:", nrow(data), "\n")
  # Step 4: Handle outliers
  cat("\n--- Handling Outliers ---\n")
  # Identify numeric columns
  numeric_cols = sapply(data, is.numeric)
  numeric_data = data[, numeric_cols, drop = FALSE]
  for (col in colnames(numeric_data)) {
    cat("Processing column:", col, "\n")
    # Get column data
    col_data = numeric_data[[col]]
    # Identify outliers using boxplot
    outliers = boxplot(col_data, plot = FALSE)$out
    cat("Outliers for column", col, ":", toString(outliers), "\n")
    if (length(outliers) > 0) {
      # Display rows with outliers
      cat("Rows with outliers in column", col, ":\n")
      print(data[which(col_data %in% outliers), ])
      # Remove rows with outliers
      data = data[-which(col_data %in% outliers), ]
      cat("Number of rows after removing outliers from column", col, ":", nrow(data), "\n")
    } else {
      cat("No outliers found in column", col, "\n")
    }
  }
  cat("\n--- Data Cleaning Complete ---\n")
  return(data)
}

ExtractAndAssign = function(DS) {
  customer_data = list()  # Initialize an empty list to hold customer data
  
  for (i in seq_len(nrow(DS))) {  # Loop over each row
    rnd = DS$rnd[i]  # GET the customer ID
    customer_name = DS$customer[i]  # GET the customer's name
    customer_city = DS$city[i]  # GET the city
    customer_age = DS$age[i]  # GET the age
    payment_type = DS$paymentType[i]  # GET the payment type
    payment = DS$total[i]
    
    # Make a list for each customer not in customer_data
    if (!rnd %in% names(customer_data)) {
      customer_data[[as.character(rnd)]] = list(
        name = customer_name,
        age = customer_age,
        cities = character(0),  # Make cities a vector in case a customer purchased from two or more seperate cities
        payment = c(Cash = 0, Credit = 0)  # Initialize payment counts
      )
    }
    # Add the city to the customer's list of cities (if not already present)
    if (!customer_city %in% customer_data[[as.character(rnd)]]$cities) {
      customer_data[[as.character(rnd)]]$cities = c(customer_data[[as.character(rnd)]]$cities, customer_city)
    }
    # Update the total payed
    if (payment_type == "Cash") {
      customer_data[[as.character(rnd)]]$payment["Cash"] = customer_data[[as.character(rnd)]]$payment["Cash"] + payment
    } else if (payment_type == "Credit") {
      customer_data[[as.character(rnd)]]$payment["Credit"] = customer_data[[as.character(rnd)]]$payment["Credit"] + payment
    }
  }
  # Calculate the preferred payment type for each customer
  for (rnd in names(customer_data)) {
    payment = customer_data[[rnd]]$payment
    total_payments = sum(payment)
    preferred_payment = ifelse(payment["Cash"] > payment["Credit"], "Cash", "Credit")
    payment_percentage = round((payment[preferred_payment] / total_payments) * 100, 2)
    customer_data[[rnd]]$preferred_payment = paste(preferred_payment, payment_percentage, "%")
  }
  for (i in seq_along(DS)) #seq_along is a function that generates a list of ascending numbers up to the max value of coloumns in parameter
  {
    column_name = colnames(DS)[i]  # Get the column name
    if (column_name %in% c("count", "total","age")) {
      next # Skip these columns entirely
    }
    if (column_name == "items") { 
      # Handle the special case for Items
      all_items = unlist(strsplit(DS[[i]], ",\\s*"))  # Split by ',' , \\ is an escape char and s for removing extra spaces, * for multiple occurances
      unique_values = unique(all_items)  # Get unique product names
    } else {
      unique_values = unique(DS[[i]])  # Extract unique values for other columns
    }
    # Dynamically assign vars
    assign(column_name, unique_values, envir = .GlobalEnv) #add these lists to the global enviroment to be used anywhere
  }
  return(customer_data)
}

#to be used in menu and items list
QueryOrders = function(data, query, column_name) {
  #Ensure the column name is treated as a character
  column_name = as.character(column_name)
  
  #Convert the query to character type to match the column data type
  query = as.character(query)
  
  #Trim extra white spaces
  query = trimws(query)
  
  #Convert the target column to character for consistent comparison
  target_column = as.character(trimws(data[[column_name]]))
  
  #Use grep for flexible partial matching and case-INSENSITIVE search by setting ignore.case to TRUE
  rows = grep(pattern = query, x = target_column, ignore.case = TRUE)
  
  #Filter the data frame based on the matching rows
  result = data[rows, ]
  
  return(result)
}

#-----------------clustering and AR---------------------------------#
customer_kmeans_clustering = function(customer_data, num_clusters) {
  # Extract customer information from Customer_data extracted from ExtractAndAssign
  customer_data_df = data.frame(
    customer_name = sapply(customer_data, function(x) x[[1]]),
    age = sapply(customer_data, function(x) x[[2]]),
    total_spending = sapply(customer_data, function(x) sum(x[[4]]))
  )
  # Perform K-means clustering
  set.seed(123)
  kmeans_result = kmeans(customer_data_df[, c("age", "total_spending")], centers = num_clusters)
  
  # Assign clusters to customers
  customer_data_df$cluster = kmeans_result$cluster
  
  # Print the clustered data
  print(customer_data_df)
  
  return(customer_data_df)
}
#Extracting to a text file eased the process of reading transactions and helped with problem tracing and debugging
textfile = function(data, file_name = "items.txt") {
  # Check for empty 'items' column
  if (all(is.na(data$items))) {
    stop("The 'items' column is empty. Please check your data.")
  }
  # Format the text
  items_text = data %>%
    pull(items) %>%
    paste(collapse = "\n")
  # Write the text to a file
  write_lines(items_text, file_name)
  # Return the file path
  return(file_name)
}
#the generation of rules starts from here
generate_association_rules = function(file_path, support, confidence) {
  # Read transactions from the text file
  transactions = read.transactions(file_path, sep = ",")
  # Check if the transaction data is empty
  if (nrow(transactions) == 0) {
    stop("The input file is empty or has no valid transactions.")
  }
  # Apply Apriori with parameter settings
  rules = apriori(transactions, parameter = list(supp = support, conf = confidence))
  # Filter rules with non-empty LHS and RHS
  rules = subset(rules, length(lhs) > 0 & length(rhs) > 0)
  # Sort rules by confidence and support
  sorted_rules = sort(rules, by = c("confidence", "support"), decreasing = TRUE)
  return(sorted_rules)
}

#--------------------------------UI-----------------------------------------------#

#styling part and formatting utilizing HTML and CSS and HTML like formatting of page for parenting of child objects
ui = fluidPage( 
  theme = bs_theme(
    bootswatch = "lux",
    base_font = font_google("Roboto"),
    primary = "#000000",
    secondary = "#ffffff"
  ),# a theme essentialy for bootstrap to be used on the web but we can use it here 
  
  #tags found by inspecting the page of the GUI
  tags$style(HTML("
    /*slider colors */
    .irs--shiny .irs-bar { background-color: #000; border-color: #000; }
    .irs--shiny .irs-single, .irs--shiny .irs-min,
    .irs--shiny .irs-max,.irs-from,.irs-to { color: #FFF; background-color:#000; }
    .irs-line { background: #fff; border-color: #000; }
    
    /* Button styles */
    .btn { 
      background-color: #000; 
      color: #fff; 
      border: 3px solid #000; 
      border-radius: 5px; 
      transition: background-color 0.3s ease-in-out, color 0.3s ease-in-out, border-color 0.3s ease-in-out;
    }
    .btn:hover { 
      background-color: #fff; 
      color: #000; 
      border-color: #000; 
    }
    
    #file_progress{
    margin-top:3px;
    }
    
    .container-fluid { padding: 15px; }
    .main-panel, .sidebar-panel { margin-bottom: 15px; }
    
    /* Responsive menus */
    .nav > li > a, .navlist-panel > li > a { 
      font-size: 1rem; 
    }
    .navlist-panel > ul { 
      flex: 1; 
    }
  ")),
  
  navbarPage(
    title = "Data Analysis Dashboard",
    id = "nav",
    
    # Data Upload and Cleaning
    tabPanel(
      "Data Upload & Cleaning",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload Data File (CSV)", accept = ".csv"),
          actionButton("clean_data", "Clean Data")
        ),
        mainPanel(
          h4("Cleaned Dataset"),
          DTOutput("cleaned_data")
        )
      )
    ),
    
    # Visualization
    tabPanel(
      "Visualization",
      navlistPanel(
        widths = c(2, 10),
        tabPanel("Payment by Type", plotOutput("payment_plot", height = "500px")),
        tabPanel("Spending by Age", plotOutput("age_plot", height = "500px")),
        tabPanel("Spending by City", plotOutput("city_plot", height = "500px")),
        tabPanel("Spending Distribution", plotOutput("distribution_plot", height = "500px"))
      )
    ),
    
    # Clustering
    tabPanel(
      "Clustering",
      sidebarLayout(
        sidebarPanel(
          sliderInput("clusters", "Number of Clusters", min = 2, max = 4, value = 3, step = 1),
          actionButton("run_clustering", "Run Clustering")
        ),
        mainPanel(
          DTOutput("cluster_table"),
          plotOutput("cluster_plot", height = "600px")
        )
      )
    ),
    
    # Association Rules
    tabPanel(
      "Association Rules",
      sidebarLayout(
        sidebarPanel(
          sliderInput("support", "Support Threshold:", min = 0.001, max = 1, value = 0.01, step = 0.001),
          sliderInput("confidence", "Confidence Threshold:", min = 0.001, max = 1, value = 0.5, step = 0.01),
          actionButton("generate_rules", "Generate Rules"),
          textOutput("rule_count")
        ),
        mainPanel(
          h4("Generated Rules"),
          DTOutput("rules_table")
        )
      )
    ),
    
    # Lists
    tabPanel(
      "Lists",
      navlistPanel(
        widths = c(2, 10),
        tabPanel("Items", DTOutput("items_table"), h4("Transactions for Selected Item"), DTOutput("item_transactions")),
        tabPanel("City", DTOutput("city_table"), h4("Transactions for Selected City"), DTOutput("city_transactions")),
        tabPanel("Payment Types", DTOutput("payment_table"), h4("Transactions for Selected Payment Type"), DTOutput("payment_transactions"))
      )
    ),
    
    # Customers
    tabPanel(
      "Customers",
      sidebarLayout(
        sidebarPanel(
          DTOutput("customers_table")
        ),
        mainPanel(
          h4("Customer Details"),
          div(
            style = "display: flex; flex-direction: row; justify-content: space-between;",
            div(style = "width: 50%;", h5("Customer Information"), DTOutput("customer_info_table")),
            div(style = "width: 50%;", h5("Preferred Payment Pie Chart"), plotOutput("customer_payment_pie", height = "400px"))
          ),
          h4("Transactions by Customer"),
          DTOutput("customer_transactions")
        )
      )
    )
  )
)


#server is the backend for the hosted web page
server = function(input, output, session) {
  # Reactive values to store data
  data = reactiveVal() #reactiveVal() works like how you expect a Var to work but specially taylored for Reactive web pages to notify and re-run commands on updating its value for any part of code that was dependent on it
  cleaned_data = reactiveVal()
  customer_data = reactiveVal()
  
  observeEvent(input$file, {  #observeEvent works like addEventListener in JS waiting for a specific input or click and runs when satisfied
    req(input$file) #req short for require meaining it wont run the rest without the required parameter BUT with the benefit of not giving an error or shuting down the server in the process
    dataset = read_csv(input$file$datapath) #instead of . in OOP this syntax uses $
    data(dataset) #assigning using parameters since data is now reactiveVal() not just a var
    showNotification("File successfully uploaded!", type = "message") #shows notification -- self explanatory
  })
  
  observeEvent(input$clean_data, {
    req(data())
    cleaned_ds = CleanDS(data())
    cleaned_data(cleaned_ds)
    customer_data(ExtractAndAssign(cleaned_ds))
    showNotification("Dataset successfully cleaned!", type = "message")
  })
  
  output$cleaned_data = renderDT({  #renderDT is short for render Data Table to render the data as a table 
    req(cleaned_data())
    datatable(cleaned_data())
  })
  
  observeEvent(input$run_clustering, {
    req(customer_data())
    num_clusters <- input$clusters
    
    # Prepare clustering data
    cluster_result <- customer_kmeans_clustering(customer_data(), num_clusters)
    
    # Add customer names to the clustering results
    cluster_result$customer_name <- sapply(customer_data(), function(x) x$name)
    
    output$cluster_table <- renderDT({
      datatable(cluster_result)
    })
    
    output$cluster_plot <- renderPlot({
      ggplot(cluster_result, aes(x = age, y = total_spending, color = as.factor(cluster))) +
        geom_point(size = 3) +
        geom_text_repel(aes(label = customer_name), size = 3) + # Prevent overlapping text
        labs(
          title = "Customer Clustering",
          x = "Age",
          y = "Total Spending",
          color = "Cluster"
        ) +
        theme_minimal()
    })
  })
  
  #AR (association rules)
  observeEvent(input$generate_rules, {
    req(cleaned_data())
    
    cleaned = cleaned_data()
    tryCatch({   #TryCatch is a short for two terms: 1-Try meaining to do the function but just as a try again means of not crashing 
      #the program in case of anything going wrong the AR process is splitted into two functions any error from one affects the other and could lead to an error
      text_file = textfile(cleaned, "items.txt")
      rules = generate_association_rules(text_file, input$support, input$confidence)
      
      if (length(rules) == 0) {
        showNotification("No rules found with the given thresholds.", type = "warning")
      } else {
        output$rule_count = renderText(paste("Total Rules Generated:", length(rules)))
        rules_df = as(rules, "data.frame")
        output$rules_table = renderDT(rules_df, options = list(pageLength = 10))
      }
    }, error = function(e) { #here is the catch for catching any error and showing a notification for the user to notify him
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  
#-----------------------------------Visualization-------------------------------#  
  # Render plots
  output$payment_plot = renderPlot({
    req(cleaned_data())
    cleaned = cleaned_data()
    payment_summary = cleaned %>%
      group_by(paymentType) %>%
      summarize(total_spending = sum(total, na.rm = TRUE))
    # Visualize the comparison using a bar plot with better number formatting
    ggplot(payment_summary, aes(x = paymentType, y = total_spending)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Total Spending by Payment Type", x = "Payment Type", y = "Total Spending") +
      scale_y_continuous(labels = scales::comma) +  # Format numbers in commas instead of e + 0 format
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))

  })
  
  output$age_plot <- renderPlot({
    req(cleaned_data())
    cleaned <- cleaned_data()
    age_summary <- cleaned %>%
      group_by(age) %>%
      summarize(total_spending = sum(total, na.rm = TRUE))
    
    ggplot(age_summary, aes(x = age, y = total_spending)) +
      geom_line(color = "deepskyblue", size = 1.5) +  # Solid line for clarity
      geom_point(color = "orange", size = 4, shape = 21, fill = "orange") +
      labs(title = "Total Spending by Age", x = "Age", y = "Total Spending") +
      scale_x_continuous(breaks = unique(age_summary$age)) +  # Show each age on the x-axis
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.grid.minor = element_blank()
      )
  })
  
  
  output$city_plot <- renderPlot({
    req(cleaned_data())
    cleaned <- cleaned_data()
    city_summary <- cleaned %>%
      group_by(city) %>%
      summarize(total_spending = sum(total, na.rm = TRUE)) %>%
      arrange(desc(total_spending))
    
    # Create a color palette for cities with more muted colors and slight transparency
    city_colors <- scale_fill_manual(values = alpha(RColorBrewer::brewer.pal(n = nrow(city_summary), name = "Set3"), 0.7))
    
    # Visualize the spending per city with individual colors, legend, and slight transparency
    ggplot(city_summary, aes(x = reorder(city, -total_spending), y = total_spending, fill = city)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Total Spending by City", x = "City", y = "Total Spending") +
      scale_y_continuous(labels = scales::comma) +
      city_colors +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10, angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "right"
      )
  })
  
  
  output$distribution_plot <- renderPlot({
    req(cleaned_data())
    cleaned <- cleaned_data()
    
    ggplot(cleaned, aes(x = total)) +
      geom_freqpoly(binwidth = 100, color = "darkblue", size = 1.5) +  # Adjust line color and size
      labs(title = "Distribution of Total Spending", x = "Total Spending", y = "Frequency") +
      scale_x_continuous(labels = scales::comma, breaks = seq(0, max(cleaned$total, na.rm = TRUE), by = 100)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  
  # Lists Tab: Items
  output$items_table = renderDT({
    req(cleaned_data())
    datatable(data.frame(Item = unique(items)), selection = "single")
  })
  
  observeEvent(input$items_table_rows_selected, {
    selected_item = items[input$items_table_rows_selected]
    transactions = QueryOrders(cleaned_data(), selected_item, "items")
    output$item_transactions = renderDT({
      datatable(transactions)
    })
  })
  
  # Lists Tab: City
  output$city_table = renderDT({
    req(cleaned_data())
    datatable(data.frame(City = unique(city)), selection = "single")
  })
  
  observeEvent(input$city_table_rows_selected, {
    selected_city = city[input$city_table_rows_selected]
    transactions = QueryOrders(cleaned_data(), selected_city, "city")
    output$city_transactions = renderDT({
      datatable(transactions)
    })
  })
  
  # Lists Tab: Payment Types
  output$payment_table = renderDT({
    req(cleaned_data())
    datatable(data.frame(PaymentType = unique(paymentType)), selection = "single")
  })
  
  observeEvent(input$payment_table_rows_selected, {
    selected_payment = paymentType[input$payment_table_rows_selected]
    transactions = QueryOrders(cleaned_data(), selected_payment, "paymentType")
    output$payment_transactions = renderDT({
      datatable(transactions)
    })
  })
  
  
  # Customers Tab
  output$customers_table = renderDT({
    req(customer_data())
    customer_list = customer_data()
    datatable(
      data.frame(
        ID = as.integer(names(customer_list)),
        Name = sapply(customer_list, function(x) x$name) #usage of sapply and function(x) to access a list inside of a list
      ),#function(x) is an anonymus function its purpuse is to interacte with the list (x) and get the name since it applies to every element of Customer_list
      selection = "single",
      rownames = FALSE
    )
  })
  observeEvent(input$customers_table_rows_selected, {
    selected_row = input$customers_table_rows_selected #get the selected row that was clicked
    customer_list = customer_data()
    
    if (!is.null(selected_row)) {
      # Get the actual customer ID based on the selected row index
      customer_ids = as.integer(names(customer_list))
      selected_id = customer_ids[selected_row]
      
      selected_customer = customer_list[[as.character(selected_id)]]
      
      # Display customer info
      output$customer_info_table = renderDT({
        datatable(data.frame(
          Name = selected_customer$name,
          Age = selected_customer$age,
          Cities = paste(selected_customer$cities, collapse = ", "),#cities is a vector hence the need for collapse = ", " to seperate
          PreferredPayment = selected_customer$preferred_payment
        ), options = list(dom = "t"), rownames = FALSE) #tells the DOM of the browser to only display the "t" short for table
      })
      # Display pie chart of payment types
      output$customer_payment_pie = renderPlot({
        pie(
          selected_customer$payment,
          labels = paste(names(selected_customer$payment), selected_customer$payment),
          main = paste("Payment Distribution for", selected_customer$name)
        )
      })
      # Query transactions for the customer
      transactions = QueryOrders(cleaned_data(), selected_customer$name, "customer")
      output$customer_transactions = renderDT({
        datatable(transactions)
      })
    }
  })
  
}

shinyApp(ui = ui, server = server) #a final while loop command to display the ui and connect it to the server

#----------------------------------------END---------------------------------------------------------------------#
