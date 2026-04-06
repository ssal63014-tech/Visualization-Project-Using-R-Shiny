
# Load necessary library =======================================================

library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(RColorBrewer)

# Create the Shiny UI ==========================================================

ui <- fixedPage(
  
  # Centered title at the top
  tags$h2(HTML("<strong>Credit Realization Trends of MSMEs by Enterprises Scale and Industry, 
               <br> with Insights into Financial Inclusion of Indonesia </strong>"), 
          style = "text-align: center;"),
  
  # Add CSS for styling the entire page
  tags$style(HTML("
    * {
      font-family: Arial;  
    }
    font
    .description-box p {
      font-size: 16px;  
      line-height: 1.5;
    }
    
    .centered-row {
      display: flex;
      align-items: center; 
      justify-content: space-between; 
      margin-bottom: 50px; 
    }
    
    .description-box {
      margin-left: 20px; 
    }
  ")),
  
  # First row: Enterprises Productivity Trend plot and description
  fixedRow(
    div(class = "centered-row",
        column(width = 8, plotOutput("enterprisesPlot")),
        column(width = 4, div(class = "description-box",
                              h3(strong("MSMEs Trend in Indonesia")),
                              htmlOutput("descriptionTextEnterprises")
        ))
    )
  ),
  
  # Second row: Interactive Heatmap and description (swapped)
  fixedRow(
    div(class = "centered-row",
        column(width = 4, div(class = "description-box",
                              h3(strong("Credit Distribution Accross Various Industry")),
                              htmlOutput("descriptionTextHeatmap")
        )),
        column(width = 8, plotlyOutput("heatmapPlot"))  
    )
  ),
  
  # Third row: Scatter plot for Financial Inclusion with region selection and description
  fixedRow(
    div(class = "centered-row",
        column(width = 6, plotlyOutput("scatterPlot")),
        column(width = 2, checkboxGroupInput("selected_regions", "Select Regions:", choices = NULL, selected = NULL)),
        column(width = 4, div(class = "description-box",
                              h3(strong("Development of Indonesia Financial Inclusion")),
                              htmlOutput("descriptionTextScatterPlot")
        ))
    )
  )
)


# Create the Shiny server ======================================================

server <- function(input, output, session) {
  
  # Read the dataset for Enterprises Classifications (sheet 1)
  df_enterprises <- read_excel("Dataset_DVP.xlsx", sheet = "Enterprises Classifications")
  
  # Clean column names and convert Year column
  colnames(df_enterprises) <- trimws(colnames(df_enterprises))
  df_enterprises$Year <- as.numeric(df_enterprises$Year)
  
  # Set axis limits for the first plot
  primary_min <- -30
  primary_max <- 60
  npl_min <- 1
  npl_max <- 6.5
  
  # Calculate scaling factor for secondary axis
  scale_factor <- (primary_max - primary_min) / (npl_max - npl_min)
  
  # Render the first plot for line chart
  output$enterprisesPlot <- renderPlot({
    
    # Scale NPL Credit to match the primary axis
    df_enterprises_long <- df_enterprises %>%
      pivot_longer(cols = c(`Number of Account (in Millions)`, `Growth of Credits (%, yoy)`, `NPL Credit (%)`), 
                   names_to = "metric", values_to = "value") %>%
      mutate(value = ifelse(metric == "NPL Credit (%)", 
                            (`value` - npl_min) * scale_factor + primary_min, 
                            value),
             metric = recode(metric, 
                             `Number of Account (in Millions)` = "Accounts", 
                             `Growth of Credits (%, yoy)` = "Growth of Credits", 
                             `NPL Credit (%)` = "NPL Credit"))
    
    # Define colors and linetypes for each metric
    color_mapping <- c("Accounts" = "#FFD700", "Growth of Credits" = "#1976D2", "NPL Credit" = "#D32F2F")
    linetype_mapping <- c("Accounts" = "solid", "Growth of Credits" = "dashed", "NPL Credit" = "dotted")
    
    ggplot(df_enterprises_long, aes(x = Year, y = value, color = metric, linetype = metric)) +
      
      # Use geom_line and geom_point for each metric
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.5) +
      
      # Set custom scales for color and linetype
      scale_color_manual(values = color_mapping) +
      scale_linetype_manual(values = linetype_mapping) +
      
      # Customize the y-axis with dual axis for NPL Credit
      scale_y_continuous(
        name = "Accounts (Millions) & Growth of Credits (%)",
        limits = c(primary_min, primary_max),
        breaks = seq(primary_min, primary_max, by = 10),
        sec.axis = sec_axis(~ (. - primary_min) / scale_factor + npl_min, 
                            name = "NPL Credit (%)", 
                            breaks = seq(npl_min, npl_max, by = 0.5))
      ) +
      
      # Set x-axis scale and labels
      scale_x_continuous(
        breaks = unique(df_enterprises$Year), 
        labels = function(x) substring(as.character(x), 3, 4),
        expand = c(0.01, 0.01)
      ) +
      
      # Create separate panels for each enterprise classification
      facet_wrap(~ `Enterprises Classifications`, nrow = 1, scales = "free_y") +
      
      # Add plot title and labels
      labs(title = "Enterprises Productivity Trend", x = "Year") +
      
      # Apply a minimal theme and customize it
      theme_minimal() +
      theme(
        text = element_text(family = "Arial"),  
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.title.y = element_text(size = 12),
        axis.title.y.right = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
  })
  
  
  # Render the description text for line chart
  output$descriptionTextEnterprises <- renderUI({
    HTML(
      "<p>The graph provides a detailed overview of the productivity trends for Medium, Micro, and Small enterprises between 2016 and 2024, 
    focusing on three key indicators: the number of accounts (in millions), the growth of credits (percentage), and non-performing loans (NPL) credit (percentage). 
    Medium enterprises show relatively stable performance, with the number of accounts remaining consistent and credit growth demonstrating moderate fluctuations. 
    However, from 2018 onwards, the NPL credit begins to rise, peaking in 2021, signaling increasing financial strain as these enterprises face challenges in managing debt. 
    Micro enterprises, on the other hand, reveal a high-risk, high-reward dynamic, with aggressive credit growth peaking dramatically in 2021, followed by a sharp decline. 
    This boom-and-bust cycle is mirrored in the NPL credit, which rises sharply, suggesting that the rapid credit expansion may have led to unsustainable debt levels by 2024.</p>

    <p>Small enterprises present a more stable trajectory, with the number of accounts remaining relatively consistent, despite fluctuations in credit growth, particularly in 2021. 
    Although NPL credit shows an initial improvement between 2016 and 2021, it starts rising again after 2021, with a significant increase in 2023 and 2024. 
    By 2024, NPL percentages exceed 5%, indicating that many small enterprises struggled with their loan obligations as economic conditions worsened. 
    Overall, the graph reveals key differences in financial health and credit behavior across enterprise categories, highlighting the varying levels of risk and stability within these business segments over time.</p>"
    )
  })
  
  # Read the dataset for Industry Origin (sheet 2)
  df_industry <- read_excel("Dataset_DVP.xlsx", sheet = "Industry Origin")
  
  # Clean column names
  colnames(df_industry) <- trimws(colnames(df_industry))
  
  # Custom hover labels using lapply and sprintf for heatmap
  hover_text <- lapply(1:nrow(df_industry), function(i) {
    sprintf(
      "Year: %s<br>Main Factor Loan Recipient: %s<br>Total Outstanding Amount: %.2f Billion Rp<br>Growth of Credit: %.2f%%<br>GDP Deflator: %.2f%%",
      df_industry$Year[i],
      df_industry$`Main Factor Loan Recipients`[i],
      df_industry$`Total of Outstanding Amount (Billions of Rp)`[i],
      df_industry$`Growth of Credit (%, yoy)`[i],
      df_industry$`GDP Deflator (%)`[i]
    )
  })
  
  # Render the interactive heatmap using plotly
  output$heatmapPlot <- renderPlotly({
    plot_ly(
      data = df_industry,
      x = ~Year,
      y = ~`Main Factor Loan Recipients`,
      z = ~`Total of Outstanding Amount (Billions of Rp)`,
      text = hover_text,
      hoverinfo = "text",
      type = "heatmap",
      colorscale = list(
        c(0, "lightyellow"),  
        c(1, "darkblue")      
      ),
      colorbar = list(
        title = list(
          text = "Total Outstanding Amount (Billion Rp)",
          font = list(size = 14, family = "sans-serif"),
          side = "top",  
          standoff = 5
        ),
        orientation = "h",  
        x = 0.5,  
        y = -0.2,  
        xanchor = "center",  
        thickness = 20,  
        len = 1  
      )
    ) %>% layout(
      title = list(
        text = "Loan Outstanding Amount by Industry",
        x = 0.7,  
        xanchor = "center",  
        font = list(size = 18, family = "sans-serif", color = "black")
      ),
      xaxis = list(
        title = "Year",
        titlefont = list(size = 14, family = "sans-serif"), 
        tickfont = list(size = 12, family = "sans-serif")    
      ),
      yaxis = list(
        title = "Main Factor Loan Recipients",
        titlefont = list(size = 14, family = "sans-serif"),  
        tickfont = list(size = 12, family = "sans-serif")    
      )
    )
  })
  
  
  # Render the description text for the Heatmap
  output$descriptionTextHeatmap <- renderUI({
    HTML("
    <p> The heatmap bar chart effectively illustrates the outstanding loan amounts across various industries, 
    providing a visual representation that is both informative and intuitive. By hovering over specific areas of the heatmap, 
    users can access detailed information, including the name of the industry, the growth rate of credit, the total outstanding amount 
    for that sector, and the GDP deflator for a given year. </p>
    
    <p> The color gradient in the visualization serves as a key indicator; lighter shades represent lower loan amounts, 
    while progressively darker shades signify higher amounts. Notably, the wholesale and retail industries stand out 
    with the highest outstanding loan amounts over the years, reflecting their significant role in the economy. </p>
    
    <p> Additionally, sectors such as [1] agriculture, hunting, and forestry, [2] manufacturing, and [3] various other industries exhibit 
    a positive trend in credit realization, suggesting a robust expansion in the business market. In contrast, the electricity, gas, and water 
    sectors have shown a concerning decline in credit realization over the same period, highlighting potential challenges within those industries.</p>
    ")
  })
  
  
  # Read the dataset for Financial Inclusion (sheet 3)
  df_financial_inclusion <- read_excel("Dataset_DVP.xlsx", sheet = "Financial Inclusion")
  
  # Populate the checkbox group input with regions
  observe({
    updateCheckboxGroupInput(session, "selected_regions", 
                             choices = unique(df_financial_inclusion$Region), 
                             selected = unique(df_financial_inclusion$Region))
  })
  
  # Render the animated buble chart 
  output$scatterPlot <- renderPlotly({
    
    # Ensure data is filtered based on selected regions
    filtered_data <- df_financial_inclusion %>%
      filter(Region %in% input$selected_regions)
    
    # Check if filtered_data is not empty
    if (nrow(filtered_data) == 0) {
      return(NULL)  
    }
    
    # Ensure Year is treated as a factor for animation frames
    filtered_data$Year <- as.factor(filtered_data$Year)
    
    # Use the RColorBrewer palette for the regions
    color_palette <- brewer.pal(6, "Set1")
    
    # Use plotly directly to create buble chart
    plot_ly(
      data = filtered_data,
      x = ~`Number of Digital Financial Service Agent (unit)`,
      y = ~`Number of Account Registered Money Electronic at Digital Financial Service Agent (Millions of Account)`,
      size = ~`Total Adults (Million People)`,
      color = ~Region,
      colors = color_palette,  # Apply RColorBrewer palette
      frame = ~Year,  # Animation by year
      text = ~paste(" Year:", Year, "<br>",
                    "Region:", Region, "<br>",
                    "Agents:", sprintf("%.2f", `Number of Digital Financial Service Agent (unit)`), "units<br>",
                    "Accounts:", sprintf("%.2f", `Number of Account Registered Money Electronic at Digital Financial Service Agent (Millions of Account)`), "million<br>",
                    "Adults:", sprintf("%.2f", `Total Adults (Million People)`), "million"),  
      hoverinfo = "text",  
      type = 'scatter',  
      mode = 'markers',  
      marker = list(sizemode = 'diameter', opacity = 0.6), 
      sizes = c(10, 60) ) %>%
      layout(
        title = "Financial Inclusion of Indonesia",
        xaxis = list(title = "Digital Financial Service Agents",
                     titlefont = list(size = 14, family = "sans-serif"), 
                     tickfont = list(size = 12, family = "sans-serif")),
        yaxis = list(title = "Registered Accounts \n (Millions)",
                     titlefont = list(size = 14, family = "sans-serif"), 
                     tickfont = list(size = 12, family = "sans-serif")  ),
        showlegend = TRUE,  
        autosize = TRUE,
        legend = list(
          orientation = "h",  
          x = 0.5,  
          y = -1.5,  
          xanchor = "center",
          yanchor = "bottom"
        ),
        margin = list(l = 50, r = 50, t = 50, b = 100)) %>%
      animation_opts(
        frame = 1000,  
        easing = "linear",
        redraw = TRUE) %>%
      animation_slider(
        currentvalue = list(prefix = "Year: ", font = list(size = 12)),
        step = 1  
      )
  })
  
  # Render the description text for the buble chart
  output$descriptionTextScatterPlot <- renderUI({ 
    HTML("
      <p> The visualization of financial inclusion in Indonesia is illustrated through an engaging bubble chart animation, 
      which provides a dynamic overview of the data. Users are invited to explore this interactive tool, where they can click the 
      play button to witness the evolution of financial inclusion over time. This growth is depicted through the interplay of two key factors: 
      the number of service agents and the total number of registered accounts. Each bubble in the chart is color-coded to represent 
      specific regions, while the size of the bubbles corresponds to the total adult population in those regions. This design allows for 
      a comprehensive understanding of how financial services reach different demographics across the country. Users can easily focus on particular 
      regions by selecting them from a checklist, and by hovering over any bubble, they can access detailed information about the number of accounts, 
      service agents, adults, the specific region, and the year of observation. </p>
      
      <p> From the visualization, it becomes evident that Java Island leads the way in financial inclusion. This dominance is directly related 
      to the high number of adults in the region who actively engage with financial services. In stark contrast, 
      Maluku and Papua Islands exhibit a marked lack of financial inclusion growth. This discrepancy can be attributed to their comparatively 
      smaller adult populations—underscoring the challenges these regions face in accessing financial services. </p>
      
      <p> Focusing on the temporal trends, the chart reveals that in 2019, the number of accounts registered for financial services reached its zenith, 
      only to experience a decline in the following year, likely a casualty of the COVID-19 pandemic. Yet, there is a notable rebound in 2021 
      that indicates an economic recovery, characterized by a diverse distribution of service agents and a renewed growth in registered accounts. 
      This upward trajectory reflects an overall improvement in financial access and utilization as the effects of the pandemic began to wane. </p>
         ")
  })
}

# Run the Shiny app ============================================================

shinyApp(ui = ui, server = server)
