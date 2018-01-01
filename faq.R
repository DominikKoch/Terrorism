bsCollapse(id = "faq",
           
  bsCollapsePanel("Where does the data come from?",
    p("The data is crowdsourced by Wikipedia. You can find ", 
      a("Lists of terrorist incidents", href = "https://en.wikipedia.org/wiki/List_of_terrorist_incidents#1970%E2%80%93present", target = "_blank"),  
      "from 1970 to the present. I'm not using the open-source",
      a("Global Terrorism Database (GTD)", href = "https://www.start.umd.edu/gtd/", target = "_blank"),  
      "because this data is one year delayed and I want to include the most recent incidents.",
      style = "text-align: justify;"),
    style = "info"),
  
  bsCollapsePanel("How many times is the data updated?",
    p("At the moment this is still a manual process due to occasionally messy Wikipedia data. 
      Therefore the data will be updated on a regular basis once per month.
      I might implement a real time data supply in the future."),
    style = "info"),
  
  bsCollapsePanel("How did you make this dashboard?",
    p("This dashboard is written in", 
      a("R", href = "https://www.r-project.org/", target = "_blank"), "and built with the", 
      a("Shiny", href = "https://shiny.rstudio.com/", target = "_blank"), "web applications frame work. 
      Shiny is an open source R package that provides an elegant and powerful web framework for building web applications without requiring HTML, CSS, or JavaScript knowledge.", 
      style = "text-align: justify;"), 
    style = "info")
)