navbarPage("gradeVis",
    tabPanel(
        title = "Welcome",
        
        HTML(
            readr::read_file("introduction.txt")
        )
    ),
    
    tabPanel(
        title = "Data",
        
        HTML(
            readr::read_file("Data.txt")
        ),
        
        verticalLayout(
            div(style = "background-color: whitesmoke; padding: 20px; margin-bottom: 20px;",
                radioButtons(
                    "check", 
                    label = "데이터를 선택해 주세요.",
                    choices = c('샘플', '내 데이터'),
                    selected = "샘플"
                ),
                
                conditionalPanel(
                    "input.check == '내 데이터'",
                    fileInput(
                        "file",
                        label = "파일을 선택하세요.",
                        accept = ".xlsx"
                    )
                )   
            ),
            
            dataTableOutput("view")
        )
    ),
    
    tabPanel(
        title = "Plot A",
        
        HTML(
            readr::read_file("plotA.txt")
        ),
        
        mainPanel(
            plotOutput("plotA")
        )
    ),
    
    tabPanel(
        "Plot B",
        
        HTML(
            readr::read_file("plotB.txt")
        ),
        
        mainPanel(
            plotOutput("plotB_r"), 
            plotOutput("plotB_n"), 
            plotOutput("plotB_b")
        )
    ),
    
    tabPanel(
        "Plot C",
        
        HTML(
            readr::read_file("plotC.txt")
        ),
        
        mainPanel(
            plotOutput("plotC_ABCDF"),
            plotOutput("plotC_PF")
        )
    ),
    
    tabPanel(
        "plot D",
        
        HTML(
            readr::read_file("plotD.txt")
        ),
        
        mainPanel(
            plotOutput("plotD_ts"),
            plotOutput("plotD_bp")
        )
        
    )
)