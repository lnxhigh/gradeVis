source("helper.R")

function (input, output, session) {
    
    dataset <- reactive({
        
        inFile <- input$file
        req(inFile)
        
        return (read_xlsx(inFile$datapath))
    })
    
    output$view <- renderDataTable({
        if (input$check == "내 데이터") {
            return (dataset())
        }
        else return (read_xlsx("gradeTest.xlsx"))
    })
    
    output$plotA <- renderPlot({
        if (input$check == "내 데이터") return (plotA(dataset()))
        else return (plotA(read_xlsx("gradeTest.xlsx")))
    })
    
    output$plotB_r <- renderPlot({
        if (input$check == "내 데이터") return (plotB(dataset(), "r"))
        else return (plotB(read_xlsx("gradeTest.xlsx"), "r"))
    })
    
    output$plotB_n <- renderPlot({
        if (input$check == "내 데이터") return (plotB(dataset(), "n"))
        else return (plotB(read_xlsx("gradeTest.xlsx"), "n"))
    })
    
    output$plotB_b <- renderPlot({
        if (input$check == "내 데이터") return (plotB(dataset(), "b"))
        else return (plotB(read_xlsx("gradeTest.xlsx"), "b"))
    })
    
    output$plotC_ABCDF <- renderPlot({
        if (input$check == "내 데이터") return (plotC(dataset(), "ABCDF"))
        else return (plotC(read_xlsx("gradeTest.xlsx"), "ABCDF"))
    })
    
    output$plotC_PF <- renderPlot({
        if (input$check == "내 데이터") return (plotC(dataset(), "PF"))
        else return (plotC(read_xlsx("gradeTest.xlsx"), "PF"))
    })
    
    output$plotD_ts <- renderPlot({
        if (input$check == "내 데이터") return (plotD(dataset(), "ts"))
        else return (plotD(read_xlsx("gradeTest.xlsx"), "ts"))
    })
    
    output$plotD_bp <- renderPlot({
        if (input$check == "내 데이터") return (plotD(dataset(), "bp"))
        else return (plotD(read_xlsx("gradeTest.xlsx"), "bp"))
    })
}
