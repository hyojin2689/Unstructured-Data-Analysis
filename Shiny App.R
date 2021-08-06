ui <- fluidPage(
  titlePanel(title="Walking data of user1 in Mobile phone sensor data."),
  sidebarPanel( #화면분할
    sidebarPanel( textInput(inputId = 'vars',
                            label='variables',
                            value='acceleration')), #문자열 입력
    selectInput('x','x',choices=c('maguserAcceleration','maggravity','magrotationRate'),selected='maguserAcceleration'),
    selectInput('y','Y',choices="time",selected='time'), #입력데이터 선택
    submitButton(text='Apply the changes',icon=icon(name='update')), #제출버튼생성
    helpText('Press the button above to update your changes.')), #도움말
  mainPanel(
    uiOutput(outputId = 'mainUI'), 
    textOutput(outputId = 'activity'),#일반문자열로출력
    tabsetPanel( #출력탭을 여러개의 탭으로 나누기
      type='tabs',
      tabPanel(h5('walking by var'),title='plot',plotOutput(outputId='line1')), #그래프출력
      tabPanel(h5('user1_walking_total'),title='data table',DTOutput(outputId='user1')) #테이블 출력
    )
  )
)

server <- function(input,output,session) {
  output$user1=renderDT( #데이터테이블
    user1_walking_total,options=list(lengthChange=FALSE)
  )
  output$activity <-renderPrint({ #출력될 문자열 생성
    str_c('Work data for user1 according to ', input$vars, sep='')
  })
  output$line1<-renderPlot({ #출력할 그래프 생성
    ggplot(user1_walking_total,aes_string(x=input$y,y=input$x))+geom_line()+
      facet_wrap(.~exp_no,ncol=1)
  })
}

shinyApp(ui=ui, server=server)
