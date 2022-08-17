# gradeVis
Visualize your grade using R shiny package

고려대학교 포털사이트에서 학점 정보를 가져와 입력하면 관련된 성적 통계를 보여 주는 사이트입니다.
2019년 12월에 R언어의 shiny 패키지를 사용하여 제작되었습니다.

R에서 다음을 실행하면 웹사이트를 볼 수 있습니다.
library(shiny)
runGitHub(repo = "gradeVis", username = "lnxhigh", ref = "main")

또는 파일을 다운받아서 다음을 실행해도 됩니다.
library(shiny)
runApp()

구성은 다음과 같습니다.
ui.R
server.R
global.R - 내부 로직에 사용되는 함수들을 구현해 놓은 파일입니다.
helper.R - 의존적인 패키지들을 모아 놓은 파일입니다.
www - 웹페이지에 띄울 이미지를 모아 놓은 폴더입니다.
gradeTest.xlsx - 테스트용 파일입니다.
txt 파일들 - 페이지에 대한 설명을 미리 텍스트 파일로 작성해 놓고 이를 R 파일에서 읽어들여 사용했습니다.

ui는 네비게이션 바로 되어 있으며, 소개, 데이터 입력, 각종 플롯들을 띄우는 부분으로 구성되어 있습니다.