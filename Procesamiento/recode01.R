
# install.packages("readr") # you only need to do this one time on your system
library(readr)
library(stringr)
mystring <- read_file("Encuesta_factorial_piloto_txt.txt")



recode01 <- c("vig001"="11","vig002"="12","vig003"="13","vig004"="14","vig005"="15","vig006"="16","vig007"="17","vig008"="18","vig009"="19","vig010"="110","vig011"="111","vig012"="112","vig013"="21","vig014"="22","vig015"="23","vig016"="24","vig017"="25","vig018"="26","vig019"="27","vig020"="28","vig021"="29","vig022"="210","vig023"="211","vig024"="212","vig025"="31","vig026"="32","vig027"="33","vig028"="34","vig029"="35","vig030"="36","vig031"="37","vig032"="38","vig033"="39","vig034"="310","vig035"="311","vig036"="312","vig037"="41","vig038"="42","vig039"="43","vig040"="44","vig041"="45","vig042"="46","vig043"="47","vig044"="48","vig045"="49","vig046"="410","vig047"="411","vig048"="412","vig049"="51","vig050"="52","vig051"="53","vig052"="54","vig053"="55","vig054"="56","vig055"="57","vig056"="58","vig057"="59","vig058"="510","vig059"="511","vig060"="512","vig061"="61","vig062"="62","vig063"="63","vig064"="64","vig065"="65","vig066"="66","vig067"="67","vig068"="68","vig069"="69","vig070"="610","vig071"="611","vig072"="612","vig073"="71","vig074"="72","vig075"="73","vig076"="74","vig077"="75","vig078"="76","vig079"="77","vig080"="78","vig081"="79","vig082"="710","vig083"="711","vig084"="712","vig085"="81","vig086"="82","vig087"="83","vig088"="84","vig089"="85","vig090"="86","vig091"="87","vig092"="88","vig093"="89","vig094"="810","vig095"="811","vig096"="812","vig097"="91","vig098"="92","vig099"="93","vig100"="94","vig101"="95","vig102"="96","vig103"="97","vig104"="98","vig105"="99","vig106"="910","vig107"="911","vig108"="912","vig109"="101","vig110"="102","vig111"="103","vig112"="104","vig113"="105","vig114"="106","vig115"="107","vig116"="108","vig117"="109","vig118"="1010","vig119"="1011","vig120"="1012")

dat01 <- NA
dat01 <- as.data.frame(dat01)
dat01$question <- as.character(mystring) 
dat01$question2<- str_replace_all(dat01$question, recode01)

quest<- dat01$question2

cat(quest,file="factorial_mod-v01.qsf",sep = "")

