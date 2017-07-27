
#' @include main.R
#' @include geomTimeline.R
#' @include geomTimelineLabel.R
#' @importFrom dplyr mutate filter
testit <- function ()
{
cleanedNOA<-eq_location_clean(eq_clean_data(readRawNOAA(file.path("~","signif.txt"))))

italy<-cleanedNOA %>% arrange(DEATHS )%>% filter(COUNTRY=="ITALY")
mydata<-cleanedNOA %>% arrange(DEATHS )%>% filter(COUNTRY=="USA" | COUNTRY=="CHINA" )

mydata %>%
ggplot() +
    aes(
      x = date,
      y= COUNTRY,
      size = EQ_PRIMARY,
      colour =     DEATHS,
      label = LOCATION_NAME
    ) +
    geom_timeline(alpha=0.4,xmin=2000,xmax=2015) +

  geom_timeline_label(xmin=2000,xmax=2017, n_max = 3, angle=30, aes(by=EQ_PRIMARY),colour="gray")+
  geom_timeline_label(xmin=2000,xmax=2017,n_max = 3, angle=60, aes(by=DEATHS),colour="red")+
  scale_colour_gradient(name = "# DEATHS", low = "black",high = "red") +
  scale_size_continuous(name = "Richter scale value") +
  theme(legend.position="bottom")
}


