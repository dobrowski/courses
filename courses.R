

library(tidyverse)
library(readxl)
library(here)


####  Load data  ------


# Both files from https://www.cde.ca.gov/ds/sd/df/filesassign.asp
assignments <- read_xlsx(here("data", "AssignmentCodes12On.xlsx"))
coursestaught <- read_tsv(here("data", "CoursesTaught17.txt"))



cs.courses <- assignments %>%
    filter(str_detect(AssignmentSubject, "Computer"))

coursestaught.mry <- coursestaught %>% 
    filter(str_detect(CountyName,"MONTEREY"),
           CourseCode %in% cs.courses$AssignmentCode) %>%
    mutate(CourseCode = as.character(CourseCode)) %>%
    left_join(assignments, by = c("CourseCode" = "AssignmentCode"))


write_csv(coursestaught.mry, "CS Courses in 17-18.csv")
