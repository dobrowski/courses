

library(tidyverse)
library(readxl)
library(here)


####  Load data  ------


# Both files from https://www.cde.ca.gov/ds/sd/df/filesassign.asp
assignments <- read_xlsx(here("data", "AssignmentCodes12On.xlsx"))
coursestaught <- read_tsv(here("data", "CoursesTaught17.txt"), col_types = "ccccccccccccccccccc" ) %>%
    filter(AcademicYear == "1")

for (i in 12:17) {
    year <- read_tsv(here("data", paste0("CoursesTaught",i,".txt")), col_types = "ccccccccccccccccccc"  )
    coursestaught <- bind_rows(coursestaught, year)
}



# cs.courses <- assignments %>%
#     filter(str_detect(AssignmentSubject, "Computer"))


cs.courses <- read_xlsx(here("data", "Rodlist.xlsx"), col_names = FALSE) %>%
    transmute(AssignmentCode = `...1`,
              CourseName = `...2`)



coursestaught.mry <- coursestaught %>% 
    filter(str_detect(CountyName,"MONTEREY"),
           CourseCode %in% cs.courses$AssignmentCode) %>%
#    mutate(CourseCode = as.character(CourseCode)) %>%
    left_join(assignments, by = c("CourseCode" = "AssignmentCode"))


# write_csv(coursestaught.mry, "CS Courses in 17-18.csv")



summary <- coursestaught.mry %>%
    group_by(DistrictName,AcademicYear) %>%
    mutate(enrolled = sum(as.numeric(Enrollment)) ,
           n = n()) %>%
    select(DistrictName, AcademicYear, enrolled, n) %>%
    distinct() %>%
    arrange(DistrictName, AcademicYear)

write_csv(summary, "CS Courses by District by Year.csv")

# 
# summary <- coursestaught.mry %>%
#     group_by(DistrictName,AcademicYear) %>%
#     count()
# 






library(ipumsr)
ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)
