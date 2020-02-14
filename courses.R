

library(tidyverse)
library(readxl)
library(here)


####  Load data  ------


# Both files from https://www.cde.ca.gov/ds/sd/df/filesassign.asp
assignments <- read_xlsx(here("data", "AssignmentCodes12On.xlsx"))
coursestaught <- read_tsv(here("data", "CoursesTaught17.txt"), col_types = "ccccccccccccccccccc" ) %>%
    filter(AcademicYear == "1")

for (i in 12:18) {
    year <- read_tsv(here("data", paste0("CoursesTaught",i,".txt")), col_types = "ccccccccccccccccccc"  )
    coursestaught <- bind_rows(coursestaught, year)
}

coursestaught <- coursestaught %>%
    filter(str_detect(CountyName,"MONTEREY")) %>%
    left_join(assignments, by = c("CourseCode" = "AssignmentCode"))


# cs.courses <- assignments %>%
#     filter(str_detect(AssignmentSubject, "Computer"))


cs.courses <- read_xlsx(here("data", "Rodlist.xlsx"), col_names = FALSE) %>%
    transmute(AssignmentCode = `...1`,
              CourseName = `...2`)

art <- assignments %>% 
    filter(str_detect(AssignmentSubject,"Art|Dance|Drama|Fashion|Music") | AssignmentCode %in% c(4605, 4606,4607,4635,2455)  ) %>%
    filter(!str_detect(AssignmentSubject,"English"))



# art programs, theater/dance, music, dance, fashion and interior design, arts media and entertainment as well as 
# 4605, 4606,4607,4617, 4635 & 2455



coursestaught.mry <- coursestaught %>% 
    filter(str_detect(CountyName,"MONTEREY"),
           CourseCode %in% art$AssignmentCode,
           AcademicYear == "1819") %>%
#    mutate(CourseCode = as.character(CourseCode)) %>%
    left_join(assignments, by = c("CourseCode" = "AssignmentCode"))


# write_csv(coursestaught.mry, "CS Courses in 17-18.csv")



summary <- coursestaught.mry %>%
    group_by(DistrictName, SchoolName, AcademicYear) %>%
    mutate(NumberEnrolled = sum(as.numeric(Enrollment)) ,
           NumberCourses = n()) %>%
    select(DistrictName, SchoolName, AcademicYear, NumberEnrolled, NumberCourses) %>%
    distinct() %>%
    arrange(DistrictName, AcademicYear)

write_csv(summary, "Art Courses by District by Year.csv")

# 
# summary <- coursestaught.mry %>%
#     group_by(DistrictName,AcademicYear) %>%
#     count()
# 




courses.summary <- function(courselist) {
    
    coursestaught %>% 
        filter(CourseCode %in% courselist)  %>%
        group_by(DistrictName, SchoolName, AcademicYear) %>%
        mutate(NumberEnrolled = sum(as.numeric(Enrollment)) ,
               NumberCourses = n()) %>%
        select(DistrictName, SchoolName, AcademicYear, NumberEnrolled, NumberCourses) %>%
        distinct() %>%
        arrange(DistrictName, SchoolName ,AcademicYear)
}

courses.summary(art$AssignmentCode)
