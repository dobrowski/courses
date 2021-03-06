

library(tidyverse)
library(readxl)
library(here)
library(vroom)
library(fs)


####  Load data  ------


# Both files from https://www.cde.ca.gov/ds/sd/df/filesassign.asp
assignments <- read_xlsx(here("data", "AssignmentCodes12On.xlsx")) 

coursestaught <- dir_ls( glob = "*/Course*txt", recurse = TRUE) %>%
    vroom() %>% 
    filter(str_detect(CountyName,"MONTEREY")) %>%
    mutate(CourseCode = as.character(CourseCode)) %>%
    left_join(assignments, by = c("CourseCode" = "AssignmentCode"))


### Examples of course lists -----


cs.courses <- read_xlsx(here("data", "Rodlist.xlsx"), col_names = FALSE) %>%
    transmute(AssignmentCode = `...1`,
              CourseName = `...2`)


# Do you have data sources that will allow you to look up the number of schools in MCOE that are providing arts programming?
# Let's see all of the schools that have art programs, theater/dance, music, dance, fashion and interior design, arts media and entertainment as well as 
# 4605, 4606,4607,4617, 4635 & 2455

art <- assignments %>% 
    filter(str_detect(AssignmentSubject,"Art|Dance|Drama|Fashion|Music") | AssignmentCode %in% c(4605, 4606,4607,4635,2455)  ) %>%
    filter(!str_detect(AssignmentSubject,"English"))

### Function -----

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


###  Example execution -----

courses.summary(art$AssignmentCode)

write_csv(summary, "Art Courses by District by Year.csv")


### End ------