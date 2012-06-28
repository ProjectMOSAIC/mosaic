#' Ages of Best Actor Oscar winners
#' 
#' The ages of Best Actor Oscar winners from 1970 to 2001 when they received the award.
#'  
#'  @docType data
#'  @name Actor
#'  @usage data(Actor)
#'  @format
#'      A data frame with 32 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Age}}{Ages of Best Actor Oscar winners from 1970 to 2001}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA
#' Average Gestation Period
#' 
#' The average gestation period, or time of pregnancy, of an animal is closely related to its longevity (the length of its lifespan.) Data on the average gestation period and longevity (in captivity) of 40 different species of animals have been examined, with the purpose of examining how the gestation period of an animal is related to (or can be predicted from) its longevity.
#'  
#'  @docType data
#'  @name Animals
#'  @usage data(Animals)
#'  @format
#'      A data frame with 40 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{animal}}{the name of the animal species}
#'     \item{\code{gestation}}{the average gestation period of the species (in days)}
#'     \item{\code{longevity}}{the average longevity of the species (in years)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Preferences for omputer Software
#' 
#' A local internet service provider (ISP) created two new versions of its software, with alternative ways of implementing a new feature. To find the product that would lead to the highest satisfaction among customers, the ISP conducted an experiment comparing user preference for the two new versions versus the existing software.  The ISP ideally wants to find out which of the three software products causes the highest user satisfaction. It has identified three major potential lurking variables that might affect user satisfaction: gender, age, and hours per week of computer use.
#'  
#'  @docType data
#'  @name Computers
#'  @usage data(Computers)
#'  @format
#'  A data frame with 20783 observations on the following variables.
#'    \itemize{
#'     \item{\code{age}}{age of computer user (in years)}
#'     \item{\code{gender}}{a factor with levels \code{female} or \code{male}}
#'     \item{\code{comp}}{weekly time of computer use (in hours)}
#'  }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Clinical Depression and Recurrence
#' 
#' In the study conducted by the National Institutes of Health, 109 clinically depressed patients were separated into three groups, and each group was given one of two active drugs (imipramine or lithium) or no drug at all. For each patient, the dataset contains the treatment used, the outcome of the treatment, and several other interesting characteristics.
#'  
#'  @docType data
#'  @name Depression
#'  @usage data(Depression)
#'  @format
#'      A data frame with 109 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Hospt}}{the hospital of patient, represented by a code for each of the 5 hospitals (1, 2, 3, 5, or 6)}
#'     \item{\code{Treat}}{the treatment received by the patient}
#'     \item{\code{Outcome}}{whether or not a recurrence occurred during the treatment of patient (0 = No Recurrence, 1 = Recurrence)}
#'     \item{\code{Time}}{either the time in days till the first recurrence, or if a recurrence did not occur, the length (in days) of the patient's participation in the study}
#'     \item{\code{AcuteT}}{the time (in days) that the patient was depressed prior to the study}
#'     \item{\code{Age}}{the age of the patient (in years), when the patient entered the study}
#'     \item{\code{Gender}}{the gender of patient (\code{Female} or \code{Male})}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @examples
#' favstats(Age ~ Gender, Depression)
#' tally(~ Outcome | Treat, Depression)
#' 
#' @keywords datasets

NA

#' Gender Concerns When Making Friends
#' 
#' The survey is designed to investigate if there is any gender concern when students make friends. A total of 1,200 U.S. college students were asked following question: "With whom do you find it easiest to make friends?" Possible responses were "No difference", "Opposite sex" and "Same sex".
#'  
#'  @docType data
#'  @name Friends
#'  @usage data(Friends)
#'  @format
#'      A data frame with 1200 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Friends}}{Answers to the question: "With whom do you find it easiest to make friends?" \code{No difference} \code{Opposite sex} \code{Same sex}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Graduation Rate
#' 
#' The percentage of each entering Freshman class that graduated on time was recorded for each of six colleges at a major university over eight years.
#'  
#'  @docType data
#'  @name Graduation
#'  @usage data(Graduation)
#'  @format
#'      A data frame with 8 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{College.A}}{graduation rate of College A}
#'     \item{\code{College.B}}{graduation rate of College B}
#'     \item{\code{College.C}}{graduation rate of College C}
#'     \item{\code{College.D}}{graduation rate of College D}
#'     \item{\code{College.E}}{graduation rate of College E}
#'     \item{\code{College.F}}{graduation rate of College F}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Height and Weight
#' 
#' The height and weight data collected from 57 males and 24 females for the purpose of exploring how the weight of a person is related to (or affected by) his or her height.
#'  
#'  @docType data
#'  @name Height
#'  @usage data(Height)
#'  @format
#'      A data frame with 81 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{female}}{0 = male, 1 = female}
#'     \item{\code{gender}}{a factor with levels \code{male} or \code{female}}
#'     \item{\code{height}}{subject height (in inches)}
#'     \item{\code{weight}}{subject weight (in pounds)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Night Lights and Nearsightedness
#' 
#' An Associated Press article captured the attention of readers with the headline "Night lights bad for kids?" The article was based on a 1999 study at the University of Pennsylvania and Children's Hospital of Philadelphia, in which parents were surveyed about the lighting conditions under which their children slept between birth and age 2 (lamp, night-light, or no light) and whether or not their children developed nearsightedness (myopia). The purpose of the study was to explore the effect of a young child's nighttime exposure to light on later nearsightedness.
#'  
#'  @docType data
#'  @name Nightlight
#'  @usage data(Nightlight)
#'  @format
#'      A data frame with 479 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Light}}{lighting conditions \code{no light} \code{night light} \code{lamp}}
#'     \item{\code{Nearsightedness}}{whether or not the subjects later became nearsighted \code{Yes} \code{No}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Winning Times for 1,500 Meter Race
#' 
#' The modern Olympic Games have changed dramatically since their inception in 1896. For example, many commentators have remarked on the change in the quality of athletic performances from year to year. Regression will allow us to investigate the change in winning times for one event: the 1,500 meter race.
#'  
#'  @docType data
#'  @name Olympics
#'  @usage data(Olympics)
#'  @format
#'   A data frame with 24 observations on the following variables.
#'    \itemize{
#'     \item{\code{Year}}{the year of the Olympic Games, from 1896 to 2000}
#'     \item{\code{Time}}{the winning time for the 1,500 meter race (in seconds)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Population Taking Introductory Statistics Courses
#' 
#' 1,129 students took introductory statistics courses for different purposes: business, social sciences, or natural sciences. The researcher were also interested in the values of four specific variables for the population: handedness (right-handed or left-handed), sex, SAT Verbal score, and age.
#'  
#'  @docType data
#'  @name Population
#'  @usage data(Population)
#'  @format
#'      A data frame with 1129 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Course}}{purpose of taking introductory statistics courses \code{natural science} \code{social science} \code{business}}
#'     \item{\code{Handed}}{handedness \code{right-handed} \code{left-handed}}
#'     \item{\code{Sex}}{a factor with levels \code{female} or \code{male}}
#'     \item{\code{Verbal}}{SAT verbal scores up to 800}
#'     \item{\code{Age}}{age (in years)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Instructor Ratings
#' 
#' At the end of a statistics course, the 27 students in the class were asked to rate the instructor on a number scale of 1 to 9 (1 being "very poor", and 9 being "best instructor I've ever had"). This dataset contains three hypothetical rating data.
#'  
#'  @docType data
#'  @name Ratings
#'  @usage data(Ratings)
#'  @format
#'      A data frame with 27 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{Class.I}}{hypothetical situation 1}
#'     \item{\code{Class.II}}{hypothetical situation 2}
#'     \item{\code{Class.III}}{hypothetical situation 3}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA
