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
#' @keywords datasets

NA

#' Baby Cry Count and IQ
#' 
#' A method for predicting IQ as soon as possible after birth could be important for early intervention in cases such as brain abnormalities or learning disabilities. It has been thought that greater infant vocalization (for instance, more crying) is associated with higher IQ. In 1964, a study was undertaken to see if IQ at 3 years of age is associated with amount of crying at newborn age. In the study, 38 newborns were made to cry after being tapped on the foot, and the number of distinct cry vocalizations within 20 seconds was counted. The subjects were followed up at 3 years of age and their IQs were measured.
#'  
#'  @docType data
#'  @name Baby
#'  @usage data(Baby)
#'  @format
#'      A data frame with 38 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{cry.count}}{the number of distinct cry vocalizations within 20 seconds}
#'     \item{\code{IQ}}{IQ at 3 years of age}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Birth Weights of Newborns
#' 
#' Some studies suggest that women having their first baby at age 35 or older are at increased risk of having a baby with a low birth weight. A medical researcher wanted to estimate the mean weight back in the population of newborns who are the first child for women over the age of 35. To this end, the researcher chose a random sample of 125 women age 35 or older who were pregnant with their first child and followed them through the pregnancy. The datafile linked below contains the birth weight (in grams) of the 125 newborns (women pregnant with more than one child were excluded from the study). 
#'  @docType data
#'  @name Birthweight
#'  @usage data(Birthweight)
#'  @format
#'      A data frame with 125 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{birthweight}}{weights of newborns (in grams)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Preferences for Computer Software
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

#'  Campus Drinking Rates
#' 
#' A group of 75 college students were randomly sampled and asked about the number of alcoholic drinks they have in a typical week. The purpose of this study was to compare the drinking habits of the students at the college to the drinking habits of college students in general. In particular, the dean of students, who initiated this study, would like to check whether the mean number of alcoholic drinks that students at his college have in a typical week differs from the mean of U.S. college students in general, which is estimated to be 4.73.
#'  
#'  @docType data
#'  @name Drinks
#'  @usage data(Drinks)
#'  @format
#'      A data frame with 75 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{drinks.per.week}}{number of alcoholic drinks (standard units) per week}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' EPA Fuel Economy
#' 
#' Every year, the Environmental Protection Agency (EPA) collects data on fuel economy. With rising gasoline prices, consumers are using these figures as they decide which automobile to purchase. We will look at two-seater automobiles, many of which are sporty vehicles. The 2007 EPA sample is used to test the hypothesis that the combined city and highway miles per gallon (mpg) of two-seater automobiles is greater than 20. The standard deviation for all vehicles is 4.7 mpg.
#'  
#'  @docType data
#'  @name EPA
#'  @usage data(EPA)
#'  @format
#'      A data frame with 71 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{CLASS}}{automobile type \code{TWO SEATERS}}
#'     \item{\code{MFR}}{automobile brand: one of \code{MERCEDES=BENZ} \code{PORSCHE} \code{BMW} \code{LAMBORGHINI} \code{CHRYSLER} \code{FERRARI} \code{ASTON MARTIN} \code{CADILLAC} \code{CHEVROLET} \code{HONDA} \code{LOTUS} \code{MAZDA} \code{NISSAN} \code{PONTIAC} \code{SATURN} \code{SPYKR}}
#'     \item{\code{CAR.LINE}}{}
#'     \item{\code{DISPLACEMENT}}{displacement of the engine}
#'     \item{\code{NUMB.CYL}}{number of cylinders}
#'     \item{\code{TRANS}}{transmission type}
#'     \item{\code{DRIVE.SYS}}{4 = , R = }
#'     \item{\code{CITY.MPG}}{city mileage (in miles per gallon)}
#'     \item{\code{HWY.MPG}}{highway mileage (in miles per gallon)}
#'     \item{\code{COMB.MPG}}{combined city and highway (in miles per gallon)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Critical Flicker Frequency (CFF) and Eye Color
#' 
#' There is various flickering light in our environment; for instance, light from computer screens and fluorescent bulbs. If the frequency of the flicker is below a certain threshold, the flicker can be detected by the eye. Different people have slightly different flicker "threshold" frequencies (known as the "critical flicker frequency," or CFF). Knowing the critical threshold frequency below which flicker is detected can be important for product manufacturing as well as tests for ocular disease. Do people with different eye color have different threshold flicker sensitivity? A 1973 study ("The effect of iris color on critical flicker frequency," Journal of General Psychology [1973], 91-95) obtained the following data from a random sample of subjects.
#'  
#'  @docType data
#'  @name Flicker
#'  @usage data(Flicker)
#'  @format
#'      A data frame with 19 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{color}}{eye color: \code{Brown}, \code{Green}, or \code{Blue}}
#'     \item{\code{cff}}{Critical Flicker Frequency}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
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
#'      A data frame with 48 observations on the following variables.
#'    
#'    \itemize{
#'     \item{ontime}{percentage of students that graduated on time}
#'     \item{\code{college}}{a factor with levels \code{A} through \code{F}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Ban on Assault Weapons
#' 
#' The U.S. federal ban on assault weapons expired in September 2004, which meant that after 10 years (since the ban was instituted in 1994) there were certain types of guns that could be manufactured legally again. A poll asked a random sample of 1,200 eligible voters (among other questions) whether they were satisfied with the fact that the law had expired. The data contains the results of this poll (Data were generated based on a poll conducted by NBC News/Wall Street Journal Poll). 
#'  @docType data
#'  @name Guns
#'  @usage data(Guns)
#'  @format
#'      A data frame with 1200 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{opinion}}{whether or not satisfied with the expiration of the ban on assault weapons: \code{satisfied} or \code{not satisfied}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
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

#' Length of Pregnancy
#' 
#' The length of human pregnancy is known to have a mean of 266 days and a standard deviation of 16 days. Based on records from a large women's hospital, a random sample of 25 women who were smoking and/or drinking alcohol during their pregnancy and their pregnancy lengths are recorded. 
#'  
#'  @docType data
#'  @name Pregnancy
#'  @usage data(Pregnancy)
#'  @format
#'      A data frame with 25 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{length} }{the length of pregnancy (in days) of women who were smoking and/or drinking alcohol during their pregnancy}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Instructor Ratings
#' 
#' At the end of a statistics course, the 27 students in the class were asked to rate the instructor on a number scale of 1 to 9 (1 being "very poor", and 9 being "best instructor I've ever had"). This dataset contains three sets of hypothetical rating data.
#'  
#'  @docType data
#'  @name Ratings
#'  @usage data(Ratings)
#'  @format
#'      A data frame with 81 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{score}}{score on the evaluation scale}
#'     \item{\code{situation}}{a factor with levels \code{Class I}, \code{Class II} or \code{Class III}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' 
#' @keywords datasets

NA

#' Gosset's Experiment on Regular and Kiln-Dried Seeds
#' 
#' William S. Gosset was employed by the Guinness brewing company of Dublin. Sample sizes available for experimentation in brewing were necessarily small, and new techniques for handling the resulting data were needed. Gosset consulted Karl Pearson (1857-1936) of University College in London, who told him that the current state of knowledge was unsatisfactory. Gosset undertook a course of study under Pearson and the outcome of his study was perhaps the most famous paper in statistical literature, "The Probable Error of a Mean" (1908), which introduced the t distribution. Since Gosset was contractually bound by Guinness, he published under a pseudonym, "Student," hence the t distribution is often referred to as Student's t distribution.
#' 
#' As an example to illustrate his analysis, Gosset reported in his paper on the results of seeding 11 different plots of land with two different types of seed: "regular" and "kiln-dried". There is reason to believe that drying seeds before planting will increase plant yield. Since different plots of soil may be naturally more fertile, this confounding variable was eliminated by using the matched pairs design and planting both types of seed in all 11 plots.
#' 
#' The hypothesis is that kiln-dried seed yields more corn than regular seed.
#'  
#'  @docType data
#'  @name Seed
#'  @usage data(Seed)
#'  @format
#'      A data frame with 11 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{regular.seed}}{the corn yield (in pounds per acre) planting regular seed}
#'     \item{\code{kiln.dried.seed}}{the corn yield (in pounds per acre) planting kiln-dried seed}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Sleeping Time
#' 
#' As part of a large survey conducted at a large state university, a random sample of 142 students were asked: "How many hours do you sleep in a typical day?" 
#'  
#'  @docType data
#'  @name Sleep
#'  @usage data(Sleep)
#'  @format
#'      A data frame with 142 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{sleep}}{sleeping time in a typical day (in hours)}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Sleeping Time of Undergraduate and Graduate Students
#' 
#' A study was conducted at a large state university in order to compare the sleeping habits of undergraduate students to those of graduate students. Random samples of 75 undergraduate students and 50 graduate students were chosen and each of the subjects was asked to report the number of hours he or she sleeps in a typical day. The hypothesis is that since undergraduate students are generally younger and party more during their years in school, they sleep less, on average, than graduate students.
#'  
#'  @docType data
#'  @name Sleep2
#'  @usage data(Sleep2)
#'  @format
#'      A data frame with 75 rows on the following variables.
#'    
#'    \itemize{
#'     \item{\code{undergraduate}}{number of hours that an undergraduate sleeps in a typical day)}
#'     \item{\code{graduate}}{number of hours that a graduate sleeps in a typical day}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' TV Time 1
#' 
#' A researcher wanted to study whether or not men and women differ in the amount of time they watch TV during a week. A random sample of 40 adults was chosen (22 of whom were women and 18 of whom were men). At the end of the week, each of the 40 subjects reported the total amount of time (in minutes) that he or she watched TV during that week.
#'  
#'  @docType data
#'  @name TV1
#'  @usage data(TV1)
#'  @format
#'      A data frame with 40 rows on the following variables.
#'    
#'    \itemize{
#'     \item{\code{time}}{amount of time (in minutes) that an individual watched TV during a week}
#'     \item{\code{gender} }{\code{Female} or \code{Male}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' TV Time 2
#' 
#' A researcher wanted to study whether or not men and women differ in the amount of time they watch TV during a week. A random sample of 400 adults was chosen (191 of whom were women and 209 of whom were men). At the end of the week, each of the 400 subjects reported the total amount of time (in minutes) that he or she watched TV during that week.
#'  
#'  @docType data
#'  @name TV2
#'  @usage data(TV2)
#'  @format
#'      A data frame with 400 rows on the following variables.
#'    
#'    \itemize{
#'     \item{\code{time}}{amount of time (in minutes) that an individual watched TV during a week}
#'     \item{\code{gender} }{\code{Female} or \code{Male}}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA

#' Exercise Habits of Male Students
#' 
#' A health educator at a small college wants to determine whether the exercise habits of male students in the college are similar to the exercise habits of male college students in general. The educator chooses a random sample of 20 male students and records the time they spend exercising in a typical week.
#'  
#'  @docType data
#'  @name Time
#'  @usage data(Time)
#'  @format
#'      A data frame with 20 observations on the following variables.
#'    
#'    \itemize{
#'     \item{\code{time1}}{time (in hours) spending on exercises in a typical week, sample 1 of size 20}
#'     \item{\code{time2}}{time (in hours) spending on exercises in a typical week, sample 2 of size 20}
#'     \item{\code{time3}}{time (in hours) spending on exercises in a typical week, sample 3 of size 20}
#'     \item{\code{time4}}{time (in hours) spending on exercises in a typical week, sample 4 of size 20}
#'   }
#' 
#' @references
#' Part of the Carnegie Mellon University Online Learning Initiative datasets.
#' 
#' @keywords datasets

NA
