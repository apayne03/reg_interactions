library(tidyverse)

my_data <- read.table(file = "lectureData6060.csv", header = TRUE, sep = ",", na.strings = c("NA"))
glimpse(my_data)



# 3 - INTERACTIONS AMONG CONTINUOUS VARIABLES

# 3.1 - Extract just the key columns and keep complete cases

analytic.data <- my_data %>% select (Exam, Anxiety, Preparation)

## keep complete cases only, list-wise deletion of cases...
analytic.data <- na.omit(analytic.data)



# 3.2 - Create mean centered versions of the variables

## add column with mean center anxiety...
analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety, center=T, scale=F)))

## add column with mean center preparation...
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric(scale(Preparation, center=T, scale=F)))



# 3.3 - Run the regression, notice how we create the product term

## In this anallysis we use x.centered*z.centered when specifiying the regression.
## This creates three predictors for the regression: x.centered, z.centered, and x.centered *z.centered

interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data = analytic.data, na.action = na.exclude)
summary(interaction.regression)

library(apaTables)
apa.reg.table(interaction.regression)

## Once you have established a significant interaction, you need to determine the pattern of results (i.e., the nature of the interaction).
## See next section for how to do that! :)



# 3.4 - Blocks approach (note that some people are more comfortable with the equivalent apprach below)

block1 <- lm(Exam ~ x.centered + z.centered, data = analytic.data, na.action=na.exclude)
block2 <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data = analytic.data, na.action = na.exclude)
apa.reg.table(block1, block2)

## They conduct two regression (one with and without the product term). They then compare the regressions.
## Notice the delta-RSQ is the same as the sr2 value from the above approach.



# 4 - INTERACTION AMONG CONTINUOUS VARIABLES: EXPLORING THE INTERACTION

## An interaction suggests that the relation b/w Anxiety (X) and Exam performance (Y) depends on the level of Preparation (Z).
## Thus, we try to get an indication of the relation b/w Anxiety and Exam performance at low Preparation (-1 SD Preparation).
## As well, we try to get an indication of the relation b/w Anxiety and Exam perform at high Preparation (+1 SD Preparation).
## The significant interaction suggests the slopes are different at these points.
## We want to see what each slope is and whether it is significant.
## That is, we want to know if there is a relation b/w Anxiety and Exam perform at each of these points. 
## Note that it is better to think in more general terms that low/high; the slope for Anxiety predicting Exam scores changes with the level of Preparation.


# 4.1 - Relation b/w Anxiety and Exam scores at High Preparation: Simple slope plus 1 SD Preparation

sd.z <- sd(analytic.data$z.centered, na.rm = TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
## this may seem counter-intuitive, but we lower the scores to increase the zero point to +1 SD...

simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD), data=analytic.data, na.action=na.exclude)
summary(simple.slope.plus.1SD)

## The summary output tells us the relation b/w Anxiety and Exam perform for individ that prepare extensively (i.e., those at 1 SD above the mean on preparation).
## B/c of the centering we can ignore the lines with z.centered.at.plus.1SD in this output (i.e., ignore slopes that involve Preparation).
## In this case at High Prep (+1 SD): Exam = -.34(Anxiety) + 64.71
## The non-significant weight for Anxiety indicates that there is no relation b/w Anxiety & Exam perform for High Prep participants.



# 4.2 - Relation b/w Anxiety and Exam scores at low Prep: Simple slope minus 1 SD Preparation

analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)
## this may seem counter-intuitive, but we raise the scores to decrease the zero point to -1 SD

simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD + I(x.centered*z.centered.at.minus.1SD), data = analytic.data, na.action=na.exclude)
summary(simple.slope.minus.1SD)
## Summmary output tells us the relation b/w Anxiety and Exam perform for individ that did not prepare extensively.
## B/c of the centering, we can ignore the slopes with z.centered.at.minus.1SD in this output.
## In this case at Low Prep (-1 SD): Exam = -3.97(Anxiety) + 46.45
## This significant weight for Anxiety indicates that there is a relation b/w Anxiety and Exam perform for low Prep participants.
## Specifically, for participants low in Prep, as Anxiety scores increase by 1, Exam perform decreases by 3.97 percent. 



# 5 - MAKE A 2D GRAPH (PART 1)

# 5.1 - Indicate range of scores on the X-axis for the two lines

## We pick Anxiety to be X...
sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

## We want the x-asis of the graph to range from -2 SD to +2 SD...
x.axis.range <- seq(-2*sd.x, 2*sd.x, by=.25*sd.x)



# 5.2 - Indicate the values of z that indicate high/low (preparation) for the two lines

## We pick Preparation to be Z...
## We want two lines - one representing high prep (+1 SD prep) & one representing low prep (-1 SD prep)
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi = 1*sd.z
z.line.lo = -1*sd.z



# 5.3 - Created predicted values for each line

## +1 SD Line...
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.hi)

## -1 SD Line...
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.lo)

## Put the information describing the lines into a data frame...
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)



# 6 - MAKE THE 2D GRAPH (PART 2)

library(ggplot2)

## set default (x,y) variables...
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

## make +1 SD Z line (b/c it is the one in the aes statement above)...
my.plot <- my.plot + geom_line(color="black", linetype="dotted", size=1.5)

## make the -1 SD Z line...
my.plot <- my.plot + geom_line(aes(x=x.axis.range, y=y.values.at.minus.1SD.z), color="black", linetype="solid", size=1.5)

## set APA part of graph below...
my.plot <- my.plot + theme_classic()
print(my.plot)


# 6.1 - Label the lines

my.plot <- my.plot + annotate("text", x = -1, y = 68.5, label = "+1 SD Preparation", angle=-3)
my.plot <- my.plot + annotate("text", x = -1, y = 43.5, label = "-1 SD Preparation", angle=-35)

print(my.plot)

## Note: If you add the annotations more than once you will need to run the plot part of the script again (i.e., everything after library(ggplot2))

## Try: using the angle argument to put these parallel to each line - for example, just add angle=-45 in the brackets.
## Try: including the regression equation in each annotation.



# 7 - TRY IT AGAIN: MAKE PREPARATION THE PREDICTOR AND ANXIETY THE MODERATOR

## The distinction b/w predictor and moderator is arbitrary.
## Therefore, run the above analyses again but make Preparation the predictor and Anxiety the moderator.
## What does the graph look like? How do you reconcile the two graphs?
## Go beyond the last graph - Create lines for -2SD, -1SD, +1SD and +2SD Anxiety.



# 8 - OTHER NON-LINEAR TERMS

# 8.1 - Inital regressions

interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered) + I(x.centered*x.centered) + I(z.centered*z.centered), data = analytic.data, na.action=na.exclude)
summary(interaction.regression)


# 8.2 - Simple slopes

## Activity: Figure out the two simple slope equations for this case.
## Hint: There will be more than one slope in each equation.


# 8.3 - Graph data

## Exactly the same as before...
sd.x <- sd(analytic.data$x.centered, na.rm=TRUE)

## we want the x-axis of the graph to range from -2 SD to +2 SD...
x.axis.range <- seq(-2*sd.x, 2*sd.x, by=.25*sd.x)

sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
z.line.hi = 1*sd.z
z.line.lo = -1*sd.z

## +1SD Line...
predictor.x.range.line.hi <- expand.grid(x.centered=x.axis.range, z.centered=z.line.hi)
y.values.at.plus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.hi)

## -1SD Line...
predictor.x.range.line.lo <- expand.grid(x.centered=x.axis.range, z.centered=z.line.lo)
y.values.at.minus.1SD.z <- predict(interaction.regression, newdata = predictor.x.range.line.lo)

# Put the information describing the lines into a data frame...
line.data <- data.frame(x.axis.range, y.values.at.plus.1SD.z, y.values.at.minus.1SD.z)


# 8.4 - Making the graph

## Exactly the same as before...
library(ggplot2)

## Set default (x,y) variables...
my.plot <- ggplot(line.data, aes(x=x.axis.range, y=y.values.at.plus.1SD.z))

## make +1 SD Z line (b/c it is the one in the aes statement above)...
my.plot <- my.plot + geom_line(color="black", linetype="dotted", size=1.5)

## make -1 SD Z line...
my.plot <- my.plot + geom_line(aes(x=x.axis.range, y=y.values.at.minus.1SD.z), color="black", linetype="solid", size=1.5)

## set APA part of the graph below...
my.plot <- my.plot + theme_classic()

print(my.plot)

## Notice how the lines are now curved to reflect the non-linear terms!




# 9 - MAKING A 3D GRAPH

# 9.1 - Look at the regression weights from the interaction regression

## Let's return to just looking at the X-Z product term...
interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data = analytic.data, na.action=na.exclude)

summary(interaction.regression)

## Notice the intercept and b-weights - Enter them into the intr.plot function...
library(MBESS)
intr.plot(b.0=55.5794, b.x=-2.1503, b.z=4.5648, b.xz=0.9077, x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z)


# 9.2 - Make the plot square by adding expand=1 (this increases the y-axis size to be proportional to the other)

intr.plot(b.0=55.5794, b.x=-2.1503, b.z=4.5648, b.xz=0.9077, x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, expand=1)


# 9.3 - Set the angle and make the plot grayscale

intr.plot(b.0=55.5794, b.x=-2.1503, b.z=4.5648, b.xz=0.9077, x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, expand=1, hor.angle=60, gray.scale=TRUE)


# 9.4 - Now add axis labels

intr.plot(b.0=55.5794, b.x=-2.1503, b.z=4.5648, b.xz=0.9077, x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, xlab="Anxiety Centered", zlab="Preparation Centered", ylab="Exam Score", expand=1, hor.angle=60, gray.scale=TRUE)


# 9.5 - Here is the tricky part

## You want to adjust the y-axis (i.e., vertical axis) for Exam to the range of grades (0 to 100).
## Unfortunately, in R, you need to refer to the y-axis (i.e., vertical axis) as the x-axis (even though this conflicts with the rest of the info we are using).
## In the line below, zlim=c(0,100) is actually setting the y-axis from 0 to 100. 
## Also note we increased the line width (line.wd).

intr.plot(b.0=55.5794, b.x=-2.1503, b.z=4.5648, b.xz=0.9077, x.min=-2*sd.x, x.max=2*sd.x, z.min=-2*sd.z, z.max=2*sd.z, xlab="Anxiety Centered", zlab="Preparation Centered", ylab="Exam Score", expand=1, hor.angle=60, gray.scale=TRUE, line.wd=4, zlim=c(0,100))




# 10 - IF YOU HAVE A MAC, THERE IS AN EASIER WAY!

## Use FastInteraction - see http://www.fastinteraction.com

## Warning: if you have missing data you need to save it in a particular way before using Fast Interaction.
## Save your data with R using the command below...
write_csv(x = analytic.data, path = "myDataOut.csv", na="")

## The na="" is essential - it ensures missing values are written in the file correctly.

## Load "myDataOut.csv" using Fast Interaction and quickly make the plot.



# 11 - MEDIATION

# 11.1 - Mediation if you insist on regression

## It's genereally not a good idea to do mediation analyses with regression unless you have a very large sample size (thousands of people).
## Furthermore, methods/stats researchers are starting to reject all techniques of this nature given the problems associated with them. 
## The current trend is away from establishing mediation via stats to establishing mediation via experimental interventions.

## Imagine we reconceptualize our data as representing mediation. 
## That is, Exam grade is still the DV, but we think that preparation reduces anxiety, which in turn increases Exam grade.
mediation.data <- analytic.data %>% select(Exam, Preparation, Anxiety)
psych::mediate(y=1, x=2, m=3, data = mediation.data)

## column numbers are 1, 2 and 3 for Exams, Prep & Anxiety respectively


# 11.2 - Mediation using Structural Equation Modelling

## Will learn in PSYC*6380, but can see handout for resources