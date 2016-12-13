### R code from vignette source 'TreatmentPatterns.Rnw'

###################################################
### code chunk number 1: Init
###################################################
library(rms)
library(VizOR)


###################################################
### code chunk number 2: PrepareData
###################################################
##
## Read and join 'demographics' and 'onset' monitor data tables,
## forming a 'baseline characteristics' table.
##
demographics <- read.table(system.file("extdata", "Patient_Demographics.tab", package="VizOR"),
                           header=TRUE,
                           colClasses=c(
                               hx.hemo="logical"
                               ,hx.anticoag="logical"
                               ,hx.afib="logical"
                               ,hx.cva="logical"
                               ,hx.cad="logical"
                               ,hx.csten="logical"
                               ,hx.pvd="logical"
                               ,hx.dm="logical"
                               ,hx.smok="logical"
                               ,hx.dyslip="logical"
                               ,hx.ivtpa="logical"
                               )
                           )
onset <- read.table(system.file("extdata", "Stroke_Onset.tab", package="VizOR"), header=TRUE)
bsl <- merge(demographics, onset, all.x=TRUE)

##
## Read various events tables, and assemble them in Entity-Attribute-Value
## (EAV) form for use by 'timelines'.
##
telestroke.req <- read.table(system.file("extdata", "TeleStrokeREQ.tab", package="VizOR"), header=TRUE)
arrival <- read.table(system.file("extdata", "Arrive.tab", package="VizOR"), header=TRUE)
telestroke <- read.table(system.file("extdata", "TeleStroke.tab", package="VizOR"), header=TRUE)
decision <- read.table(system.file("extdata", "Decision.tab", package="VizOR"), header=TRUE)
infusion <- rbind(read.table(system.file("extdata", "OSH_Infusion_Start.tab", package="VizOR"), header=TRUE),
                  read.table(system.file("extdata", "CSC_Infusion_Start.tab", package="VizOR"), header=TRUE))
## To prevent hangups, let's make the input data frames quite small:
bsl <- subset(bsl, ptid < 200)
bsl$patno <- as.factor(paste("P", bsl$ptid, sep=""))

df <- bsl
df <- merge(df, arrival, all.x=TRUE)
df <- merge(df, telestroke.req, all.x=TRUE)
df <- merge(df, telestroke, all.x=TRUE)
df <- merge(df, decision, all.x=TRUE)
df <- merge(df, infusion, all.x=TRUE)

## Ordered vector of reasons why tPA was not given
reasons <- c(Anticoag="INR"
             ,Give_tPA=NA
             ,HxHemo="History of hemorrhage"
             ,Other="Other"
             ,Over3h="Onset > 3 hours ago"
             ,RISS="Mild/RISS"
             ,TooSevere="Severe stroke")

df <- upData(df
             ## generate several key process measures
             ,ott = tPA.start - onset.time
             ,treated = !is.na(tPA.start)
             ## reference all acute stroke care event times to onset
             ,vs.t = vs.t - onset.time
             ,hx.t = hx.t - onset.time
             ,ecg.t = ecg.t - onset.time
             ,blood.t = blood.t - onset.time
             ,ct.t = ct.t - onset.time
             ,req.t = req.t - onset.time
             ,login.t = login.t - onset.time
             ,cs.start = cs.start - onset.time
             ,cs.end = cs.end - onset.time
             ,decision.t = decision.t - onset.time
             ,tPA.start = tPA.start - onset.time
             ## reference arrival to onset
             ,arrival.t = arrival.t - onset.time
             ## generate several non-negative sub-process delays
             ,login.delay = login.t - req.t
             ## generate factors for conditioning various graphics
             ,telestroke.requestor = ifelse(is.na(req.by),"CSC",
                  ifelse(req.by=="CT","CT requestor","ED MD requestor"))
             ,center.type = ifelse(center=="CSC", "CSC", "OSH")
             ,arrival.loc = ifelse(ptid %% 2, "Direct-to-CT", "ED Triage")
             ,reason.no.tPA = factor(reasons[tPA.rec], levels=reasons[!is.na(reasons)])
             ## (note how this 1-dimensional factor is employed to generate Table 2 below)
             ,process = paste(
                  ifelse(arrival.loc=="Direct-to-CT", "Imaging", "Triage"),
                  c(`CT requestor`="CT req"
                    ,`ED MD requestor`="MD req"
                    ,`CSC`=NA #"CSC"
                    )[as.character(telestroke.requestor)],
                  sep="/")
             ,labels=c(
                  ott="Onset-to-Treatment Time"
                  ,treated="Received tPA"
                  ,vs.t="Time from onset to first vital signs"
                  ,hx.t="Time from onset to brief history"
                  ,ecg.t="Time from onset to ECG"
                  ,blood.t="Time from onset to blood draw"
                  ,ct.t="Time from onset to CT head"
                  ,req.t="Time from onset to request for telestroke consult"
                  ,login.t="Time from onset to telestroke login"
                  ,login.delay="Time from telestroke request to telestroke login"
                  ,cs.start="Time from onset to start of telestroke consult"
                  ,cs.end="Time from onset to completion of telestroke consult"
                  ,decision.t="Time from onset to tPA decision"
                  ,tPA.start="Time from onset to tPA bolus"
                  ,arrival.t="Time from stroke onset to hospital arrival"
                  ,distance="Distance from hospital"
                  ,tPA.rec="tPA recommendation"
                  ,reason.no.tPA="tPA not given"
                  ,center.type="Comprehensive Stroke Center or Outlying Spoke Hospital"
                  ,severity="Stroke Severity"
                  ,telestroke.requestor="telestroke initiator at OSH"
                  ,process="full process configuration at OSH"
                  ,arrival.loc="transport strategy"
                  ,mode="Transport mode"
                  ,hx.hemo="Prior hemorrhage"
                  ,hx.anticoag="Anticoagulated"
                  ,hx.cva="Prior stroke/TIA"
                  ,hx.afib="History of atrial fibrillation/flutter"
                  ,hx.cad="History of coronary artery disease"
                  ,hx.csten="Carotid stenosis"
                  ,hx.pvd="Peripheral vascular disease"
                  ,hx.dm="Diabetes mellitus"
                  ,hx.htn="History of hypertension"
                  ,hx.smok="Smoker"
                  ,hx.dyslip="History of dyslipidemia"
                  ,hx.ivtpa="Prior IV tPA treatment"
                  )
             ,units=c(
                  ott="minutes"
                  ,vs.t="minutes"
                  ,hx.t="minutes"
                  ,ecg.t="minutes"
                  ,blood.t="minutes"
                  ,ct.t="minutes"
                  ,req.t="minutes"
                  ,login.t="minutes"
                  ,login.delay="minutes"
                  ,cs.start="minutes"
                  ,cs.end="minutes"
                  ,decision.t="minutes"
                  ,tPA.start="minutes"
                  ,arrival.t="minutes"
                  ,distance="minutes"
                  ,severity="NIHSS"
                  )
             )


###################################################
### code chunk number 3: PrepDelays
###################################################
##
## At long last, I should be ready here to construct the promised
## intervals and events!
##
## I presently construct the following intervals (delays, durations):
## (d1) Delay from onset to arrival
## (d2) Delay from telestroke request to telestroke login
## (d3) Duration of telestroke consultation
##
## I also identify the following events occurring during acute care:
## (e1) Arrival at CT or ED triage
## (e2) Telestroke requestor type
## (e3) tPA decision, with reason if not advised
##
delays <- rbind(data.frame(ptid=df$ptid, activity=rep("Prehospital", nrow(df)),
                           start=as.difftime(rep(0, nrow(df)), units='mins'),
                           end=as.difftime(df$arrival.t, units="mins")),
                data.frame(ptid=df$ptid, activity=rep("Login", nrow(df)),
                           start=as.difftime(df$req.t, units="mins"),
                           end=as.difftime(df$login.t, units="mins")),
                data.frame(ptid=df$ptid, activity=rep("Teleconsult", nrow(df)),
                           start=as.difftime(df$login.t, units="mins"),
                           end=as.difftime(df$decision.t, units="mins"))
                )
delays$activity <- colored(delays$activity, color.key=c(
                                                Prehospital='sienna1',
                                                Login='orange3',
                                                Teleconsult='orchid4'
                                                ))
delays$patno <- as.factor(paste("P", delays$ptid, sep=""))


###################################################
### code chunk number 4: PrepEvents
###################################################
events <- rbind(data.frame(ptid=df$ptid, event=as.character(df$arrival.loc),
                           time=as.difftime(df$arrival.t, units="mins")),
                data.frame(ptid=df$ptid, event=as.character(df$telestroke.requestor),
                           time=as.difftime(df$req.t, units="mins")),
                data.frame(ptid=df$ptid, event=rep("tPA infusion start", nrow(df)),
                           time=as.difftime(df$tPA.start, units="mins"))
                )
events$event <- colored(events$event, color.key=c(
                                          ## 2 possible arrival locations
                                          `Direct-to-CT`='springgreen4',
                                          `ED Triage`='yellowgreen',
                                          ## 2 types of telestroke requestor
                                          `CT requestor`='gray',
                                          `ED MD requestor`='black',
                                          ## infusion
                                          `tPA infusion start`='red3'
                                          ))
events$patno <- as.factor(paste("P", events$ptid, sep=""))


###################################################
### code chunk number 5: EightTreatmentCourses
###################################################
timeline("timeline1", delays, events, bsl, ptid="patno",
         condition = mode=="AMB",
         formula = activity + event ~ time | ptid,
         followed=list(
           from=as.difftime(0, units='mins'),
           to=as.difftime(144000, units='mins') # 100 days
           ),
         cols.rows=c(1,8),
         prefix.string = "plot"
         )


