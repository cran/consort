#' Demo clinical trial disposition data
#'
#' This is a demo data to demonstrate the use of the package. One row per
#' participant. Participants who are excluded should provide a reason, missing
#' otherwise. 
#'
#' @format 
#' A data frame with 300 rows and 11 columns:
#' \describe{
#'   \item{trialno}{Participants ID of the participants}
#'   \item{exclusion1, exclusion2}{Exclusion reason before and after induction}
#'   \item{induction}{Participants ID of the participants who are included in the induction
#'   phase, an extra treatment before randomisation.}
#'   \item{exclusion}{Exclusion reason before randomisation, including before and after
#'   induction}
#'   \item{arm, arm3}{Arms pariticipants randomised to.}
#'   \item{sbujid_dosed}{Participants ID of the participants who had at least one dose of 
#'   the protocol treatment.}
#'   \item{subjid_notdosed}{Reason for participants not dosed.}
#'   \item{followup}{Participants ID planned for follow-up.}
#'   \item{lost_followup}{Reason for participants not dosed.}
#'   \item{assessed}{Participants ID participants attended assessment.}
#'   \item{no_value}{Reason for participants missing final assessment.}
#'   \item{mitt}{Participants ID included in the mITT analysis.}
#' }
"dispos.data"

