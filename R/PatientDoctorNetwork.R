#' PatientDoctorNetwork
#'
#' @description
#' This is an artificial network data. This Network is build to understand the
#' functionalities of the `edges_to_adjacency` function.
#'
#' @usage PatientDoctorNetwork
#'
#' @format
#' A list of the following enties :
#'
#' \describe{
#'   \item{patient_names}{Patient's surnames}
#'   \item{doctors_names}{Doctor's names}
#'   \item{patient_patient}{
#'   data.frame : 3 cols x 403 rows
#'   \enumerate{
#'      \item{from : Patient calling}
#'      \item{to : Patient called}
#'      \item{value : time spend calling (min)}
#'    }
#'   }
#'   \item{doctor_patient}{
#'   data.frame : 3 cols x 335 rows
#'   \enumerate{
#'      \item{from : Patient in meeting}
#'      \item{to : Doctor in meeting}
#'      \item{value : number of meetings}
#'    }
#'   }
#' }
"PatientDoctorNetwork"
