#' Import a survey to Qualtrics
#'
#' Import a Qualtrics survey via API so that question text can be version
#' controlled.
#'
#' @param survey_name String. The name to be associated with the survey in Qualtrics.
#' @param file_path String. Path to file describing survey questions according
#' to the format specified \link[here]{https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/#PreparingASimpleFormatTXTFile}.
#' Needs to be \code{qsd}, \code{doc}, or \code{txt} format.
#'
#' @seealso See \url{https://api.qualtrics.com/} for documentation on
#' the Qualtrics API.
#'
#' @export
#' @examples
#' \dontrun{
#' # Register your Qualtrics credentials if you haven't already
#' qualtrics_api_credentials(
#'   api_key = "<YOUR-API-KEY>",
#'   base_url = "<YOUR-BASE-URL>"
#' )
#'
#' # Import a Survey
#' surveys <- all_surveys("Test Survey", "/survey/path")
#'
#' }
#'
import_survey <- function(
  survey_name = NULL,
  file_path = NULL
  ) {

  ## Are the API credentials stored?
  assert_base_url()
  assert_api_key()

  # CONSTRUCT API CALL ----

  # determine file mime:
  mime_ext <- tools::file_ext(file_path)
  file_mime <- glue::glue("application/vnd.qualtrics.survey.{mime_ext}")

  # POST request
  res <- httr::POST(
    httr::add_headers(
      `x-api-token` = Sys.getenv("QUALTRICS_API_KEY")
      ),
    url = generate_url(query = "importsurvey"),
    body = list(
      name = survey_name,
      file = httr::upload_file(
        path = file_path,
        type = file_mime
      )
    ),
    encode = "multipart"
  )

  if (httr::http_type(res) != "application/json") {
    stop("Something is wrong, try your query again.", call. = FALSE)
  }

  return(res)

}
