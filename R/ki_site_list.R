#' Get list of available sites
#'
#' @export
#' @description Returns a tibble containing metadata available catchments.
#' @param hub The KiWIS database you are querying. Either one of the defaults or a URL.
#'  See \href{https://github.com/rywhale/kiwisR}{README}.
#' @param return_fields (Optional) KiWIS return fields as a list.
#' @param datasource (Optional) The data source to be used, defaults to 0.
#' @return A tibble
#' @examples
#' \dontrun{
#' ki_site_list(hub = 'swmc')
#' }
#'

ki_site_list <- function(hub, return_fields, datasource = 0) {


  # Account for user-provided return fields
  if (missing(return_fields)) {
    return_fields <- "site_no, site_id, site_uuid, site_name, site_longname, site_latitude, site_longitude, site_carteasting, site_cartnorthing,
    site_type_name, site_type_shortname, parametertype_id, parametertype_name, stationparameter_name, site_georefsystem, site_area_wkt, site_area_wkt_org"
  } else {
    if (!inherits(return_fields, "character")) {
      stop(
        "User supplied return_fields must be comma separated string or vector of strings"
      )
    }
  }


    # Identify hub
  api_url <- check_hub(hub)

  api_query <- list(
    service = "kisters",
    datasource = datasource,
    type = "queryServices",
    request = "getSiteList",
    format = "json",
    kvp = "true",
    returnfields = paste(
      return_fields,
      collapse = ","
    )
  )

  # Send request
  raw <- tryCatch({
    httr::GET(
      url = api_url,
      query = api_query,
      httr::timeout(15)
    )}, error = function(e){
      return(e)
    })

  check_ki_response(raw)

  # Parse response
  raw_content <- httr::content(raw, "text")

  # Parse text
  json_content <- jsonlite::fromJSON(raw_content)

  content_dat <- tibble::as_tibble(
    json_content[2:nrow(json_content), ],
    .name_repair = "minimal"
    )

  names(content_dat) <- json_content[1, ]

  return(content_dat)
}
