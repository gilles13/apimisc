globalVariables(c("url_complete", "dateparution",
									"numeroannonce", "numerodepartement", "desc", "description",
									"records_count", "attributions", "dt", "id", "title",
									"bbox",
									"type",
									"distribution",
									"keyword",
									"publicationavis"))

#' @title apimisc_make_link
#' @description Retourne la chaine de caractere entouree des balises pour la rendre cliquable
#' @param url l'url a rendre cliquable
#' @param text texte a afficher en remplacement de l'url
#' @return une chaine de caracteres cliquable
#' @examples
#' \dontrun{
#' apimisc_make_link(url = "monurl", text = "clic")
#' }
#' @export
apimisc_make_link <- function(url, text = "clic"){
  paste0("<a target = \'_blank\' href= ", url, ">", text, "</a>")
}

#' @title apimisc_get_bodacc_siren
#' @description Retourne le resultat de la requete sur siren dans le bodacc
#' @param siren le siren a interroger
#' @return un data.frame du resultat de la requete sur siren
#' @examples
#' \dontrun{
#' apimisc_get_bodacc_siren(siren = "123456789")
#' }
#' @export
apimisc_get_bodacc_siren <- function(siren = "") {
	siren <- gsub(" ", "", siren)
	urlBase <- "https://bodacc-datadila.opendatasoft.com/api/explore/v2.1/catalog/datasets/annonces-commerciales/exports/csv"
	urlTot <- paste0(
		urlBase,
		"?where=registre%3A%22",
		siren,
		"%22&limit=-1&timezone=UTC&use_labels=false&compressed=false")
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>
		httr2::req_perform() |> 
		httr2::resp_body_string()
	data <- utils::read.csv2(text = res)
	return(data)
}

#' @title apimisc_parse_bodacc_siren
#' @description Retourne le resultat parse de la requete sur siren dans le bodacc
#' @param x une requete get_bodacc_siren
#' @return un data.frame du resultat parse de la requete sur siren
#' @examples
#' \dontrun{
#' apimisc_parse_bodacc_siren(x = marequete)
#' }
#' @export
apimisc_parse_bodacc_siren <- function(x) {
	res <- 
		x |> 
		dplyr::select(-tidyselect::any_of(c(
																				"listepersonnes",
																				"ftcontent",
																				"acte",
																				"depot",
																				"registre",
																				"listeetablissements",
																				"pdf_parution_subfolder",
																				"radiationaurcs",
																				"tribunal",
																				"parutionavisprecedent",
																				"listeprecedentexploitant",
																				"listeprecedentproprietaire",
																				"parution",
																				"ispdf_unitaire",
																				"typeavis_lib",
																				"departement_nom_officiel",
																				"region_code",
																				"region_nom_officiel",
																				""))) |> 
		dplyr::rename(url = url_complete) |> 
		dplyr::mutate(dateparution = as.Date(dateparution, format = "%Y-%m-%d")) |> 
		dplyr::mutate(url = apimisc::apimisc_make_link(url)) |> 
		dplyr::relocate(dateparution) |> 
		dplyr::rename(pubavis = publicationavis) |> 
		dplyr::rename(num = numeroannonce) |> 
		dplyr::rename(dept = numerodepartement) |> 
		dplyr::arrange(desc(dateparution))
	return(res)
}

#' @title apimisc_get_balo_siren
#' @description Retourne le resultat de la requete sur siren dans le balo
#' @param siren le siren a interroger
#' @return un data.frame du resultat de la requete sur siren dans le balo
#' @examples
#' \dontrun{
#' apimisc_get_balo_siren(siren = "123456789")
#' }
#' @export
apimisc_get_balo_siren <- function(siren = "") {
	siren <- gsub(" ", "", siren)
	urlBase <- "https://journal-officiel-datadila.opendatasoft.com/api/explore/v2.1/catalog/datasets/balo/records"
	urlTot <- 
		paste0(urlBase,
					 "?where=siren%3A%22",
					 siren,
					 "%22&limit=100&include_links=true&include_app_metas=true")
	res <- 
		urlTot |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = TRUE)
	return(res)
}

# ------------------------------------------------
# API JO
# ------------------------------------------------

#' @title apimisc_get_jo_cat
#' @description Retourne le catalogue de l'API journalofficiel
#' @return un data.frame des catalogues de l'API JO
#' @examples
#' \dontrun{
#' apimisc_get_jo_cat()
#' }
#' @export
apimisc_get_jo_cat <- function() {
	urlBase <- "https://journal-officiel-datadila.opendatasoft.com/api/explore/v2.1/catalog/datasets?limit=50&offset=0&timezone=UTC&include_links=false&include_app_metas=false"
	res <- 
		urlBase |> 
		httr2::request() |> 
		httr2::req_headers("Accept" = 'application/json') |>
		httr2::req_perform() |> 
		httr2::resp_body_json(simplifyVector = TRUE)
	return(res)
}

#' @title apimisc_parse_jo_cat
#' @description Retourne le catalogue de l'API journalofficiel parse
#' @return un data.frame des catalogues de l'API JO parse
#' @param cat le catalogue a parser
#' @param limite si TRUE (par defaut), limite a 4 le nb de variables renvoyees par la fonction
#' @examples
#' \dontrun{apimisc_parse_jo_cat(cat = "moncat")}
#' @export
apimisc_parse_jo_cat <- function(cat, limite = TRUE) {
	codename <- 
		cat |> 
		purrr::pluck("results", "dataset_id")
	metas <- 
		cat |> 
		purrr::pluck("results", "metas", "default") |> 
		dplyr::select(-description, -bbox)
	res <- 
		cbind(codename, metas)
	if(limite) {
		res <- 
			res |> 
			dplyr::select(codename, title, records_count, attributions)
	}
	return(res)
}


#' @title acceder au repertoire du template pour tdb multi api
#' @description Retourne le chemin du template Rmarkdown pour dashboard siren
#' @param pack le nom du package
#' @param tempname le nom du template
#' @return une chaine de char
#' @examples
#' \dontrun{
#' apimisc_template_path(pack = "apimisc")
#' }
#' @export
apimisc_template_path <- function(pack = "apimisc", tempname = "multiapi") {
	if(.Platform$OS.type == "unix") {
		r_libs <- Sys.getenv("R_LIBS_USER")
		r_path <- paste0(r_libs,
										 "/",
										 pack,
										 "/rmarkdown/templates/",
										 tempname,
										 "/skeleton/skeleton.Rmd")
	} else {
		r_libs <- Sys.getenv("R_HOME")
		r_path <- paste0(r_libs,
										 "/library/",
										 pack,
										 "/rmarkdown/templates/",
										 tempname,
										 "/skeleton/skeleton.Rmd")
	}
	return(file.path(r_path))
}

#' @title acceder au repertoire www du package
#' @description Retourne le chemin du repertoire www
#' @param pack le nom du package
#' @return une chaine de char
#' @examples
#' \dontrun{
#' apimisc_www_path(pack = "apimisc")
#' }
#' @export
apimisc_www_path <- function(pack = "apimisc") {
	if(.Platform$OS.type == "unix") {
		r_libs <- Sys.getenv("R_LIBS_USER")
		r_path <- paste0(r_libs,
										 "/",
										 pack,
										 "/www/")
	} else {
		r_libs <- Sys.getenv("R_HOME")
		r_path <- paste0(r_libs,
										 "/library/",
										 pack,
										 "/www/")
	}
	return(file.path(r_path))
}


#' @title generer un rapport html d'une entreprise
#' @description Affiche une page html synthetisant de l'information de base
#' @param siren une chaine de caractere pour l'entreprise a consulter
#' @return un fichier html, affiche dans le browser
#' @examples
#' \dontrun{
#' apimisc_make_report(siren = "401887401")
#' }
#' @export
apimisc_make_report <- function(siren) {
	apisitempfile <- tempfile(fileext = ".html")
	rmarkdown::render(
			input = apimisc_template_path(),
			params = list(mysiren = siren),
			envir = new.env(),
			output_file = apisitempfile,
			quiet = TRUE
	)
	message("Le fichier genere : ", apisitempfile)
	utils::browseURL(apisitempfile)
}

# ------------------------------------------------
# INFO FINANCIERES
# ------------------------------------------------

#' @title get info-fi API catalog
#' @description Retourne la liste des catalogues de l'API info fi
#' @return un data.frame du catalogue de l'API info fi
#' @examples
#' \dontrun{
#' if_get_catalog_list()
#' }
#' @export
if_get_catalog_list <- function() {
  urlBase <- "https://www.info-financiere.gouv.fr/api/explore/v2.1/catalog/datasets"
  res <- 
    urlBase |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = TRUE)
  return(res)
}

#' @title get info-fi API dataset
#' @description Retourne le catalogue de l'API info fi demande
#' @param cat le catalogue demande
#' @param simp if TRUE (default) simplify vector
#' @return un data.frame du catalogue de l'API info fi
#' @examples
#' \dontrun{
#' if_get_dataset(cat = "")
#' }
#' @export
if_get_dataset <- function(cat = "", simp = TRUE) {
  urlBase <- "https://www.info-financiere.gouv.fr/api/explore/v2.1/catalog/datasets/"
  urlTot <- paste0(urlBase, cat, "/records?limit=100&include_links=true&include_app_metas=true")
  # message(urlTot)
  res <- 
    urlTot |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = simp)
  totalcount <- res$total_count
  message(totalcount)
  nbssreqafaire <- ceiling(totalcount / 100)
  listeoffset <- seq(from = 0, to = totalcount, by = 100)
  maliste <- vector(mode = "list", length = 0)
  for(i in seq_len(nbssreqafaire)) {
    myoffset <- listeoffset[i]
    newurl <- paste0(urlTot, "&offset=", myoffset)
#     message(newurl)
    x <- 
      newurl |> 
      httr2::request() |> 
      httr2::req_headers("Accept" = 'application/json') |>
      httr2::req_perform() |> 
      httr2::resp_body_json(simplifyVector = simp) |> 
      purrr::pluck("results")
    maliste <- append(maliste, list(x))
  }
  res <- do.call("rbind", maliste)
  return(res)
}

# ------------------------------------------------
# BANQUE DE FRANCE
# ------------------------------------------------

#' @title get banque de france dataset catalog
#' @description retourne un data.frame des datasets du catalogue banque de france
#' @return data.frame
#' @examples
#' \dontrun{
#' bdf_get_catalog(dataset = "OFC")
#' }
#' @export
bdf_get_catalog <- function() {
	urlBase <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/webstat-datasets/exports/json?select=dataset_id,model_id,description_fr,description_en,series_count,dimensions_and_codelists,series_attributes_and_codelists,observations_attributes_and_codelists,sources,last_observation_date&order_by=dataset_id"
  res <- 
		urlBase |> 
    httr2::request() |> 
    httr2::req_headers(Authorization = paste("Apikey", Sys.getenv("WEBSTAT_APIKEY"))) |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_perform() |> 
		httr2::resp_body_json(simplify = TRUE)
  return(res)
}

#' @title get series infos from banque de france dataset catalog
#' @description retourne un data.frame d'information sur les series d'un dataset du catalogue banque de france
#' @param dataset le code du catalogue
#' @return data.frame
#' @examples
#' \dontrun{
#' bdf_list_datasets(dataset = "OFC")
#' }
#' @export
bdf_list_datasets <- function(dataset = "OFC") {
	urlBase <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/series/exports/json?select=series_key,title_fr,title_long_fr,title_en,title_long_en,first_time_period_date,last_time_period_date,source_agency,series_dimensions_and_values,series_attributes_and_values&refine=dataset_id:"
  urlTot <- paste0(urlBase, dataset)
  res <- 
		urlTot |> 
    httr2::request() |> 
    httr2::req_headers(Authorization = paste("Apikey", Sys.getenv("WEBSTAT_APIKEY"))) |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_perform() |> 
		httr2::resp_body_json(simplify = TRUE)
  return(res)
}

#' @title get serie from banque de france dataset catalog
#' @description retourne un data.frame d'une serie de la banque de france
#' @param key le nom de la serie
#' @return data.frame
#' @examples
#' \dontrun{
#' bdf_get_serie(key = "MIR1.M.FR.B.L23FRLA.D.R.A.2230U6.EUR.O")
#' }
#' @export
bdf_get_serie <- function(key = NULL) {
	urlBase <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json?order_by=time_period_start&refine=series_key:"
	urlTot <- paste0(urlBase, key)
	res <- 
		urlTot |> 
    httr2::request() |> 
    httr2::req_headers(Authorization = paste("Apikey", Sys.getenv("WEBSTAT_APIKEY"))) |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_perform() |> 
		httr2::resp_body_json(simplify = TRUE)
  return(res)
}

# ----------------------------------------------------------
# API DATA.GOUV.FR DOCUMENTATION
# ----------------------------------------------------------

#' @title get data.gouv.fr data portal informations
#' @description cree un repertoire dans HOME et enregistre un data.frame sur les series de data.gouv.fr
#' @return data.frame
#' @examples
#' \dontrun{
#' dg_get_site_data_portal()
#' }
#' @export
dg_get_site_data_portal <- function() {
	urlBase <- "https://www.data.gouv.fr/api/1/site/data.json"
	homeDir <- Sys.getenv("HOME")
	siteDataDir <- paste0(homeDir, "/.site_data_portal")
	siteDataFile <- paste0(siteDataDir, "/data.RDS")
	if(!dir.exists(file.path(siteDataDir))) {
		dir.create(file.path(siteDataDir))
	}
  res <- 
    urlBase |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>  
    httr2::req_perform() |> 
    httr2::resp_body_json(simplify = TRUE)
	saveRDS(object = res, file = paste0(siteDataDir, "/data.RDS"))
	message("Objet sauvegarde dans :", siteDataDir)
	return(res)
}

#' @title read data.gouv.fr data portal informations
#' @description lit dans le repertoire HOME le fichier sur les series de data.gouv.fr
#' @return data.frame
#' @examples
#' \dontrun{
#' dg_read_site_data_portal()
#' }
#' @export
dg_read_site_data_portal <- function() {
	homeDir <- Sys.getenv("HOME")
	siteDataDir <- paste0(homeDir, "/.site_data_portal")
	siteDataFile <- paste0(siteDataDir, "/data.RDS")
	myfile <- readRDS(siteDataFile)
	res <- 
		myfile |> 
		purrr::pluck("@graph") |> 
		janitor::clean_names() |> 
		data.frame()
	return(res)
}

#' @title make report on dataservice
#' @description cree un rapport sur un dataservice
#' @param ident l'identifiant du dataservice
#' @return flexdashboard report
#' @examples
#' \dontrun{
#' dg_report_dataservice(ident = NULL)
#' }
#' @export
dg_report_dataservice <- function(ident) {
	mydt <- dg_read_site_data_portal()
	res <- 
		dt |> 
		dplyr::filter(id == ident) |> 
		dplyr::select(title, type, id, distribution, keyword, description)
	return(res)
}

# ----------------------------------------------------------
# IMF
# ----------------------------------------------------------

#' @title get countries code
#' @description telecharge les codes pays et lib en fr (iso 3166 alpha 3)
#' @return data.frame
#' @examples
#' \dontrun{
#' imf_get_country()
#' }
#' @export
imf_get_country <- function() {
  entrypoint <- "https://www.imf.org/external/datamapper/api/v1/countries"
  res <- 
    httr2::request(entrypoint) |> 
    httr2::req_headers(`Content-Type` = 'application/json') |> 
    httr2::req_perform() |>
		httr2::resp_body_json(simplify = TRUE)
  countriescode <- 
    res |> 
    purrr::pluck("countries") |> 
    names()
  countrieslab <- 
    res |> 
    purrr::pluck("countries") |>
    purrr::map_dfr(\(x) {
			tibble::tibble(
        labels = x |> purrr::pluck('label', .default = "")
      )}) |> 
    dplyr::pull()
  resultat <- data.frame(
    code = countriescode,
    label = countrieslab
  )
  return(resultat)
}

#' @title get dataset informations
#' @description telecharge des infos sur les datasets dispo sur IMF
#' @return data.frame
#' @examples
#' \dontrun{
#' imf_get_dataset_info()
#' }
#' @export
imf_get_dataset_info <- function() {
  entrypoint <- "https://www.imf.org/external/datamapper/api/v1/indicators"
  res <- 
    httr2::request(entrypoint) |> 
    httr2::req_headers(`Content-Type` = 'application/json') |> 
    httr2::req_perform() |>
		httr2::resp_body_json(simplify = TRUE)
  imfindiccode <- 
    res |> 
    purrr::pluck('indicators') |> 
    names()
  imfindicinfos <- 
    res |> 
    purrr::pluck('indicators', .default = "") |> 
    purrr::map_dfr(\(x) {
			 tibble::tibble(
        dataset = x |> purrr::pluck('dataset', .default = ""),
        label = x |> purrr::pluck('label', .default = ""),
        desc = x |> purrr::pluck('description', .default = ""),
        unit = x |> purrr::pluck('unit', .default = "")
      )
    })
  resultat <- cbind(
    tibble::tibble(code = imfindiccode),
    imfindicinfos
  )
  return(resultat)
}

# ----------------------------------------------------------
# WORLD BANK
# ----------------------------------------------------------

#' @title get world bank catalog
#' @description telecharge des infos sur les datasets dispo via World Bank
#' @return data.frame
#' @examples
#' \dontrun{
#' wb_get_catalog()
#' }
#' @export
wb_get_catalog <- function() {
  entrypoint <- ""
  res <- 
    httr2::request(entrypoint) |> 
    httr2::req_headers(`Content-Type` = 'application/json') |> 
    httr2::req_perform() |>
		httr2::resp_body_json(simplify = TRUE)
  return(res)
}

# MISC

# get_params <- function(...) {
#   arguments <- as.list(match.call())
#   arguments <- arguments[-1]
#   return(arguments)
# }
# get_params(a, b = 2, ... = list(c = "titi"))
# get_params(a, b = 2, ... = "titi")
# get_params()
# get_params(b)





