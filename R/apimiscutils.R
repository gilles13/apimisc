
#' @title apimisc_make_link
#' @description Retourne la chaine de caractere entouree des balises pour la rendre cliquable
#' @param url l'url a rendre cliquable
#' @param text texte a afficher en remplacement de l'url
#' @return une chaine de caracteres cliquable
#' @examples
#' apimisc_make_link(url = "https://mapetiteurl.fr", text = "clic")
#' @export
apimisc_make_link <- function(url, text = "clic"){
  paste0("<a target = '_blank' href= ", url, ">", text, "</a>")
}

#' @title apimisc_get_bodacc_siren
#' @description Retourne le resultat de la requete sur siren dans le bodacc
#' @param siren le siren a interroger
#' @return un data.frame du resultat de la requete sur siren
#' @examples
#' apimisc_get_bodacc_siren(siren = "123456789")
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
	data <- read.csv2(text = res)
	return(data)
}

#' @title apimisc_parse_bodacc_siren
#' @description Retourne le resultat parse de la requete sur siren dans le bodacc
#' @param x une requete get_bodacc_siren
#' @return un data.frame du resultat parse de la requete sur siren
#' @examples
#' apimisc_parse_bodacc_siren(x)
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
		dplyr::mutate(url = apimisc_make_link(url)) |> 
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
#' apimisc_get_balo_siren(siren = "123456789")
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
# ------------------------------------------------

# API JO

#' @title apimisc_get_jo_cat
#' @description Retourne le catalogue de l'API journalofficiel
#' @return un data.frame des catalogues de l'API JO
#' @examples
#' apimisc_get_jo_cat()
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
#' @examples
#' apimisc_parse_jo_cat(cat = "")
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

# ------------------------------------------------
# ------------------------------------------------

# INFO FINANCIERES

#' @title apimisc_if_get_catalog_list
#' @description Retourne la liste des catalogues de l'API info fi
#' @return un data.frame du catalogue de l'API info fi
#' @examples
#' apimisc_if_get_catalog_list()
#' @export
apimisc_if_get_catalog_list <- function() {
  urlBase <- "https://www.info-financiere.gouv.fr/api/explore/v2.1/catalog/datasets"
  res <- 
    urlBase |> 
    httr2::request() |> 
    httr2::req_headers("Accept" = 'application/json') |>
    httr2::req_perform() |> 
    httr2::resp_body_json(simplifyVector = TRUE)
  return(res)
}

#' @title apimisc_if_get_dataset
#' @description Retourne le catalogue de l'API info fi demande
#' @param cat le catalogue demande
#' @return un data.frame du catalogue de l'API info fi
#' @examples
#' apimisc_if_get_dataset(cat = "")
#' @export
apimisc_if_get_data <- function(cat = "", simp = TRUE) {
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

#' @title acceder au repertoire du template pour tdb multi api
#' @description Retourne le chemin du template Rmarkdown pour dashboard siren
#' @param pack le nom du package
#' @param tempname le nom du template
#' @return une chaine de char
#' @examples
#' apimisc_template_path(pack = "apimisc")
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
#' apimisc_www_path(pack = "apimisc")
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
#' @description Affiche une page html synthétisant de l'information de base
#' @param siren une chaine de caractere pour l'entreprise a consulter
#' @return un fichier html, affiche dans le browser
#' @examples
#' apimisc_make_report(siren = "401887401")
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
	message("Le fichier généré : ", apisitempfile)
	browseURL(apisitempfile)
}

# ------------------------------------------------
# ------------------------------------------------

# MISC

get_params <- function(...) {
  arguments <- as.list(match.call())
  arguments <- arguments[-1]
  return(arguments)
}
# get_params(a, b = 2, ... = list(c = "titi"))
# get_params(a, b = 2, ... = "titi")
# get_params()
# get_params(b)
