find_pg_m <- function(ind, year, chamber, member) {
  url1 <-'https://www.opensecrets.org/industries/summary.php?ind='
  url2 <- '&cycle='
  url3 <- '&recipdetail='
  url4 <- '&mem='
  url5 <- '&page='

  pg_m <- stringr::str_split(gsub("[A-z\\:]+", "",
                              rvest::html_text(rvest::html_nodes(xml2::read_html(
                                paste0(url1, ind, url2, year, url3, chamber, url4,
                                       member, url5, 1)), ".pageCtrl"))),
                         " ")[[1]]
  return(pg_m[length(pg_m)])
}
