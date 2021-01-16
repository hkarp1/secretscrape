one_don_scrape <-function(ind, year, chamber, member) {
  url1 <-'https://www.opensecrets.org/industries/summary.php?ind='
  url2 <- '&cycle='
  url3 <- '&recipdetail='
  url4 <- '&mem='
  url5 <- '&page='
  pg_m <-   tryCatch({
    find_pg_m(ind, year, chamber, member)
  }, warning = function(w) {
    print("unknown warning")
  }, error = function(e) {
    0
  })
  closeAllConnections()
  if (pg_m == 0) {
    return(data.frame(Candidate = NA, Amount = NA, year = year,
                      chamber = chamber))
  }
  acc <- rlist::list.rbind(lapply(1:pg_m, function(x)
  {rvest::html_table(rvest::html_nodes(xml2::read_html(paste0(url1, ind,
              url2, year, url3, chamber, url4, member, url5, x)),
                         ".datadisplay"))[[1]]}))
  acc$year <- year
  acc$chamber <- chamber
  closeAllConnections()
  return(acc)
}

