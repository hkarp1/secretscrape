more_don_scrapes <- function(ind, years, chamber, member) {
  acc <- rlist::list.rbind(lapply(years, function(x) {one_don_scrape(ind, x, chamber, member)}))
  acc$ind <- ind
  return(acc)
}

