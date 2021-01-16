many_don_scrapes <- function(inds, years, chamber, member) {
  return(rlist::list.rbind(lapply(inds, function(x) {more_don_scrapes(x, years, chamber, member)})))
}
