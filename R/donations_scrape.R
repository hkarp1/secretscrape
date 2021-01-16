#' @title donations_scrape
#' @description scrapes OpenSecrets political campaign contribution data. This function collects donations by specific industries to congresspeople and congressional candidates 
#' @param inds vector of industries in string format to scrape donations from
#' @param years vector of election years in string format identifying the elections to scrape data from
#' @param chambers vector of chambers of Congress in string format to scrape donations to, Default: c("H", "S")
#' @param member either "Y" or "N", include only donations to members of Congress or donations to members and failed candidate, Default: 'Y'
#' @return outputs a dataframe containing contribution data including candidate information, the amount donated, the chamber of congress, the election year, and the donating industry
#' @details very large requests will result in overtaxing the OpenSecrets servers and causing a HTTP 429 error, wait and rerun smaller requests when this error occurs
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  donations_scrape("", "2004", "H", "N")
#'  }
#' }
#' @rdname donations_scrape
#' @export 

donations_scrape <- function(inds, years, chambers = c("H", "S"), member = "Y") {
  valid_inds = c("G2100", "G2400", "G2300", "A1200", "A1400",
                 "C1100", "C2400", "C2100", "C5120", "C2200", "C2600",
                 "C2300", "B4200", "E1500", "E1210", "E1140", "F2700",
                 "F4600", "F1420", "F2600", "F1410", "F2500", "H1500",
                 "H1400", "H4100", "H4600", "H4300", "T1100",
                 "T2310", "T2300", "T2100", "T6250", "G5210", "M3100",
                 "G7000", "G5400", "G6550", "G6400", "G2900", "LT100",
                 "L1300", "L1500", "H5300", "Z1200", "J2100", "J7300",
                 "Z1100", "J2200", "A07", "A01", "A04", "A09", "A10",
                 "B12", "B13", "B01", "B09", "B08", "B02", "C02", "C04",
                 "C01", "C05", "C03", "D01", "D02", "D03", "E08", "E04",
                 "E01", "E10", "F11", "F03", "F05", "F06", "F09", "F13",
                 "F10", "F04", "F07", "H01", "H03", "H02", "H04", "K01",
                 "K02", "M01", "M02", "M04", "M05", "M03", "N02", "N00",
                 "N05", "N07", "N13", "N01", "N08", "N09", "Q08", "N15",
                 "N04", "N06", "N03", "N14", "N16", "P01", "P02", "P05",
                 "P04", "P03", "W03", "W05", "W04", "W02", "W06", "Q14",
                 "Q15", "Q16", "Q02", "Q11", "Q04", "Q12", "Q13", "Q09",
                 "Q03", "Q05", "Q01", "A06", "A05", "A02")
  
  ind_info = c("Food and kindred products manufacturing",
               "Food stores", "Meat processing & products",
               "Sugar cane & sugar beets", "Vegetables, fruits and tree nut",
               "Book, newspaper & periodical publishing",
               "Motion Picture production & distribution",
               "Commercial TV & radio stations",
               "Computer software", "Cable & satellite TV production",
               "Recorded Music & music production",
               "TV production", "Architectural services",
               "Alternate energy production & services",
               "Coal mining", "Natural Gas transmission & distribution",
               "Hedge Funds", "Mortgage bankers and brokers",
               "Payday lenders", "Private Equity & Investment Firms",
               "Student loan companies", "Venture capital",
               "Chiropractors", "Dentists", "Medical Devices & Supplies",
               "Nutritional & dietary supplements",
               "Pharmaceutical manufacturing", "Airlines",
               "Auto dealers, foreign imports", "Auto dealers, new & used",
               "Auto manufacturers", "Cruise ships & lines",
               "Advertising & public relations services",
               "Clothing & accessories",
               "Correctional facilities constr & mgmt/for-profit",
               "Funeral services", "Indian Gaming",
               "Professional sports, arenas & related equip & svcs",
               "Restaurants & drinking establishments",
               "Air transport unions", "Teachers unions",
               "US Postal Service unions & associations",
               "For-profit Education", "Democratic Candidate Committees",
               "Democratic leadership PAC", "Gay & lesbian rights & issues",
               "Republican Candidate Committees", "Republican leadership PAC",
               "Agricultural Services/Products",
               "Crop Production & Basic Processing",
               "Dairy", "Food Processing & Sales", "Forestry & Forest Products",
               "Electronics Mfg & Equip", "Internet",
               "Printing & Publishing", "Telecom Services",
               "Telephone Utilities", "TV/Movies/Music",
               "Home Builders", "Construction Services",
               "General Contractors", "Building Materials & Equipment",
               "Special Trade Contractors", "Defense Aerospace",
               "Defense Electronics", "Misc Defense",
               "Electric Utilities", "Mining", "Oil & Gas",
               "Waste Management", "Accountants", "Commercial Banks",
               "Credit Unions", "Finance/Credit Companies",
               "Insurance", "Misc Finance", "Real Estate",
               "Savings & Loans", "Securities & Investment",
               "Health Professionals", "Health Services/HMOs",
               "Hospitals/Nursing Homes", "Pharmaceuticals/Health Products",
               "Lawyers/Law Firms", "Lobbyists",
               "Air Transport", "Automotive", "Railroads",
               "Sea Transport", "Trucking", "Beer, Wine & Liquor",
               "Business Associations", "Business Services",
               "Casinos/Gambling", "Chemical & Related Manufacturing",
               "Food & Beverage", "Lodging/Tourism", "Marijuana",
               "Women's Issues", "Misc Manufacturing & Distributing",
               "Misc Services", "Recreation/Live Entertainment",
               "Retail Sales", "Steel Production",
               "Textiles", "Building Trade Unions",
               "Industrial Unions", "Misc Unions",
               "Public Sector Unions", "Transportation Unions",
               "Civil Servants/Public Officials", 
               "Clergy & Religious Organizations",
               "Education", "Non-Profit Institutions",
               "Retired", "Abortion Policy/Anti-Abortion",
               "Abortion Policy/Pro-Abortion Rights",
               "Candidate Committees",
               "Democratic/Liberal", "Environment",
               "Foreign & Defense Policy", "Gun Control",
               "Gun Rights", "Human Rights",
               "Leadership PACs", "Pro-Israel",
               "Republican/Conservative", "Livestock",
               "Poultry & Eggs", "Tobacco")
  ind_df = data.frame(ids = valid_inds, inds = ind_info)
  
  valid_years = as.character(seq(from =  1990, to = 2020, by = 2))
  valid_chambers = c("H", "S")
  
  if (sum(inds %in% valid_inds) != length(inds)) {
    stop("invalid industry code entry")
  }
  
  if (sum(years %in% valid_years) != length(years)) {
    stop("invalid year entry")
  }
  
  if (sum(chambers %in% valid_chambers) != length(chambers)) {
    stop("invalid chamber entry")
  }
  
  if (!(member == "Y" | member == "N")) {
    stop("invalid membership entry")
  }
  
  acc = rlist::list.rbind(lapply(chambers, function(x) {many_don_scrapes(inds, years, x, member)}))
  return(dplyr::left_join(acc, ind_df, by = c("ind" = "ids")))
}

