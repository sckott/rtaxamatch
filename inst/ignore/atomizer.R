#' parse
#'
#' @export
#' @param name A name
#' @examples
#' parse(name='Homo sapiens Linnaeus')
parse <- function(name){
  @parsed_raw = @parser.parse(name)[:scientificName]
  organize_results(@parsed_raw)
}

#' Raw parsed
#'
#' @export
parsed_raw <- function(){
  @parsed_raw
}

#' Organize results
#'
#' @export
#' @param parsed_raw Raw parsed input
organize_results <- function(parsed_raw){
  pr = parsed_raw
  return nil unless pr[:parsed]
  @res = {:all_authors => [], :all_years => []}
  d = pr[:details][0]
  @res[:canonical_form] = pr[:canonical]
  process_node(:uninomial, d[:uninomial])
  process_node(:genus, d[:genus])
  process_node(:species, d[:species], true)
  process_infraspecies(d[:infraspecies])
  @res[:all_authors] = @res[:all_authors].uniq.map do |a|
    Taxamatch::Normalizer.normalize(a)
  end
  @res[:all_years].uniq!
  @res.keys.size > 2 ? @res : nil
}

#' Process a node
#'
#' @export
#' @param name A name
#' @param node xxxx
#' @param is_species (logical) xxx
process_node <- function(name, node, is_species = FALSE){
  return unless node && node[:string]
  @res[name] = {}
  @res[name][:string] = node[:string]
  @res[name][:normalized] = Taxamatch::Normalizer.normalize(node[:string])
  @res[name][:phonetized] =
    Taxamatch::Phonetizer.near_match(node[:string], is_species)
  get_authors_years(node, @res[name])
}

#' Proceses infraspecies
#'
#' @export
#' @param node xxx
process_infraspecies <- function(node){
  return unless node
  @res[:infraspecies] = []
  node.each do |infr|
    next unless infr[:string]
    hsh = {}
    hsh[:string] = infr[:string]
    hsh[:normalized] = Taxamatch::Normalizer.normalize(infr[:string])
    hsh[:phonetized] = Taxamatch::Phonetizer.near_match(infr[:string], true)
    get_authors_years(infr,hsh)
    @res[:infraspecies] << hsh
  end
}

#' Get authors, year
#'
#' @export
#' @param node xxxx
#' @param res xxxx
get_authors_years <- function(node, res){
  res[:authors] = []
  res[:years] = []
  [:basionymAuthorTeam, :combinationAuthorTeam].each do |au|
    if node[au]
      res[:authors] += node[au][:author]
      if node[au][:year]
        year = Taxamatch::Normalizer.normalize_year(node[au][:year])
        res[:years] << year if year
      end
      if node[au][:exAuthorTeam]
        res[:authors] += node[au][:exAuthorTeam][:author]
        if node[au][:exAuthorTeam][:year]
          year = node[au][:exAuthorTeam][:year]
          year = Taxamatch::Normalizer.normalize_year(year)
          res[:years] << year if year
        end
      end
    end
  end
  res[:authors].uniq!
  res[:normalized_authors] = res[:authors].map do |a|
    Taxamatch::Normalizer.normalize_author(a)
  end
  res[:years].uniq!
  if res[:normalized_authors].size > 0
    @res[:all_authors] += res[:normalized_authors]
  end
  @res[:all_years] += res[:years] if res[:years].size > 0
}
