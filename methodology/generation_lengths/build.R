# Builds the refined generation-length tables shipped in inst/extdata:
#
#   generation_lengths_species.csv  vertebrates and Habitats Directive taxa,
#                                   one row per species (or HD-listed taxon)
#   generation_lengths_clades.csv   family (or genus, where a family mixes
#                                   trees and herbs) and NDOP-group defaults
#                                   for everything else
#
# Inputs (this folder):
#   vertebrates_life_history.csv  curated age at first reproduction (AFR, IUCN
#                                 age convention) and longevity in the wild
#   hd_taxa_life_history.csv      non-vertebrate HD taxa: parameters or a
#                                 direct value with its basis
#   clades.csv                    curated clade values (non-vertebrate groups)
#   cso_birds.txt                 ČSO checklist of the birds of Czechia
#   ndop_bird_names.txt           bird names as used in NDOP (snapshot sample)
#   bird_synonyms.csv             checklist / NDOP name -> BirdLife name
# Downloaded:
#   BirdLife International (2025) Generation lengths of the world's birds v3.1
#   Amniote life-history database (Myhrvold et al. 2015), for bird families
#
# Method (Red List Guidelines 4.4): birds take BirdLife's value; other
# vertebrates and iteroparous invertebrates and plants take option 3,
# GL = AFR + z * (longevity - AFR) with z = 0.29 (Pacifici et al. 2013);
# semelparous taxa take their age at reproduction; seed- or egg-bank taxa
# option 6. Run from the package root: Rscript methodology/generation_lengths/build.R

here <- "methodology/generation_lengths"
z <- 0.29
read_csv8 <- function(f) utils::read.csv(file.path(here, f), fileEncoding = "UTF-8",
                                          stringsAsFactors = FALSE, na.strings = "")
option3 <- function(afr, L) round(afr + z * pmax(L - afr, 0), 1)

groups <- utils::read.csv("inst/extdata/generation_lengths.csv", fileEncoding = "UTF-8",
                          stringsAsFactors = FALSE)
lat_of <- setNames(groups$group_lat, groups$kat_tax)
cz_of <- setNames(groups$kat_tax, groups$group_lat)

# --- Habitats Directive annexes of vertebrates occurring in Czechia ----------
hd_vert <- c(
  "Bombina bombina" = "II, IV", "Bombina variegata" = "II, IV",
  "Lissotriton montandoni" = "II, IV", "Triturus carnifex" = "II, IV",
  "Triturus cristatus" = "II, IV", "Triturus dobrogicus" = "II, IV",
  "Hyla arborea" = "IV", "Bufotes viridis" = "IV", "Epidalea calamita" = "IV",
  "Pelobates fuscus" = "IV", "Rana arvalis" = "IV", "Rana dalmatina" = "IV",
  "Pelophylax lessonae" = "IV", "Rana temporaria" = "V",
  "Pelophylax esculentus" = "V", "Pelophylax ridibundus" = "V",
  "Emys orbicularis" = "II, IV", "Coronella austriaca" = "IV",
  "Zamenis longissimus" = "IV", "Lacerta agilis" = "IV", "Lacerta viridis" = "IV",
  "Natrix tessellata" = "IV", "Podarcis muralis" = "IV", "Ablepharus kitaibelii" = "IV",
  "Canis lupus" = "II, IV", "Castor fiber" = "II, IV", "Lutra lutra" = "II, IV",
  "Lynx lynx" = "II, IV", "Mustela eversmanii" = "II, IV",
  "Spermophilus citellus" = "II, IV", "Ursus arctos" = "II, IV",
  "Cricetus cricetus" = "IV", "Dryomys nitedula" = "IV",
  "Muscardinus avellanarius" = "IV", "Sicista betulina" = "IV",
  "Felis silvestris" = "IV", "Martes martes" = "V", "Mustela putorius" = "V",
  "Rupicapra rupicapra" = "V",
  "Barbastella barbastellus" = "II, IV", "Miniopterus schreibersii" = "II, IV",
  "Myotis bechsteinii" = "II, IV", "Myotis blythii" = "II, IV",
  "Myotis dasycneme" = "II, IV", "Myotis emarginatus" = "II, IV",
  "Myotis myotis" = "II, IV", "Rhinolophus ferrumequinum" = "II, IV",
  "Rhinolophus hipposideros" = "II, IV",
  "Cobitis elongatoides" = "II", "Cobitis taenia" = "II", "Cottus gobio" = "II",
  "Eudontomyzon mariae" = "II", "Lampetra planeri" = "II", "Misgurnus fossilis" = "II",
  "Rhodeus amarus" = "II", "Romanogobio albipinnatus" = "II",
  "Romanogobio banaticus" = "II", "Romanogobio belingi" = "II",
  "Romanogobio kesslerii" = "II", "Romanogobio vladykovi" = "II",
  "Sabanejewia balcanica" = "II", "Zingel streber" = "II",
  "Gymnocephalus baloni" = "II, IV", "Gymnocephalus schraetser" = "II, V",
  "Leuciscus aspius" = "II, V", "Pelecus cultratus" = "II, V",
  "Rutilus virgo" = "II, V", "Salmo salar" = "II, V", "Zingel zingel" = "II, V",
  "Hucho hucho" = "II, V", "Acipenser ruthenus" = "V", "Barbus barbus" = "V",
  "Coregonus maraena" = "V", "Thymallus thymallus" = "V")
bats_iv <- c("Eptesicus nilssonii", "Eptesicus serotinus", "Hypsugo savii",
             "Myotis alcathoe", "Myotis brandtii", "Myotis daubentonii",
             "Myotis mystacinus", "Myotis nattereri", "Nyctalus lasiopterus",
             "Nyctalus leisleri", "Nyctalus noctula", "Pipistrellus kuhlii",
             "Pipistrellus nathusii", "Pipistrellus pipistrellus",
             "Pipistrellus pygmaeus", "Plecotus auritus", "Plecotus austriacus",
             "Vespertilio murinus")
hd_vert[bats_iv] <- "IV"

# --- Vertebrates other than birds ---------------------------------------------
v <- read_csv8("vertebrates_life_history.csv")
v$generation_length_years <- ifelse(v$semelparous, v$afr_years,
                                    option3(v$afr_years, v$longevity_years))
v$method <- ifelse(v$semelparous, "semelparous: age at reproduction",
                   "Guidelines 4.4 option 3, z = 0.29")
v$source <- ifelse(v$semelparous, "life history (curated)",
                   "AFR and longevity in the wild (curated); z after Pacifici et al. 2013")
v$source[v$taxon == "Anguilla anguilla"] <- "IUCN Red List (Pike, Crook & Gollock 2020)"
v$group_lat <- unname(lat_of[v$kat_tax])
v$hd_annex <- unname(hd_vert[v$taxon])
v$hd_annex[is.na(v$hd_annex)] <- ""

# --- Birds ----------------------------------------------------------------------
bl_url <- "https://cdn.sanity.io/files/6ibvd6r4/production/6112c4a02b41081735feb810145cac6f656a5d96.xlsx"
bl_file <- tempfile(fileext = ".xlsx")
utils::download.file(bl_url, bl_file, mode = "wb", quiet = TRUE)
bl <- as.data.frame(readxl::read_excel(bl_file, sheet = 1, skip = 1))
names(bl) <- c("species", "english", "gl", "bl_source")
syn <- read_csv8("bird_synonyms.csv")
ndop_birds <- readLines(file.path(here, "ndop_bird_names.txt"), encoding = "UTF-8")
cso <- readLines(file.path(here, "cso_birds.txt"), encoding = "UTF-8")
cso <- cso[nzchar(cso)]
b <- data.frame(name = unique(c(ndop_birds, cso)), stringsAsFactors = FALSE)
b$bl_name <- ifelse(b$name %in% bl$species, b$name, syn$birdlife_name[match(b$name, syn$name)])
if (anyNA(b$bl_name)) stop("Birds without a BirdLife name: ", paste(b$name[is.na(b$bl_name)], collapse = ", "))
lumped <- syn$name[syn$relation == "lumped by BirdLife"]
# One row per BirdLife species, named as in NDOP where NDOP uses the name;
# taxa BirdLife lumps keep rows of their own.
b$key <- ifelse(b$name %in% lumped, b$name, b$bl_name)
pick <- function(n, fallback) { nd <- n[n %in% ndop_birds]; if (length(nd)) nd[1] else fallback }
birds <- do.call(rbind, lapply(split(b, b$key), function(d) {
  shown <- if (d$key[1] %in% lumped) d$key[1] else pick(c(d$bl_name[1], d$name), d$bl_name[1])
  others <- setdiff(unique(c(d$name, d$bl_name)), shown)
  data.frame(taxon = shown, bl_name = d$bl_name[1], synonyms = paste(others, collapse = "; "),
             stringsAsFactors = FALSE)
}))
m <- match(birds$bl_name, bl$species)
birds$generation_length_years <- round(as.numeric(bl$gl[m]), 2)
birds$method <- "BirdLife value"
birds$source <- paste0("BirdLife International (2025) v3.1: ", bl$bl_source[m],
                       ifelse(birds$taxon != birds$bl_name,
                              paste0(" (as ", birds$bl_name, ")"), ""))
birds$note <- ifelse(birds$taxon %in% lumped, "BirdLife lumps this taxon; value of the lumped species", "")

# Bird families: by genus from the Amniote database, with the genera it files
# under older names mapped by hand.
am_file <- tempfile(fileext = ".rda")
utils::download.file("https://raw.githubusercontent.com/RS-eco/traitdata/main/data/amniota.rda",
                     am_file, mode = "wb", quiet = TRUE)
e <- new.env(); load(am_file, envir = e); am <- get(ls(e)[1], e)
am <- am[am$Class == "Aves", ]
fam_of_genus <- tapply(am$Family, am$Genus, function(x) names(sort(table(x), decreasing = TRUE))[1])
fam_extra <- c(Curruca = "Sylviidae", Mareca = "Anatidae", Spatula = "Anatidae",
               Clanga = "Accipitridae", Tetrastes = "Phasianidae", Lyrurus = "Phasianidae",
               Leiopicus = "Picidae", Dendrocoptes = "Picidae", Dryobates = "Picidae",
               Coloeus = "Corvidae", Hydrocoloeus = "Laridae", Chroicocephalus = "Laridae",
               Ichthyaetus = "Laridae", Thalasseus = "Laridae", Microcarbo = "Phalacrocoracidae",
               Gulosus = "Phalacrocoracidae", Linaria = "Fringillidae", Acanthis = "Fringillidae",
               Chloris = "Fringillidae", Spinus = "Fringillidae", Calidris = "Scolopacidae",
               Tachymarptis = "Apodidae", Catharacta = "Stercorariidae", Anarhynchus = "Charadriidae",
               Ptyonoprogne = "Hirundinidae", Cecropis = "Hirundinidae", Lophophanes = "Paridae",
               Periparus = "Paridae", Cyanistes = "Paridae", Poecile = "Paridae",
               Saxicola = "Muscicapidae", Oenanthe = "Muscicapidae", Phoenicurus = "Muscicapidae",
               Luscinia = "Muscicapidae", Erithacus = "Muscicapidae", Ficedula = "Muscicapidae",
               Monticola = "Muscicapidae", Calliope = "Muscicapidae", Tarsiger = "Muscicapidae",
               Iduna = "Acrocephalidae", Hippolais = "Acrocephalidae", Acrocephalus = "Acrocephalidae",
               Locustella = "Locustellidae", Phylloscopus = "Phylloscopidae", Regulus = "Regulidae",
               Panurus = "Panuridae", Remiz = "Remizidae", Aegithalos = "Aegithalidae",
               Calandrella = "Alaudidae", Alaudala = "Alaudidae", Mergellus = "Anatidae",
               Clangula = "Anatidae", Alopochen = "Anatidae", Ardea = "Ardeidae",
               Bubulcus = "Ardeidae", Ardeola = "Ardeidae", Egretta = "Ardeidae",
               Hieraaetus = "Accipitridae", Circaetus = "Accipitridae", Bucanetes = "Fringillidae",
               Carpodacus = "Fringillidae", Emberiza = "Emberizidae", Plectrophenax = "Calcariidae",
               Calcarius = "Calcariidae", Zapornia = "Rallidae", Porzana = "Rallidae")
birds$family <- unname(ifelse(sub(" .*", "", birds$bl_name) %in% names(fam_extra),
                              fam_extra[sub(" .*", "", birds$bl_name)],
                              fam_of_genus[sub(" .*", "", birds$bl_name)]))
if (anyNA(birds$family)) warning("Birds without a family: ",
                                 paste(birds$taxon[is.na(birds$family)], collapse = ", "))
birds$kat_tax <- cz_of[["Aves"]]; birds$group_lat <- "Aves"
birds$afr_years <- NA; birds$longevity_years <- NA; birds$hd_annex <- ""

# --- Non-vertebrate HD taxa ---------------------------------------------------
h <- read_csv8("hd_taxa_life_history.csv")
o3 <- h$method == "option 3"
h$generation_length_years[o3] <- option3(h$afr_years[o3], h$longevity_years[o3])
h$method <- c("option 3" = "Guidelines 4.4 option 3, z = 0.29",
              "semelparous" = "semelparous: age at reproduction",
              "seed bank" = "Guidelines 4.4 option 6 (seed or egg bank)",
              "life strategy" = "life strategy (expert estimate)",
              "lichen convention" = "lichen red-listing convention")[h$method]
h$source <- "life history (curated)"
h$source[h$method == "lichen red-listing convention"] <- "Estonian lichen red list practice (7 / 17 years)"
h$group_lat <- unname(lat_of[h$kat_tax])
h$hd_annex <- h$annex

cols <- c("taxon", "kat_tax", "group_lat", "family", "generation_length_years", "method",
          "afr_years", "longevity_years", "hd_annex", "source", "synonyms", "note")
sp <- rbind(v[, cols], birds[, cols], h[, cols])
sp$synonyms[is.na(sp$synonyms)] <- ""; sp$note[is.na(sp$note)] <- ""
sp$scope <- ifelse(sp$group_lat %in% c("Aves", "Mammalia", "Chiroptera", "Reptilia", "Amphibia",
                                       "Actinopterygii et Cephalaspidomorphi"),
                   "vertebrate", "Habitats Directive")
sp <- sp[order(match(sp$group_lat, groups$group_lat), sp$taxon), c(cols[1:4], "scope", cols[-(1:4)])]
stopifnot(!anyDuplicated(sp$taxon), all(sp$generation_length_years > 0), !anyNA(sp$group_lat))

# --- Clades -------------------------------------------------------------------
cl <- read_csv8("clades.csv")
cl$note[is.na(cl$note)] <- ""
# Vertebrates: medians of the species above, by family and by NDOP group.
vert <- sp[sp$scope == "vertebrate", ]
med <- function(x) round(stats::median(x), 1)
fam <- stats::aggregate(generation_length_years ~ group_lat + family, vert, med)
fam_n <- stats::aggregate(generation_length_years ~ group_lat + family, vert, length)
fam$rank <- "family"; fam$clade <- fam$family
fam$basis <- "median of species"
fam$note <- sprintf("median of %d species in the species table", fam_n$generation_length_years)
vert$genus <- sub(" .*", "", vert$taxon)
gen <- stats::aggregate(generation_length_years ~ group_lat + genus, vert, med)
gen_n <- stats::aggregate(generation_length_years ~ group_lat + genus, vert, length)
gen <- gen[gen_n$generation_length_years > 1, ]   # a lone species is in the species table
gen$rank <- "genus"; gen$clade <- gen$genus
gen$basis <- "median of species"
gen$note <- sprintf("median of %d species in the species table",
                    gen_n$generation_length_years[gen_n$generation_length_years > 1])
grp <- stats::aggregate(generation_length_years ~ group_lat, vert, med)
grp_n <- stats::aggregate(generation_length_years ~ group_lat, vert, length)
grp$rank <- "group"; grp$clade <- grp$group_lat
grp$basis <- "median of species"
grp$note <- sprintf("median of %d species in the species table", grp_n$generation_length_years)
ccols <- c("group_lat", "rank", "clade", "generation_length_years", "basis", "note")
cl <- rbind(cl[, ccols], grp[, ccols], fam[, ccols], gen[, ccols])
cl$kat_tax <- unname(cz_of[cl$group_lat])
cl$rank <- factor(cl$rank, c("group", "family", "genus"))
cl <- cl[order(match(cl$group_lat, groups$group_lat), cl$rank, cl$clade),
         c("group_lat", "kat_tax", "rank", "clade", "generation_length_years", "basis", "note")]
cl$rank <- as.character(cl$rank)
stopifnot(all(groups$group_lat %in% cl$group_lat[cl$rank == "group"]),
          !anyDuplicated(cl[, c("group_lat", "rank", "clade")]))

# --- Genus -> family, for the family step of the lookup -----------------------
# NDOP exports carry no family. Plants, fungi, lichens and bryophytes: the
# AOPK checklist (portal.nature.cz, compiled in the cbs-msk-digitalizace
# project); the most frequent family of each genus. Vertebrates: the species
# table. Other animals have genus rows wherever the value differs.
checklist <- "../cbs-msk-digitalizace/reference/checklist_aopk.csv"
ck <- utils::read.csv(checklist, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
ck$group_lat <- c(vascular_plant = "Tracheophyta", fungus = "Fungi", lichen = "Lichenes",
                  bryophyte = "Bryophyta")[ck$taxon_group]
ck$genus <- sub(" .*", "", trimws(ck$scientific_name))
ck$family <- trimws(ck$family)
ck <- ck[!is.na(ck$group_lat) & grepl("^[A-Z][a-z]+$", ck$genus) & grepl("aceae$", ck$family), ]
gf <- do.call(rbind, lapply(split(ck, paste(ck$group_lat, ck$genus)), function(d) {
  data.frame(group_lat = d$group_lat[1], genus = d$genus[1],
             family = names(sort(table(d$family), decreasing = TRUE))[1], stringsAsFactors = FALSE)
}))
gf_v <- unique(data.frame(group_lat = vert$group_lat, genus = vert$genus, family = vert$family,
                          stringsAsFactors = FALSE))
gf <- rbind(gf, gf_v)
gf <- gf[order(match(gf$group_lat, groups$group_lat), gf$genus), ]
stopifnot(!anyDuplicated(gf[, c("group_lat", "genus")]))
fam_rows <- cl[cl$rank == "family", ]
miss <- setdiff(paste(gf$group_lat, gf$family), paste(fam_rows$group_lat, fam_rows$clade))
message(length(unique(miss)), " checklist families take their group default")

utils::write.csv(gf, "inst/extdata/generation_lengths_genus_family.csv", row.names = FALSE,
                 fileEncoding = "UTF-8")
utils::write.csv(sp, "inst/extdata/generation_lengths_species.csv", row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")
utils::write.csv(cl, "inst/extdata/generation_lengths_clades.csv", row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")
message(nrow(sp), " species rows (", sum(sp$scope == "vertebrate"), " vertebrates), ",
        nrow(cl), " clade rows")
