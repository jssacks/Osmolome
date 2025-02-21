





compound.pal <- 
  c("L-Glutamic acid" =  "#393b79",
    "L-Glutamine" = "#5254a3",
    "L-Aspartic acid" = "#6b6ecf",
    "L-Proline" =	"#9c9ede",
    "L-Alanine" =	"#357eb9",
    "beta-Alanine" = "#5a9ecc",
    "L-Isoleucine" = "#bad2eb",
    "L-Asparagine" =  "#0f8299", 
    "Hydroxyisoleucine" =	"#3e9fb3",
    "Sarcosine" =	"#7abecc",
    "L-Threonine" =	"#b8dee6",
    "beta-Glutamic acid" = "#063970",
    "Glycine betaine" =	"#8c6d31",
    "beta-Alaninebetaine" = "#bd9e39",
    "Homarine" =	"#e7ba52",
    "Trigonelline" =	"#e7cb94",
    "Carnitine" =	"#843c39",
    "Proline betaine"	= "#ad494a",
    "Betonicine" =	"#d6616b",
    "(3-Carboxypropyl)trimethylammonium" = "#e7969c",
    "Homoserine Betaine (tentative)" = 	"#7b4173",
    "Threonine Betaine (tentative)"	= "#a55194",
    "Ectoine" =	"#ce6dbd",
    "5-Hydroxyectoine" =	"#de9ed6",
    "2-O-alpha-D-Glucosylglycerol" =	"#3d0f99",  
    "Sucrose" =	"#653eb3",
    "Trehalose" =	"#756bb1",
    "Dimethylsulfonioacetate" =	"#637939",
    "Gonyol" =	"#8ca252",
    "Dimethylsulfoniopropionate"	= "#cedb9c",
    "Isethionic acid" =	"#fdae6b",
    "Taurine" = "#fdd0a2",
    "(R)-2,3-Dihydroxypropane-1-sulfonate" =	"#fd8d3c",
    "Trimethylamine N-oxide"	= "#e6550d",
    "Arsenobetaine" = "#cccccc")



compound.pal.fig <- 
  c("Glutamic acid" =  "#393b79",
    "Glutamine" = "#5254a3",
    "Aspartic acid" = "#6b6ecf",
    "Proline" =	"#9c9ede",
    "Alanine" =	"#357eb9",
    "beta-Alanine" = "#5a9ecc",
    "(Iso)leucine" = "#bad2eb",
    "Asparagine" =  "#0f8299", 
    "Hydroxyisoleucine" =	"#3e9fb3",
    "Sarcosine" =	"#7abecc",
    "Threonine" =	"#b8dee6",
    "beta-Glutamic acid" = "#063970",
    "GBT" =	"#8c6d31",
    "beta-Alanine betaine" = "#bd9e39",
    "Homarine" =	"#e7ba52",
    "Trigonelline" =	"#e7cb94",
    "Carnitine" =	"#843c39",
    "Proline betaine"	= "#ad494a",
    "Betonicine" =	"#d6616b",
    "TMAB" = "#e7969c",
    "Homoserine Betaine (t)" = 	"#7b4173",
    "Threonine Betaine (t)"	= "#a55194",
    "Ectoine" =	"#ce6dbd",
    "Hydroxyectoine" =	"#de9ed6",
    "GG" =	"#3d0f99",  
    "Sucrose" =	"#653eb3",
    "Trehalose" =	"#756bb1",
    "DMSA" =	"#637939",
    "Gonyol" =	"#8ca252",
    "DMSP"	= "#cedb9c",
    "Isethionic acid" =	"#fdae6b",
    "Taurine" = "#fdd0a2",
    "DHPS" =	"#fd8d3c",
    "TMAO"	= "#e6550d",
    "Arsenobetaine" = "#cccccc")






tod.palette <- c(
  "morning" = "#fbca50",
  "midday" = "#919562", 
  "evening" = "#67a9b8", 
  "night" = "#1b1e14")



region.palette <- c(
  "NPSG" = "#ffd65f",
  "Equator" = "#4dbbd5", 
  "NPTZ" = "#00a087", 
  "CC" = "#3c5488",
  "PS" = "#ec795d")



compound.order <- 
  tibble(Compound = c("L-Glutamic acid", "L-Glutamine", "L-Aspartic acid", "L-Proline", "L-Alanine", "beta-Alanine", "L-Isoleucine",
                      "L-Asparagine", "Hydroxyisoleucine", "Sarcosine", "L-Threonine", "beta-Glutamic acid", "Glycine betaine", "beta-Alaninebetaine", "Homarine", "Trigonelline",
                      "Carnitine", "Proline betaine", "Betonicine", "(3-Carboxypropyl)trimethylammonium",
                      "Ectoine", "5-Hydroxyectoine", "2-O-alpha-D-Glucosylglycerol",
                      "Sucrose", "Trehalose", "Dimethylsulfonioacetate", "Gonyol", "Dimethylsulfoniopropionate",
                      "Isethionic acid", "Taurine", "(R)-2,3-Dihydroxypropane-1-sulfonate", "Trimethylamine N-oxide", "Arsenobetaine"), 
         order = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30, 31, 32, 33),
         class = c("AA", "AA", "AA", "AA", "AA", "AA", "AA", "AA", "AA", "AA", "AA", "AA", "Betaine", "Betaine", "Betaine", "Betaine", "Betaine", "Betaine", "Betaine",
                   "Betaine", "Other", "Other","Sugar", "Sugar", "Sugar", "Sulfonium", "Sulfonium", "Sulfonium",
                   "Sulfonate","Sulfonate", "Sulfonate", "Other", "Other"),
         compound.name.figure = c("Glutamic acid", "Glutamine", "Aspartic acid", "Proline", "Alanine", "beta-Alanine", "(Iso)leucine", "Asparagine", "Hydroxyisoleucine",
                                  "Sarcosine", "Threonine", "beta-Glutamic acid", "GBT", "beta-Alanine betaine", "Homarine", "Trigonelline", 
                                  "Carnitine", "Proline betaine", "Betonicine", "TMAB", "Ectoine",
                                  "Hydroxyectoine", "GG", "Sucrose", "Trehalose", "DMSA", "Gonyol", "DMSP", "Isethionic acid", "Taurine", "DHPS", "TMAO",
                                  "Arsenobetaine"))



