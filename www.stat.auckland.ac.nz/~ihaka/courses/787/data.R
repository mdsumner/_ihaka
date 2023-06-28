jg =
    data.frame(
        journal =
            c("J Soc Psychol", "Oxford Rev Educ", "Br J Sociol",
              "Rev Educ Res", "Social Forces", "Am Sociol Rev", 
              "Am J Sociol", "Rev Econ Stat", "Econometrica",
              "Educ Res", "J Polit Econ", "Bell J Econ",
              "Am Econ Rev", "Am Educ Res", "Br J Psychol", 
              "J Exp Psychol", "Percept Psychophys",
              "T Am Math Soc", "Ann Statist", "Computer",
              "Biometrika", "Commun Acm", "Am Math Mo",
              "Computer J", "J Am Stat As (Theory)",
              "Comput Gra Im Proc", "J Am Stat As (Applic)", 
              "J R Stat Soc (Ser C)", "Cell", "Science (Articles)",
              "Geogr Rev", "Geogr J", "Ann As Am Geogr",
              "New Eng J Med", "Ibm J Res Develop", 
              "Science (Reports)", "Lancet", "Profess Geogr",
              "Nature (London)", "Phys Rev (A)", "J Am Chem Soc",
              "J Exp Med", "Phys Rev (Lett)", "Scientific Am",
              "Proc R Soc (Ser B)", "J Physics (C)",
              "J Clin Investigation", "Ieee Trans Commun",
              "J Phys Chem", "Life Sciences", "Bell Syst Tech J", 
              "Ieee Comm Radar Sig", "J Appl Physics",
              "J Chem (Faraday Trans)", "J Exp Biol",
              "J Appl Polym Sci", "J Geophys Res"),
        subject =
            c("Psyc", "Educ", "Soc", "Educ", "Soc", "Soc", "Soc",
              "Econ", "Econ", "Educ", "Econ", "Econ", "Econ", "Educ",
              "Psyc", "Psyc", "Psyc", "Math", "Math", "Comp", "Stat",
              "Comp", "Math", "Comp", "Stat", "Comp", "Stat", "Stat",
              "Bio", "Gen", "Geog", "Geog", "Geog", "Med", "Eng",
              "Gen", "Med", "Geog", "Gen", "Phys", "Chem", "Med",
              "Phys", "Gen", "Bio", "Phys", "Med", "Eng", "Chem",
              "Bio", "Eng", "Eng", "Phys", "Chem", "Bio", "Chem",
              "Phys"),
        science =
            c("Social", "Social", "Social", "Social", "Social", "Social",
              "Social", "Social", "Social", "Social", "Social", "Social",
              "Social", "Social", "Social", "Social", "Social",
              "Mathematical", "Mathematical", "Mathematical",
              "Mathematical", "Mathematical", "Mathematical",
              "Mathematical", "Mathematical", "Mathematical",
              "Mathematical", "Mathematical", "Natural", "Natural",
              "Natural", "Natural", "Natural", "Natural", "Natural",
              "Natural", "Natural", "Natural", "Natural", "Natural", 
              "Natural", "Natural", "Natural", "Natural", "Natural",
              "Natural", "Natural", "Natural", "Natural", "Natural",
              "Natural", "Natural", "Natural", "Natural", "Natural",
              "Natural", "Natural"),
        area =
            c(0.000, 0.001, 0.004, 0.004, 0.009, 0.014, 0.014, 0.014,
              0.025, 0.032, 0.034, 0.038, 0.038, 0.041, 0.053, 0.082,
              0.089, 0.002, 0.006, 0.014, 0.019, 0.023, 0.034, 0.045,
              0.057, 0.059, 0.067, 0.105, 0.034, 0.053, 0.055, 0.063,
              0.066, 0.068, 0.070, 0.078, 0.084, 0.089, 0.091, 0.097,
              0.098, 0.106, 0.108, 0.108, 0.114, 0.122, 0.127, 0.139,
              0.149, 0.157, 0.158, 0.164, 0.199, 0.200, 0.207, 0.264,
              0.31),
        stderr =
	    c(0.000, 0.001, 0.002, 0.002, 0.005, 0.004, 0.005, 0.040,
	      0.008, 0.012, 0.008, 0.008, 0.007, 0.011, 0.012, 0.020,
	      0.014, 0.002, 0.003, 0.005, 0.009, 0.008, 0.010, 0.021, 
              0.020, 0.018, 0.013, 0.020, 0.007, 0.010, 0.015, 0.012,
	      0.016, 0.009, 0.012, 0.013, 0.014, 0.018, 0.016, 0.014,
	      0.014, 0.012, 0.010, 0.019, 0.015, 0.016, 0.012, 0.014,
	      0.017, 0.015, 0.021, 0.020, 0.013, 0.021, 0.019, 0.017,
	      0.019),
	stringsAsFactors = FALSE)

"animalSpeed" =
  structure(c(70, 61, 50, 50, 50, 47.5, 45, 45, 43, 42, 40, 40, 
              40, 39.35, 35.5, 35, 35, 35, 32, 32, 30, 30, 30,
              30, 27.89, 25, 20, 18, 15, 12, 11, 9, 1.17, 0.17,
              0.15, 0.03),
            .Names = c("Cheetah", "Pronghorn Antelope", "Wildebeest",
                       "Lion", "Thomson's Gazelle", "Quarterhorse",
                       "Elk", "Cape Hunting Dog", "Coyote", "Gray Fox", 
                       "Hyena", "Zebra", "Mongolian Wild Ass",
                       "Greyhound", "Whippet", "Rabbit (domestic)",
                       "Mule Deer", "Jackal", "Reindeer", "Giraffe", 
                       "White-Tailed Deer", "Wart Hog", "Grizzly Bear",
                       "Cat (domestic)", "Human", "Elephant",
                       "Black Mamba Snake", "Six-Lined Race Runner", 
                       "Wild Turkey", "Squirrel", "Pig (domestic)",
                       "Chicken", "Spider (Tegenaria atrica)", 
                       "Giant Tortoise", "Three-Toed Sloth",
                       "Garden Snail"))

## Per Capita Taxes
## From Cleveland's "The Elements of Graphing Data",
## First Edition. Page 147.
## Data captured by scanning and the use of "g3data".

taxes =
    round(c(290.016639308, 392.12318715, 410.645447819, 425.771230625,
            440.896757637, 474.890185047, 476.997116578, 490.991081152,
            493.098524269, 498.978268502, 499.198921578, 508.287232049,
	    513.035925429, 515.897572976, 519.136578531, 519.922757035,
	    521.086293546, 537.155215579, 538.319007883, 538.161535574,
	    547.060911248, 549.922303003, 552.972885347, 554.136933443,
	    568.884846656, 569.671280952, 578.758824045, 581.809406389,
	    585.425514159, 586.777729675, 591.715102058, 596.463539645,
	    599.324675607, 609.733739106, 618.444435776, 619.607972287,
	    624.922191094, 642.123698735, 654.419552273, 684.639143811,
	    705.425552528, 711.683422147, 714.733748698, 723.821291791,
	    785.739723373, 817.658193324, 823.538449142, 866.400045531))
names(taxes) =
    c("New Hampshire", "South Dakota", "Tennessee", "Missouri", "Ohio",
      "Texas", "Alabama", "Indiana", "Florida", "Mississippi", "Georgia",
      "Arkansas", "Virginia", "Colorado", "Idaho", "Nebraska", "Vermont",
      "Kansas", "South Carolina", "Utah", "North Carolina", "Maine",
      "Oregon", "Montana", "North Dakota", "Louisiana", "New Jersey",
      "Rhode Island", "Kentucky", "Oklahoma", "Connecticut", "Nevada",
      "Iowa", "Pennsylvania", "Illinois", "Arizona", "West Virginia",
      "Michigan", "Maryland", "Massachusetts", "Washington", "New Mexico",
      "Wisconsin", "New York", "Minnesota", "California", "Wyoming",
      "Delaware")

##  Immigration to the US, by place of origin and time period.

immigrants = 
    array(c( 1, 15,  5, 21, 17, 41,
             3, 39, 13, 12, 16, 17,
             3, 41, 32,  4, 13,  7,
             3, 42, 39,  3,  8,  5),
	  dim = c(6, 4),
	  dimnames = list(c("Other", "Latin America", "Asia",
            "Canada", "S & E Europe", "N & W Europe"),
            c("1931-1960", "1961-1970",
			    "1971-1976", "1977-1979")))

immigrants = immigrants[c(1, 4, 6, 5, 3, 2),]

