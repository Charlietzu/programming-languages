datatype SolarSystemPlanets = Mercurio | Venus | Terra | Marte | Jupiter | Saturno | Urano | Netuno;

fun planetAge (ageMonths, planet) = 
    case planet of
        Mercurio => (ageMonths div 12) * 88
        | Venus => (ageMonths div 12) * 225
        | Terra => (ageMonths div 12) * 365
        | Marte => (ageMonths div 12) * 687
        | Jupiter => (ageMonths div 12) * 4332
        | Saturno => (ageMonths div 12) * 10760
        | Urano => (ageMonths div 12) * 30681
        | Netuno => (ageMonths div 12) * 60190;

planetAge(24, Jupiter);