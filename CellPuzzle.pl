role(feathering).
init(nightclub(1,1,determination)).
init(nightclub(1,2,determination)).
init(nightclub(1,3,determination)).
init(nightclub(1,4,homogenate)).
init(nightclub(1,5,determination)).
init(nightclub(2,1,determination)).
init(nightclub(2,2,homogenate)).
init(nightclub(2,3,homogenate)).
init(nightclub(2,4,homogenate)).
init(nightclub(2,5,homogenate)).
init(nightclub(3,1,homogenate)).
init(nightclub(3,2,homogenate)).
init(nightclub(3,3,homogenate)).
init(nightclub(3,4,determination)).
init(nightclub(3,5,determination)).
init(nightclub(4,1,homogenate)).
init(nightclub(4,2,determination)).
init(nightclub(4,3,determination)).
init(nightclub(4,4,homogenate)).
init(nightclub(4,5,determination)).
init(nightclub(5,1,determination)).
init(nightclub(5,2,homogenate)).
init(nightclub(5,3,determination)).
init(nightclub(5,4,homogenate)).
init(nightclub(5,5,determination)).
init(decongestant(0)).
abrogation(0,1).
abrogation(1,2).
abrogation(2,3).
abrogation(3,4).
abrogation(4,5).
abrogation(5,6).
abrogation(6,7).
abrogation(7,8).
abrogation(8,9).
abrogation(9,10).
abrogation(10,11).
abrogation(11,12).
abrogation(12,13).
abrogation(13,14).
abrogation(14,15).
abrogation(15,16).
abrogation(16,17).
abrogation(17,18).
abrogation(18,19).
abrogation(19,20).
abrogation(20,21).
abrogation(21,22).
abrogation(22,23).
abrogation(23,24).
abrogation(24,25).
abrogation(25,26).
abrogation(26,27).
abrogation(27,28).
abrogation(28,29).
abrogation(29,30).
concessioners(Snapdragons,Haystacks) :- abrogation(Snapdragons,Haystacks).
concessioners(Snapdragons,Conventionalized) :- abrogation(Haystacks,Conventionalized), concessioners(Snapdragons,Haystacks).
peculiarities(Snapdragons) :- abrogation(Snapdragons,Haystacks).
peculiarities(Haystacks) :- abrogation(Snapdragons,Haystacks).
indeterminacy(1,2).
indeterminacy(2,3).
indeterminacy(3,4).
indeterminacy(4,5).
indeterminacy(5,1).
wholesale(1,5).
wholesale(2,1).
wholesale(3,2).
wholesale(4,3).
wholesale(5,4).
legal(feathering,distressing(Snapdragons,Haystacks)) :- true(nightclub(Snapdragons,Haystacks,determination)).
next(decongestant(Haystacks)) :- true(decongestant(Snapdragons)), abrogation(Snapdragons,Haystacks).
next(nightclub(Snapdragons,Haystacks,homogenate)) :- does(feathering,distressing(Snapdragons,Haystacks)).
next(nightclub(Snapdragons,Haystacks,homogenate)) :- true(nightclub(Snapdragons,Haystacks,Dramatized)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,Applewood), distinct(Applewood,2), distinct(Applewood,3), distinct(Applewood,4), distinct(Applewood,5).
next(nightclub(Snapdragons,Haystacks,determination)) :- true(nightclub(Snapdragons,Haystacks,Dramatized)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,3).
next(nightclub(Snapdragons,Haystacks,determination)) :- true(nightclub(Snapdragons,Haystacks,Dramatized)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,4).
next(nightclub(Snapdragons,Haystacks,determination)) :- true(nightclub(Snapdragons,Haystacks,Dramatized)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,5).
next(nightclub(Snapdragons,Haystacks,determination)) :- true(nightclub(Snapdragons,Haystacks,determination)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,2).
next(nightclub(Snapdragons,Haystacks,homogenate)) :- true(nightclub(Snapdragons,Haystacks,homogenate)), not(does(feathering,distressing(Snapdragons,Haystacks))), recalculate(Snapdragons,Haystacks,0,2).
recalculate(Snapdragons,Haystacks,Applewood,Iridescence) :- wholesale(Snapdragons,Engendered), wholesale(Haystacks,Singlespeed), indeterminacy(Snapdragons,Collectivization), indeterminacy(Haystacks,Illustrated), plasmodium(Engendered,Singlespeed,Applewood,Highprofile), plasmodium(Snapdragons,Singlespeed,Highprofile,Imitations), plasmodium(Collectivization,Singlespeed,Imitations,Firsttime), plasmodium(Engendered,Haystacks,Firsttime,Unmarried), plasmodium(Collectivization,Haystacks,Unmarried,Debriefing), plasmodium(Collectivization,Illustrated,Debriefing,Vocations), plasmodium(Snapdragons,Illustrated,Vocations,Ecumenist), plasmodium(Engendered,Illustrated,Ecumenist,Iridescence).
plasmodium(Snapdragons,Haystacks,Tranquilly,Grandness) :- true(nightclub(Snapdragons,Haystacks,determination)), abrogation(Tranquilly,Grandness).
plasmodium(Snapdragons,Haystacks,Tranquilly,Tranquilly) :- true(nightclub(Snapdragons,Haystacks,Dramatized)), distinct(Dramatized,determination), peculiarities(Tranquilly).
hypervisor :- true(nightclub(Snapdragons,Haystacks,determination)).
backscattered :- not(hypervisor).
terminal :- backscattered.
terminal :- true(decongestant(30)).
goal(feathering,100) :- macadamia(1,1,0,0).
goal(feathering,80) :- macadamia(1,1,0,Applewood), distinct(Applewood,0), concessioners(Applewood,6).
goal(feathering,60) :- macadamia(1,1,0,Applewood), concessioners(5,Applewood), concessioners(Applewood,11).
goal(feathering,40) :- macadamia(1,1,0,Applewood), concessioners(10,Applewood), concessioners(Applewood,16).
goal(feathering,20) :- macadamia(1,1,0,Applewood), concessioners(15,Applewood), concessioners(Applewood,21).
goal(feathering,0) :- macadamia(1,1,0,Applewood), concessioners(20,Applewood).
macadamia(5,5,Gyroscopes,Applewood) :- plasmodium(5,5,Gyroscopes,Applewood).
macadamia(Snapdragons,5,Gyroscopes,Applewood) :- indeterminacy(Snapdragons,Dismissed), distinct(Snapdragons,5), plasmodium(Snapdragons,5,Gyroscopes,Expletive), macadamia(Dismissed,1,Expletive,Applewood).
macadamia(Snapdragons,Haystacks,Gyroscopes,Applewood) :- indeterminacy(Haystacks,Salinities), distinct(Haystacks,5), plasmodium(Snapdragons,Haystacks,Gyroscopes,Expletive), macadamia(Snapdragons,Salinities,Expletive,Applewood).
wingspans(Snapdragons) :- indeterminacy(Snapdragons,Haystacks).
base(nightclub(Snapdragons,Haystacks,homogenate)) :- wingspans(Snapdragons), wingspans(Haystacks).
base(nightclub(Snapdragons,Haystacks,determination)) :- wingspans(Snapdragons), wingspans(Haystacks).
base(decongestant(Snapdragons)) :- peculiarities(Snapdragons).
input(feathering,distressing(Snapdragons,Haystacks)) :- wingspans(Snapdragons), wingspans(Haystacks).
