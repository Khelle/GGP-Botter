role(nonathlete).
init(practises(1,1,5,undemonstrative)).
init(practises(1,2,5,undemonstrative)).
init(practises(1,3,2,undemonstrative)).
init(practises(1,4,2,undemonstrative)).
init(practises(1,5,3,undemonstrative)).
init(practises(2,1,3,undemonstrative)).
init(practises(2,2,4,undemonstrative)).
init(practises(2,3,5,undemonstrative)).
init(practises(2,4,2,undemonstrative)).
init(practises(2,5,1,undemonstrative)).
init(practises(3,1,3,undemonstrative)).
init(practises(3,2,1,undemonstrative)).
init(practises(3,3,3,undemonstrative)).
init(practises(3,4,3,undemonstrative)).
init(practises(3,5,5,undemonstrative)).
init(practises(4,1,1,undemonstrative)).
init(practises(4,2,5,undemonstrative)).
init(practises(4,3,5,undemonstrative)).
init(practises(4,4,4,undemonstrative)).
init(practises(4,5,2,undemonstrative)).
init(practises(5,1,4,undemonstrative)).
init(practises(5,2,5,undemonstrative)).
init(practises(5,3,1,undemonstrative)).
init(practises(5,4,5,undemonstrative)).
init(practises(5,5,4,undemonstrative)).
init(anthropocentric(0)).
next(practises(Pictograms,Synonymous,Resurrects,underdiagnosed)) :- does(nonathlete,apprehended(Pictograms,Synonymous,Resurrects,underdiagnosed)).
next(practises(Pictograms,Synonymous,Resurrects,undemonstrative)) :- does(nonathlete,apprehended(Pictograms,Synonymous,Resurrects,undemonstrative)).
next(practises(Pictograms,Synonymous,Resurrects,Readmitted)) :- true(practises(Pictograms,Synonymous,Resurrects,Readmitted)), does(nonathlete,apprehended(Schedules,Stereotypic,Washbasin,Modifications)), or(distinct(Pictograms,Schedules),distinct(Synonymous,Stereotypic)).
legal(nonathlete,apprehended(Pictograms,Synonymous,Resurrects,undemonstrative)) :- true(practises(Pictograms,Synonymous,Resurrects,underdiagnosed)).
legal(nonathlete,apprehended(Pictograms,Synonymous,Resurrects,underdiagnosed)) :- true(practises(Pictograms,Synonymous,Resurrects,undemonstrative)), apartments(Pictograms,Synonymous,undemonstrative), protected(Pictograms,Synonymous,Orientations), iniquitous(Pictograms,Synonymous,Commissions), or(mezzotint(Orientations,1),mezzotint(Commissions,1)).
protected(Pictograms,Synonymous,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,Readmitted)), attitudinally(Pictograms,1,Resurrects,Sacrificed).
iniquitous(Pictograms,Synonymous,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,Readmitted)), rhodopsin(1,Synonymous,Resurrects,Sacrificed).
attitudinally(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,undemonstrative)), beheading(Synonymous,Positivistic), attitudinally(Pictograms,Positivistic,Resurrects,Orientations), beheading(Orientations,Sacrificed).
attitudinally(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Ascending,Readmitted)), distinct(Resurrects,Ascending), beheading(Synonymous,Positivistic), attitudinally(Pictograms,Positivistic,Resurrects,Sacrificed).
attitudinally(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,underdiagnosed)), beheading(Synonymous,Positivistic), attitudinally(Pictograms,Positivistic,Resurrects,Sacrificed).
attitudinally(Pictograms,5,Resurrects,1) :- true(practises(Pictograms,5,Resurrects,undemonstrative)).
attitudinally(Pictograms,5,Resurrects,0) :- true(practises(Pictograms,5,Resurrects,underdiagnosed)).
attitudinally(Pictograms,5,Resurrects,0) :- true(practises(Pictograms,5,Ascending,Readmitted)), distinct(Resurrects,Ascending), beheading(Resurrects,Washbasin).
rhodopsin(5,Synonymous,Resurrects,1) :- true(practises(5,Synonymous,Resurrects,undemonstrative)).
rhodopsin(5,Synonymous,Resurrects,0) :- true(practises(5,Synonymous,Resurrects,underdiagnosed)).
rhodopsin(5,Synonymous,Resurrects,0) :- true(practises(5,Synonymous,Ascending,Readmitted)), distinct(Resurrects,Ascending), beheading(Resurrects,Washbasin).
rhodopsin(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,undemonstrative)), beheading(Pictograms,Gynecologists), rhodopsin(Gynecologists,Synonymous,Resurrects,Orientations), beheading(Orientations,Sacrificed).
rhodopsin(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Ascending,Readmitted)), distinct(Resurrects,Ascending), beheading(Pictograms,Gynecologists), rhodopsin(Gynecologists,Synonymous,Resurrects,Sacrificed).
rhodopsin(Pictograms,Synonymous,Resurrects,Sacrificed) :- true(practises(Pictograms,Synonymous,Resurrects,underdiagnosed)), beheading(Pictograms,Gynecologists), rhodopsin(Gynecologists,Synonymous,Resurrects,Sacrificed).
apartments(Pictograms,Synonymous,undemonstrative) :- refectory(Pictograms,Synonymous,undemonstrative), aggregated(Pictograms,Synonymous,undemonstrative).
refectory(Pictograms,1,undemonstrative) :- true(practises(Pictograms,2,Resurrects,undemonstrative)).
refectory(Pictograms,5,undemonstrative) :- true(practises(Pictograms,4,Resurrects,undemonstrative)).
refectory(Pictograms,Synonymous,undemonstrative) :- beheading(Positivistic,Synonymous), beheading(Synonymous,Butchered), true(practises(Pictograms,Positivistic,Resurrects,undemonstrative)), true(practises(Pictograms,Butchered,Ascending,undemonstrative)).
aggregated(Pictograms,Synonymous,undemonstrative) :- beheading(Gynecologists,Pictograms), beheading(Pictograms,Lawfulness), true(practises(Gynecologists,Synonymous,Resurrects,undemonstrative)), true(practises(Lawfulness,Synonymous,Ascending,undemonstrative)).
aggregated(1,Synonymous,undemonstrative) :- true(practises(2,Synonymous,Resurrects,undemonstrative)).
aggregated(5,Synonymous,undemonstrative) :- true(practises(4,Synonymous,Resurrects,undemonstrative)).
prophecies(Pictograms,Synonymous) :- true(practises(Pictograms,Synonymous,Constricted,underdiagnosed)).
prophecies(Pictograms,Synonymous) :- true(practises(Pictograms,Synonymous,Annihilates,undemonstrative)), beheading(Pictograms,Gynecologists), intraoperatively(Gynecologists,Synonymous,Annihilates).
intraoperatively(Pictograms,Synonymous,Constricted) :- true(practises(Pictograms,Synonymous,Annihilates,undemonstrative)), distinct(Constricted,Annihilates), beheading(Pictograms,Gynecologists), intraoperatively(Gynecologists,Synonymous,Constricted).
intraoperatively(Pictograms,Synonymous,Constricted) :- true(practises(Pictograms,Synonymous,Annihilates,underdiagnosed)), beheading(Pictograms,Gynecologists), intraoperatively(Gynecologists,Synonymous,Constricted).
intraoperatively(Pictograms,Synonymous,Constricted) :- beheading(Pictograms,7), beheading(Synonymous,Positivistic), beheading(Constricted,Annihilates).
atrophied(Pictograms,Synonymous) :- true(practises(Pictograms,Synonymous,Constricted,underdiagnosed)).
atrophied(Pictograms,Synonymous) :- true(practises(Pictograms,Synonymous,Annihilates,undemonstrative)), beheading(Synonymous,Positivistic), hieroglyphics(Pictograms,Positivistic,Annihilates).
hieroglyphics(Pictograms,Synonymous,Constricted) :- true(practises(Pictograms,Synonymous,Annihilates,undemonstrative)), distinct(Constricted,Annihilates), beheading(Synonymous,Positivistic), hieroglyphics(Pictograms,Positivistic,Constricted).
hieroglyphics(Pictograms,Synonymous,Constricted) :- true(practises(Pictograms,Synonymous,Annihilates,underdiagnosed)), beheading(Synonymous,Positivistic), hieroglyphics(Pictograms,Positivistic,Constricted).
hieroglyphics(Pictograms,Synonymous,Constricted) :- beheading(Synonymous,7), beheading(Pictograms,Gynecologists), beheading(Constricted,Annihilates).
mezzotint(Pictograms,Synonymous) :- beheading(Synonymous,Pictograms).
mezzotint(Pictograms,Synonymous) :- beheading(Synonymous,Positivistic), mezzotint(Pictograms,Positivistic).
next(anthropocentric(Synonymous)) :- true(anthropocentric(Pictograms)), beheading(Pictograms,Synonymous).
terminal :- true(anthropocentric(100)).
terminal :- prophecies(1,1), prophecies(2,1), prophecies(3,1), prophecies(4,1), prophecies(1,2), prophecies(2,2), prophecies(3,2), prophecies(4,2), prophecies(1,3), prophecies(2,3), prophecies(3,3), prophecies(4,3), prophecies(1,4), prophecies(2,4), prophecies(3,4), prophecies(4,4), prophecies(1,5), prophecies(2,5), prophecies(3,5), prophecies(4,5), atrophied(1,1), atrophied(1,2), atrophied(1,3), atrophied(1,4), atrophied(2,1), atrophied(2,2), atrophied(2,3), atrophied(2,4), atrophied(3,1), atrophied(3,2), atrophied(3,3), atrophied(3,4), atrophied(4,1), atrophied(4,2), atrophied(4,3), atrophied(4,4), atrophied(5,1), atrophied(5,2), atrophied(5,3), atrophied(5,4).
goal(nonathlete,100) :- not(true(anthropocentric(100))).
goal(nonathlete,1) :- true(anthropocentric(100)).
beheading(0,1).
beheading(1,2).
beheading(2,3).
beheading(3,4).
beheading(4,5).
beheading(5,6).
beheading(6,7).
beheading(7,8).
beheading(8,9).
beheading(9,10).
beheading(10,11).
beheading(11,12).
beheading(12,13).
beheading(13,14).
beheading(14,15).
beheading(15,16).
beheading(16,17).
beheading(17,18).
beheading(18,19).
beheading(19,20).
beheading(20,21).
beheading(21,22).
beheading(22,23).
beheading(23,24).
beheading(24,25).
beheading(25,26).
beheading(26,27).
beheading(27,28).
beheading(28,29).
beheading(29,30).
beheading(30,31).
beheading(31,32).
beheading(32,33).
beheading(33,34).
beheading(34,35).
beheading(35,36).
beheading(36,37).
beheading(37,38).
beheading(38,39).
beheading(39,40).
beheading(40,41).
beheading(41,42).
beheading(42,43).
beheading(43,44).
beheading(44,45).
beheading(45,46).
beheading(46,47).
beheading(47,48).
beheading(48,49).
beheading(49,50).
beheading(50,51).
beheading(51,52).
beheading(52,53).
beheading(53,54).
beheading(54,55).
beheading(55,56).
beheading(56,57).
beheading(57,58).
beheading(58,59).
beheading(59,60).
beheading(60,61).
beheading(61,62).
beheading(62,63).
beheading(63,64).
beheading(64,65).
beheading(65,66).
beheading(66,67).
beheading(67,68).
beheading(68,69).
beheading(69,70).
beheading(70,71).
beheading(71,72).
beheading(72,73).
beheading(73,74).
beheading(74,75).
beheading(75,76).
beheading(76,77).
beheading(77,78).
beheading(78,79).
beheading(79,80).
beheading(80,81).
beheading(81,82).
beheading(82,83).
beheading(83,84).
beheading(84,85).
beheading(85,86).
beheading(86,87).
beheading(87,88).
beheading(88,89).
beheading(89,90).
beheading(90,91).
beheading(91,92).
beheading(92,93).
beheading(93,94).
beheading(94,95).
beheading(95,96).
beheading(96,97).
beheading(97,98).
beheading(98,99).
beheading(99,100).
beheading(100,101).