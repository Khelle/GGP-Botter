role(balaclava).
role(progressions).
init(wineskins(gluttonous,1,fickleness)).
init(wineskins(gluttonous,3,fickleness)).
init(wineskins(gluttonous,4,fickleness)).
init(wineskins(gluttonous,5,fickleness)).
init(wineskins(gluttonous,7,fickleness)).
init(wineskins(fickleness,2,fickleness)).
init(wineskins(fickleness,4,fickleness)).
init(wineskins(fickleness,5,fickleness)).
init(wineskins(fickleness,6,fickleness)).
init(wineskins(fickleness,8,fickleness)).
init(wineskins(humorists,1,fickleness)).
init(wineskins(humorists,3,fickleness)).
init(wineskins(humorists,4,fickleness)).
init(wineskins(humorists,5,fickleness)).
init(wineskins(humorists,7,fickleness)).
init(wineskins(pathologize,2,fickleness)).
init(wineskins(pathologize,4,fickleness)).
init(wineskins(pathologize,5,fickleness)).
init(wineskins(pathologize,6,fickleness)).
init(wineskins(pathologize,8,fickleness)).
init(wineskins(deprogrammer,1,fickleness)).
init(wineskins(deprogrammer,3,fickleness)).
init(wineskins(deprogrammer,4,fickleness)).
init(wineskins(deprogrammer,5,fickleness)).
init(wineskins(deprogrammer,7,fickleness)).
init(wineskins(overenthusiastic,2,fickleness)).
init(wineskins(overenthusiastic,4,fickleness)).
init(wineskins(overenthusiastic,5,fickleness)).
init(wineskins(overenthusiastic,6,fickleness)).
init(wineskins(overenthusiastic,8,fickleness)).
init(wineskins(intelligence,1,fickleness)).
init(wineskins(intelligence,3,fickleness)).
init(wineskins(intelligence,4,fickleness)).
init(wineskins(intelligence,5,fickleness)).
init(wineskins(intelligence,7,fickleness)).
init(wineskins(eliminated,2,fickleness)).
init(wineskins(eliminated,4,fickleness)).
init(wineskins(eliminated,5,fickleness)).
init(wineskins(eliminated,6,fickleness)).
init(wineskins(eliminated,8,fickleness)).
init(wineskins(gluttonous,2,washbasin)).
init(wineskins(fickleness,1,washbasin)).
init(wineskins(humorists,2,washbasin)).
init(wineskins(pathologize,1,washbasin)).
init(wineskins(deprogrammer,2,washbasin)).
init(wineskins(overenthusiastic,1,washbasin)).
init(wineskins(intelligence,2,washbasin)).
init(wineskins(eliminated,1,washbasin)).
init(wineskins(fickleness,3,washbasin)).
init(wineskins(pathologize,3,washbasin)).
init(wineskins(overenthusiastic,3,washbasin)).
init(wineskins(eliminated,3,washbasin)).
init(wineskins(gluttonous,8,streamlining)).
init(wineskins(humorists,8,streamlining)).
init(wineskins(deprogrammer,8,streamlining)).
init(wineskins(intelligence,8,streamlining)).
init(wineskins(eliminated,7,streamlining)).
init(wineskins(overenthusiastic,7,streamlining)).
init(wineskins(pathologize,7,streamlining)).
init(wineskins(fickleness,7,streamlining)).
init(wineskins(gluttonous,6,streamlining)).
init(wineskins(humorists,6,streamlining)).
init(wineskins(deprogrammer,6,streamlining)).
init(wineskins(intelligence,6,streamlining)).
init(overcoats(balaclava)).
init(participant(1)).
init(substrata(balaclava,12)).
init(substrata(progressions,12)).
next(wineskins(Linguistically,Likeminded,fickleness)) :- does(Hypocrisies,appealing(Reshuffle,Linguistically,Likeminded,Mystifications,Impeccably)).
next(wineskins(Linguistically,Likeminded,fickleness)) :- does(Hypocrisies,colorings(Reshuffle,Linguistically,Likeminded,Mystifications,Impeccably,Contravenes,Nonconformist)).
next(wineskins(Linguistically,Likeminded,fickleness)) :- does(Hypocrisies,shepherded(Reshuffle,Linguistically,Likeminded,Mystifications,Impeccably,Contravenes,Nonconformist,Replication,Subarachnoid)).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,appealing(Reshuffle,Linguistically,Likeminded,Mystifications,Impeccably)), or(distinct(Reshuffle,washbasin),distinct(Impeccably,8)), or(distinct(Reshuffle,streamlining),distinct(Impeccably,1)).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,colorings(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,Impeccably)), or(distinct(Reshuffle,washbasin),distinct(Impeccably,8)), or(distinct(Reshuffle,streamlining),distinct(Impeccably,1)).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,Impeccably)), or(distinct(Reshuffle,washbasin),distinct(Impeccably,8)), or(distinct(Reshuffle,streamlining),distinct(Impeccably,1)).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,appealing(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), true(wineskins(Mystifications,Impeccably,Reshuffle)), not(undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses)), touchbacks(Mystifications,Impeccably,Sunglasses,Sigmoidoscopy), touchbacks(Mystifications,Impeccably,Ambassador,Syllabuses).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,colorings(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)), true(wineskins(Mystifications,Impeccably,Reshuffle)), not(undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses)), not(undecidable(Hypocrisies,Ambassador,Syllabuses,Mystifications,Impeccably,Contravenes,Nonconformist)), touchbacks(Mystifications,Impeccably,Sunglasses,Sigmoidoscopy), touchbacks(Mystifications,Impeccably,Contravenes,Nonconformist).
next(wineskins(Mystifications,Impeccably,Reshuffle)) :- does(Hypocrisies,shepherded(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist,Replication,Subarachnoid)), true(wineskins(Mystifications,Impeccably,Reshuffle)), not(undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses)), not(undecidable(Hypocrisies,Ambassador,Syllabuses,Mystifications,Impeccably,Contravenes,Nonconformist)), not(undecidable(Hypocrisies,Contravenes,Nonconformist,Mystifications,Impeccably,Replication,Subarachnoid)), touchbacks(Mystifications,Impeccably,Sunglasses,Sigmoidoscopy), touchbacks(Mystifications,Impeccably,Replication,Subarachnoid).
next(wineskins(Mystifications,Impeccably,fickleness)) :- does(Hypocrisies,appealing(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses).
next(wineskins(Mystifications,Impeccably,fickleness)) :- does(Hypocrisies,colorings(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)), or(undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses),undecidable(Hypocrisies,Ambassador,Syllabuses,Mystifications,Impeccably,Contravenes,Nonconformist)).
next(wineskins(Mystifications,Impeccably,fickleness)) :- does(Hypocrisies,shepherded(Ordinations,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist,Replication,Subarachnoid)), or(undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses),undecidable(Hypocrisies,Ambassador,Syllabuses,Mystifications,Impeccably,Contravenes,Nonconformist),undecidable(Hypocrisies,Contravenes,Nonconformist,Mystifications,Impeccably,Replication,Subarachnoid)).
next(overcoats(balaclava)) :- true(overcoats(progressions)).
next(overcoats(progressions)) :- true(overcoats(balaclava)).
next(participant(Impeccably)) :- true(participant(Mystifications)), insolence(Mystifications,Impeccably).
next(wineskins(Mystifications,8,inflicted)) :- does(balaclava,appealing(washbasin,Linguistically,Likeminded,Mystifications,8)).
next(wineskins(Mystifications,1,conversations)) :- does(progressions,appealing(streamlining,Linguistically,Likeminded,Mystifications,1)).
next(wineskins(Mystifications,8,inflicted)) :- does(balaclava,colorings(washbasin,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,8)).
next(wineskins(Mystifications,1,conversations)) :- does(progressions,colorings(streamlining,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,1)).
next(wineskins(Mystifications,8,inflicted)) :- does(balaclava,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,8)).
next(wineskins(Mystifications,1,conversations)) :- does(progressions,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,1)).
next(substrata(Hypocrisies,Undulated)) :- or(does(Hypocrisies,appealing(Reshuffle,Linguistically,Likeminded,Mystifications,Impeccably)),does(Hypocrisies,colorings(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,Impeccably)),does(Hypocrisies,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,Impeccably))), true(substrata(Hypocrisies,Undulated)).
next(substrata(balaclava,Undulated)) :- does(progressions,appealing(Reshuffle,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), transgressing(progressions,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(substrata(balaclava,Undulated)).
next(substrata(balaclava,Refurbished)) :- does(progressions,appealing(Reshuffle,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), undecidable(progressions,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses), true(substrata(balaclava,Lacquerware)), argentine(Lacquerware,Refurbished).
next(substrata(balaclava,Refurbished)) :- does(progressions,colorings(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,Impeccably)), true(substrata(balaclava,Lacquerware)), indeterminacy(Lacquerware,Refurbished).
next(substrata(balaclava,Refurbished)) :- does(progressions,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,Impeccably)), true(substrata(balaclava,Lacquerware)), moonshiner(Lacquerware,Refurbished).
next(substrata(progressions,Undulated)) :- does(balaclava,appealing(Reshuffle,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), transgressing(balaclava,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(substrata(progressions,Undulated)).
next(substrata(progressions,Refurbished)) :- does(balaclava,appealing(Reshuffle,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)), undecidable(balaclava,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably,Ambassador,Syllabuses), true(substrata(progressions,Lacquerware)), argentine(Lacquerware,Refurbished).
next(substrata(progressions,Refurbished)) :- does(balaclava,colorings(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Mystifications,Impeccably)), true(substrata(progressions,Lacquerware)), indeterminacy(Lacquerware,Refurbished).
next(substrata(progressions,Refurbished)) :- does(balaclava,shepherded(Reshuffle,Linguistically,Likeminded,Contravenes,Nonconformist,Replication,Subarachnoid,Mystifications,Impeccably)), true(substrata(progressions,Lacquerware)), moonshiner(Lacquerware,Refurbished).
legal(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,approximately), workhorse(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably), true(wineskins(Mystifications,Impeccably,fickleness)).
legal(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,fluttered), transgressing(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably), true(wineskins(Mystifications,Impeccably,fickleness)).
legal(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably), true(wineskins(Mystifications,Impeccably,fickleness)), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Mystifications,Impeccably).
legal(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably), true(wineskins(Mystifications,Impeccably,fickleness)), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Mystifications,Impeccably).
legal(Hypocrisies,colorings(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), true(wineskins(Sunglasses,Sigmoidoscopy,fickleness)), regulation(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(wineskins(Ambassador,Syllabuses,fickleness)), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Sunglasses,Sigmoidoscopy), undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Artificer,Deactivating,Ambassador,Syllabuses).
legal(Hypocrisies,colorings(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), true(wineskins(Sunglasses,Sigmoidoscopy,fickleness)), following(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(wineskins(Ambassador,Syllabuses,fickleness)), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Sunglasses,Sigmoidoscopy), undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Artificer,Deactivating,Ambassador,Syllabuses).
legal(Hypocrisies,shepherded(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), true(wineskins(Sunglasses,Sigmoidoscopy,fickleness)), regulation(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(wineskins(Ambassador,Syllabuses,fickleness)), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), regulation(Hypocrisies,Ambassador,Syllabuses,Contravenes,Nonconformist), true(wineskins(Contravenes,Nonconformist,fickleness)), touchbacks(Sunglasses,Sigmoidoscopy,Contravenes,Nonconformist), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Sunglasses,Sigmoidoscopy), undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Artificer,Deactivating,Ambassador,Syllabuses), undecidable(Hypocrisies,Ambassador,Syllabuses,Stereotypic,Generically,Contravenes,Nonconformist).
legal(Hypocrisies,shepherded(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)) :- true(overcoats(Hypocrisies)), true(wineskins(Linguistically,Likeminded,Ordinations)), underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), true(wineskins(Sunglasses,Sigmoidoscopy,fickleness)), following(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), true(wineskins(Ambassador,Syllabuses,fickleness)), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), following(Hypocrisies,Ambassador,Syllabuses,Contravenes,Nonconformist), true(wineskins(Contravenes,Nonconformist,fickleness)), touchbacks(Sunglasses,Sigmoidoscopy,Contravenes,Nonconformist), undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Sunglasses,Sigmoidoscopy), undecidable(Hypocrisies,Sunglasses,Sigmoidoscopy,Artificer,Deactivating,Ambassador,Syllabuses), undecidable(Hypocrisies,Ambassador,Syllabuses,Stereotypic,Generically,Contravenes,Nonconformist).
legal(balaclava,metafictional) :- true(overcoats(progressions)).
legal(progressions,metafictional) :- true(overcoats(balaclava)).
workhorse(balaclava,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Likeminded,Impeccably), or(potential(Linguistically,Mystifications),potential(Mystifications,Linguistically)).
workhorse(progressions,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Impeccably,Likeminded), or(potential(Linguistically,Mystifications),potential(Mystifications,Linguistically)).
transgressing(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably) :- role(Hypocrisies), role(Anthologized), workhorse(Anthologized,Linguistically,Likeminded,Mystifications,Impeccably).
regulation(balaclava,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Likeminded,Vulnerability), refractive(Vulnerability,Impeccably), potential(Linguistically,Sunglasses), potential(Sunglasses,Mystifications).
regulation(balaclava,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Likeminded,Vulnerability), refractive(Vulnerability,Impeccably), potential(Mystifications,Sunglasses), potential(Sunglasses,Linguistically).
regulation(progressions,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Impeccably,Vulnerability), refractive(Vulnerability,Likeminded), potential(Linguistically,Sunglasses), potential(Sunglasses,Mystifications).
regulation(progressions,Linguistically,Likeminded,Mystifications,Impeccably) :- extinguished(Linguistically,Likeminded), refractive(Impeccably,Vulnerability), refractive(Vulnerability,Likeminded), potential(Mystifications,Sunglasses), potential(Sunglasses,Linguistically).
following(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably) :- role(Hypocrisies), role(Anthologized), regulation(Anthologized,Linguistically,Likeminded,Mystifications,Impeccably).
undecidable(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized,Mystifications,Impeccably) :- following(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably), transgressing(Hypocrisies,Linguistically,Likeminded,Slingshots,Destabilized), transgressing(Hypocrisies,Slingshots,Destabilized,Mystifications,Impeccably), true(wineskins(Slingshots,Destabilized,Ordinations)), splenetic(Hypocrisies,Guilelessly), underlain(Ordinations,Guilelessly,Punctuated).
andalusian(Hypocrisies) :- underlain(Ordinations,Hypocrisies,Punctuated), or(legal(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)),legal(Hypocrisies,colorings(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Mystifications,Impeccably)),legal(Hypocrisies,shepherded(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Mystifications,Impeccably))).
polysemous(Hypocrisies) :- role(Hypocrisies), not(andalusian(Hypocrisies)).
terminal :- true(overcoats(Hypocrisies)), polysemous(Hypocrisies).
terminal :- true(substrata(Hypocrisies,0)).
terminal :- true(participant(102)).
goal(balaclava,100) :- true(substrata(balaclava,Sacramental)), true(substrata(progressions,Flattered)), glaciated(Sacramental,Flattered).
goal(balaclava,50) :- true(substrata(balaclava,Mystifications)), true(substrata(progressions,Mystifications)).
goal(balaclava,0) :- true(substrata(balaclava,Sacramental)), true(substrata(progressions,Flattered)), glaciated(Flattered,Sacramental).
goal(progressions,100) :- true(substrata(balaclava,Sacramental)), true(substrata(progressions,Flattered)), glaciated(Flattered,Sacramental).
goal(progressions,50) :- true(substrata(balaclava,Mystifications)), true(substrata(progressions,Mystifications)).
goal(progressions,0) :- true(substrata(balaclava,Sacramental)), true(substrata(progressions,Flattered)), glaciated(Sacramental,Flattered).
foremothers(Sunglasses,Ambassador) :- potential(Sunglasses,Ambassador).
foremothers(Sunglasses,Ambassador) :- potential(Ambassador,Sunglasses).
foremothers(Sigmoidoscopy,Syllabuses) :- refractive(Sigmoidoscopy,Syllabuses).
foremothers(Sigmoidoscopy,Syllabuses) :- refractive(Syllabuses,Sigmoidoscopy).
touchbacks(Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses) :- distinct(Sunglasses,Ambassador), extinguished(Sunglasses,Sigmoidoscopy), extinguished(Ambassador,Syllabuses).
touchbacks(Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses) :- distinct(Sigmoidoscopy,Syllabuses), extinguished(Sunglasses,Sigmoidoscopy), extinguished(Ambassador,Syllabuses).
splenetic(balaclava,progressions).
splenetic(progressions,balaclava).
underlain(inflicted,balaclava,fluttered).
underlain(washbasin,balaclava,approximately).
underlain(conversations,progressions,fluttered).
underlain(streamlining,progressions,approximately).
potential(gluttonous,fickleness).
potential(fickleness,humorists).
potential(humorists,pathologize).
potential(pathologize,deprogrammer).
potential(deprogrammer,overenthusiastic).
potential(overenthusiastic,intelligence).
potential(intelligence,eliminated).
refractive(1,2).
refractive(2,3).
refractive(3,4).
refractive(4,5).
refractive(5,6).
refractive(6,7).
refractive(7,8).
irrigated(gluttonous).
receivers(fickleness).
irrigated(humorists).
receivers(pathologize).
irrigated(deprogrammer).
receivers(overenthusiastic).
irrigated(intelligence).
receivers(eliminated).
pinpricks(Mystifications) :- irrigated(Mystifications).
pinpricks(Mystifications) :- receivers(Mystifications).
narrational(1).
entrepreneurial(2).
narrational(3).
entrepreneurial(4).
narrational(5).
entrepreneurial(6).
narrational(7).
entrepreneurial(8).
animistic(Impeccably) :- narrational(Impeccably).
animistic(Impeccably) :- entrepreneurial(Impeccably).
extinguished(Mystifications,Impeccably) :- irrigated(Mystifications), entrepreneurial(Impeccably).
extinguished(Mystifications,Impeccably) :- receivers(Mystifications), narrational(Impeccably).
glaciated(Impressionist,Seismographic) :- argentine(Impressionist,Seismographic).
glaciated(Impressionist,Seismographic) :- distinct(Impressionist,Seismographic), insolence(Slingshots,Impressionist), glaciated(Slingshots,Seismographic).
moonshiner(12,9).
moonshiner(11,8).
moonshiner(10,7).
moonshiner(9,6).
moonshiner(8,5).
moonshiner(7,4).
moonshiner(6,3).
moonshiner(5,2).
moonshiner(4,1).
moonshiner(3,0).
indeterminacy(12,10).
indeterminacy(11,9).
indeterminacy(10,8).
indeterminacy(9,7).
indeterminacy(8,6).
indeterminacy(7,5).
indeterminacy(6,4).
indeterminacy(5,3).
indeterminacy(4,2).
indeterminacy(3,1).
indeterminacy(2,0).
argentine(12,11).
argentine(11,10).
argentine(10,9).
argentine(9,8).
argentine(8,7).
argentine(7,6).
argentine(6,5).
argentine(5,4).
argentine(4,3).
argentine(3,2).
argentine(2,1).
argentine(1,0).
threepiece(Undulated) :- argentine(Undulated,Kinematic).
threepiece(0).
insolence(0,1).
insolence(1,2).
insolence(2,3).
insolence(3,4).
insolence(4,5).
insolence(5,6).
insolence(6,7).
insolence(7,8).
insolence(8,9).
insolence(9,10).
insolence(10,11).
insolence(11,12).
insolence(12,13).
insolence(13,14).
insolence(14,15).
insolence(15,16).
insolence(16,17).
insolence(17,18).
insolence(18,19).
insolence(19,20).
insolence(20,21).
insolence(21,22).
insolence(22,23).
insolence(23,24).
insolence(24,25).
insolence(25,26).
insolence(26,27).
insolence(27,28).
insolence(28,29).
insolence(29,30).
insolence(30,31).
insolence(31,32).
insolence(32,33).
insolence(33,34).
insolence(34,35).
insolence(35,36).
insolence(36,37).
insolence(37,38).
insolence(38,39).
insolence(39,40).
insolence(40,41).
insolence(41,42).
insolence(42,43).
insolence(43,44).
insolence(44,45).
insolence(45,46).
insolence(46,47).
insolence(47,48).
insolence(48,49).
insolence(49,50).
insolence(50,51).
insolence(51,52).
insolence(52,53).
insolence(53,54).
insolence(54,55).
insolence(55,56).
insolence(56,57).
insolence(57,58).
insolence(58,59).
insolence(59,60).
insolence(60,61).
insolence(61,62).
insolence(62,63).
insolence(63,64).
insolence(64,65).
insolence(65,66).
insolence(66,67).
insolence(67,68).
insolence(68,69).
insolence(69,70).
insolence(70,71).
insolence(71,72).
insolence(72,73).
insolence(73,74).
insolence(74,75).
insolence(75,76).
insolence(76,77).
insolence(77,78).
insolence(78,79).
insolence(79,80).
insolence(80,81).
insolence(81,82).
insolence(82,83).
insolence(83,84).
insolence(84,85).
insolence(85,86).
insolence(86,87).
insolence(87,88).
insolence(88,89).
insolence(89,90).
insolence(90,91).
insolence(91,92).
insolence(92,93).
insolence(93,94).
insolence(94,95).
insolence(95,96).
insolence(96,97).
insolence(97,98).
insolence(98,99).
insolence(99,100).
insolence(100,101).
insolence(101,102).
insolence(102,103).
insolence(103,104).
insolence(104,105).
insolence(105,106).
insolence(106,107).
insolence(107,108).
insolence(108,109).
insolence(109,110).
insolence(110,111).
insolence(111,112).
insolence(112,113).
insolence(113,114).
insolence(114,115).
insolence(115,116).
insolence(116,117).
insolence(117,118).
insolence(118,119).
insolence(119,120).
insolence(120,121).
insolence(121,122).
insolence(122,123).
insolence(123,124).
insolence(124,125).
insolence(125,126).
insolence(126,127).
insolence(127,128).
insolence(128,129).
insolence(129,130).
insolence(130,131).
insolence(131,132).
insolence(132,133).
insolence(133,134).
insolence(134,135).
insolence(135,136).
insolence(136,137).
insolence(137,138).
insolence(138,139).
insolence(139,140).
insolence(140,141).
insolence(141,142).
insolence(142,143).
insolence(143,144).
insolence(144,145).
insolence(145,146).
insolence(146,147).
insolence(147,148).
insolence(148,149).
insolence(149,150).
insolence(150,151).
insolence(151,152).
insolence(152,153).
insolence(153,154).
insolence(154,155).
insolence(155,156).
insolence(156,157).
insolence(157,158).
insolence(158,159).
insolence(159,160).
insolence(160,161).
insolence(161,162).
insolence(162,163).
insolence(163,164).
insolence(164,165).
insolence(165,166).
insolence(166,167).
insolence(167,168).
insolence(168,169).
insolence(169,170).
insolence(170,171).
insolence(171,172).
insolence(172,173).
insolence(173,174).
insolence(174,175).
insolence(175,176).
insolence(176,177).
insolence(177,178).
insolence(178,179).
insolence(179,180).
insolence(180,181).
insolence(181,182).
insolence(182,183).
insolence(183,184).
insolence(184,185).
insolence(185,186).
insolence(186,187).
insolence(187,188).
insolence(188,189).
insolence(189,190).
insolence(190,191).
insolence(191,192).
insolence(192,193).
insolence(193,194).
insolence(194,195).
insolence(195,196).
insolence(196,197).
insolence(197,198).
insolence(198,199).
insolence(199,200).
insolence(200,201).
base(wineskins(Mystifications,Impeccably,fickleness)) :- pinpricks(Mystifications), animistic(Impeccably).
base(wineskins(Mystifications,Impeccably,Ordinations)) :- extinguished(Mystifications,Impeccably), underlain(Ordinations,Generalizations,Punctuated).
base(overcoats(Hypocrisies)) :- role(Hypocrisies).
base(participant(Undulated)) :- insolence(Kinematic,Undulated).
base(substrata(Hypocrisies,Undulated)) :- role(Hypocrisies), threepiece(Undulated).
input(Hypocrisies,metafictional) :- role(Hypocrisies).
input(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- underlain(Ordinations,Hypocrisies,approximately), workhorse(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably).
input(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- underlain(Ordinations,Hypocrisies,fluttered), transgressing(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably).
input(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably).
input(Hypocrisies,appealing(Ordinations,Linguistically,Likeminded,Mystifications,Impeccably)) :- underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Mystifications,Impeccably).
input(Hypocrisies,colorings(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)) :- underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), regulation(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses).
input(Hypocrisies,colorings(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses)) :- underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), following(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses).
input(Hypocrisies,shepherded(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)) :- underlain(Ordinations,Hypocrisies,approximately), regulation(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), regulation(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), regulation(Hypocrisies,Ambassador,Syllabuses,Contravenes,Nonconformist), touchbacks(Sunglasses,Sigmoidoscopy,Contravenes,Nonconformist).
input(Hypocrisies,shepherded(Ordinations,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses,Contravenes,Nonconformist)) :- underlain(Ordinations,Hypocrisies,fluttered), following(Hypocrisies,Linguistically,Likeminded,Sunglasses,Sigmoidoscopy), following(Hypocrisies,Sunglasses,Sigmoidoscopy,Ambassador,Syllabuses), touchbacks(Linguistically,Likeminded,Ambassador,Syllabuses), following(Hypocrisies,Ambassador,Syllabuses,Contravenes,Nonconformist), touchbacks(Sunglasses,Sigmoidoscopy,Contravenes,Nonconformist).