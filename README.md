# Botten Adas hem
===

Ada är en bayesiansk modell som räknar ut sannolikheten för olika valresultat. Här finns den data som Ada användr sig av. En del av denna data kommer från [Novus](http://www.novus.se/vaeljaropinionen/ekotnovus-poll-of-polls.aspx) sammanställning men sedan april har vi uppdaterat materialet själva.


### Adas motor (tekniska detaljer)
 
Ada bygger på en [multivariat dynamisk linjär bayesiansk modell](http://www.jstatsoft.org/v36/i12/paper). Denna typ av modell utgår från att opinionsundersökningar mäter en underliggande ”sann” partisympatinivå för varje parti med ett visst slumpmässigt mätfel. I stort är det samma typ av modell som används i ett [Kalman filter](http://sv.wikipedia.org/wiki/Kalmanfilter).

Ada modellerar partisympatierna per dag. Varje opinionsundersökning delas upp på de dagar undersökningsperioden pågår och sedan vägs olika opinionsundersökningar ihop för respektive dag. Detta innebär att både undersökningsperiodens längd och urvalsstorleken betraktas i modellen. I modellen antas att ungefär lika många intervjuer görs varje dag. Vi antar också att varje parti i opinionsmätningen är oberoende av de andra partisympatierna vilket inte stämmer då modellen de facto är multinomial. Anledningen till detta är att vid ett antagande om multinomialmodell blir kovariansmatrsien singulär om partiernas proportioner summerar till 1.

Utöver detta så skattas även ”house effects” på ett liknande sätt som i [följande artikel](http://linkinghub.elsevier.com/retrieve/pii/S0261379410000946). En skillnad ligger i att  dessa ”house effects” står i relation till SCB:s mätningar i stället för tidigare val. Detta innebär att vi antar att SCB:s partisympatimätningar är "unbiased".
 
De underliggande partisympatier förändras sedan över tid genom slumpmässiga fluktuationer varje dag. Dessa fluktuationer sker dock inte helt slumpmässigt utan fluktuationerna beror på hur de olika partierna har samvarierat tidigare under perioden baserat på en skattad kovariansmatris. Denna typ av modell brukar kallas för [”multivariate local level model”](http://books.google.com/books?hl=en&lr=&id=VCt3zVq8TO8C&oi=fnd&pg=PA1&dq=Dynamic+linear+models+with+R&ots=PW78ub8fxV&sig=jvwM_ed56JCaitUWTAv3rUDjM2E) och är en liknande modell som används i [bayesianska poll of polls](http://eppsac.utdallas.edu/files/jackman/CAJP%2040-4%20Jackman.pdf).

För att göra prediktioner för riksdagsvalet körs sedan modellen ”baklänges” på ett liknande sätt som i [Votamatic](http://votamatic.org/). Syftet med detta är att kunna lägga till en strukturell prediktion av resultatet på valdagen i modellen. Ju närmare valet vi kommer desto mer vikt kommer modellen lägga på sammanvägningen av opinionsundersökningarna och mindre på den strukturella prediktionen. Detta beror dock på hur mycket den skilda partierna rör sig under perioden 1,5 år innan valet, partier som rör sig långsammare kommer lita mer på opinionsundersökningar och de som rör sig mycket i opinionen kommer vägas mer mot den strukturella prediktionen på valdagen. Det innebär att osäkerheten är olika stor för olika partier.

För den strukturella prediktionen har framförallt SCB:s majundersökning använts med vissa mindre korrigeringar. Vi har undersökt hur mycket olika partier har varierat mellan SCB:s majmätning och riksdagsvalen sedan 1970-talet och använt denna information för att lägga vår strukturella prediktion av valresultatet. Framförallt har vi på detta sätt uppskattat hur osäker SCB:s majundersökning är för olika partier. 

Vi har tittat på andra prediktorer utöver SCB:s majundersökning som arbetslöshetsstatistik och BNP (som använts mycket i USA) men dessa har inte visat sig ge någon extra prediktiv styrka i Sverige utöver majundersökningen. Sammantaget innebär detta att vi lägger en strukturell prediktion på att mindre partier kommer att öka i slutet av valrörelsen och att de större partierna minskar något. Minskningen beror på hur partierna har samvarierat under perioden 1,5 år innan valdagen.
 
Modellen skattas med Markov chain monte carlo (MCMC) och då framförallt med Gibbs sampling (med en burnin på 500, thining på 4 och 2000 MCMC samples). Med undantag från priorn på valdagen är samtliga priors vaga (vague) men korrekta (proper).
 
### Adas brister
Modellen svaghet just nu är dels att vi inte på ett naturligt sätt kan inkludera FI i modellen då det saknas historisk opinionsdata för att få en bilda av hur FI samvarierar med andra partier. 

En annan svaghet är att modellen som sådan inte fullt tar hänsyn till att de mindre partierna daglig variation ökar under valrörelsen. Det gör att strukturella prediktioner har behövt extrapolera den strukturella prediktionen för att få in denna valrörelseeffekt. 

En sista svaghet med modellen är att sannolikheten för extrema utfall inte modelleras särskilt bra då modellen bygger på normalfördelningantaganden. För mindre partier skulle en multivariat t-fördelning eller multivariat gammafördelning troligtvis vara bättre för att fånga in extrema utfall bland mindre partier (som KD 1998, FP 2002 och FP 1985).
