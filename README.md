Ada
===

### Adas motor
 
Ada bygger på en (multivariat dynamisk linjär modell)[http://www.jstatsoft.org/v36/i12/paper]. Denna typ av modell utgår från att opinionsundersökningar mäter en underliggande ”sann” partisympatinivå för varje parti. I stort är det samma typ av modell som används i ett (Kalman filter)[http://sv.wikipedia.org/wiki/Kalmanfilter].

Ada modellerar partisympatierna per dag, vilket innebär att varje opinionsundersökning delas upp på de dagar undersökningsperioden pågår och sedan vägs olika opinionsundersökningar ihop för respektive dag. Detta innebär att både undersökningsperiodens längd och urvalsstorleken betraktas i modellen under antagandet om att ungefär lika många intervjuer görs varje dag.

Utöver detta så skattas även ”house effects” på ett liknande sätt som i följande (artikel)[http://linkinghub.elsevier.com/retrieve/pii/S0261379410000946]. En skillnad ligger är dock att dessa ”house effects” står i relation till SCB:s mätningar i stället för tidigare val. Detta innebär att vi antar att SCB:s mätning är "unbiased".
 
De underliggande partisympatier förändras sedan över tid genom slumpmässiga fluktuationer varje dag. Dessa fluktuationer sker dock inte helt slumpmässigt utan fluktuationerna beror på hur de olika partierna har samvarierat tidigare under perioden. Denna typ av modell brukar kallas för (”multivariate local level model”)[http://books.google.com/books?hl=en&lr=&id=VCt3zVq8TO8C&oi=fnd&pg=PA1&dq=Dynamic+linear+models+with+R&ots=PW78ub8fxV&sig=jvwM_ed56JCaitUWTAv3rUDjM2E]
 
För att göra prediktioner för riksdagsvalet körs sedan modellen ”baklänges” på ett liknande sätt som i (Votamaticmodellen)[http://votamatic.org/]. Syftet med detta är att kunna lägga till en strukturell prediktion av resultatet på valdagen i modellen. Ju närmare valet vi kommer desto mer vikt kommer modellen lägga på sammanvägningen av opinionsundersökningarna och mindre på den strukturella prediktionen.
 
För den strukturella prediktionen har framförallt SCB:s majundersökning använts med vissa mindre korrigeringar. Vi har undersökt hur mycket olika partier har varierat mellan SCB:s majmätning och riksdagsvalen sedan 1970-talet använt denna information för att lägga vår strukturella prediktion av valresultatet med tillhörande osäkerhet. 

Vi har tittat på andra prediktorer som arbetslöshetsstatistik och BNP (som använts mycket i USA) men dessa har inte visat sig ge någon extra prediktiv styrka i Sverige. Detta innebär att vi bland annat lägger en strukturell prediktion på att mindre partier kommer att öka i slutet av valrörelsen och att de större partierna minskar något.
 
Modellen skattas med Markov chain monte carlo (MCMC) och då framförallt med Gibbs sampling (med en burnin på 500, thining på 4 och 2000 MCMC samples). Med undantag från priorn på valdagen är samtliga priors vaga.
 
Modellen svaghet just nu är dels att vi inte på ett naturligt sätt kan inkludera FI i modellen då det saknas historisk opinionsdata. En annan svaghet är att modellen som sådan inte fullt tar hänsyn till att de mindre partierna daglig variation ökar under valrörelsen. Det gör att strukturella prediktioner har behövt extrapolera den strukturella prediktionen för att få in denna valrörelseeffekt. 
