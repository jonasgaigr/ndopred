**ODBORNÁ REŠERŠE  •  NÁVRH METODIKY**

# **Automatizované hodnocení ohroženosti druhů podle IUCN**

## *Rešerše algoritmických přístupů a návrh jejich využití nad Nálezovou databází ochrany přírody (NDOP)*

**Zaměření: české červené seznamy, prioritně bezobratlí a hmyz**

Stav literatury a zdrojů: 13\. října 2026  •  Verze 1.0

Charakter materiálu: pracovní metodický návrh; není oficiálně schválenou metodikou AOPK ČR ani IUCN.

| Základní závěr Pro NDOP je nejvhodnější hybridní systém: dávkové a reprodukovatelné výpočty prostorových parametrů (EOO/AOO), podmíněně modely trendu korigující nerovnoměrné mapovací úsilí a pravidlové vyhodnocení IUCN. Výsledkem je předběžná kategorie s doloženým kritériem a mírou jistoty, nikoli neověřená finální klasifikace. |
| :---- |

# **Manažerské shrnutí**

* Nevytvářet jeden „černoskříňový“ algoritmus, který by převáděl počet nálezů nebo čtverců přímo na kategorii CR/EN/VU. Zvlášť ne v NDOP, která obsahuje směs cílených, oportunistických a historických záznamů \[S1, P14, P16\].  
* Pravidlový modul má vyhodnocovat kritéria A–E odděleně; v první etapě je realistické plnohodnotně implementovat výpočty pro B1/B2 a podpůrné indikátory A2(c), případně D2; bez důkazů pro podkritéria neuzavírat kategorii \[I1, I2\].  
* Základ prostorových výpočtů tvoří 2×2 km AOO v jednotné síti a EOO vypočítané z vhodně kontrolovaných aktuálních výskytů; chybějící nálezy a nepravidelné mapování aktivně vyhodnocovat \[P01, P06, P22\].  
* Trendy primárně odhadovat z opakovaných, dohledatelných průzkumných akcí a druhových seznamů pomocí occupancy/detection modelů; „absence záznamu“ sama o sobě není záznamem absence \[P16, P17, P20, P21\].  
* ConR využít jako referenční implementaci pro hromadné předběžné hodnocení, redlistr pro kontrolované prostorové a časové metriky a sRedList k expertnímu došetření vybraných druhů; není nutné převzít žádný z nástrojů jako jediný systém \[P01–P04\].  
* Spustit pilot na dvou různě dobře prozkoumaných skupinách hmyzu, vyhodnotit falešné CR/EN/VU i falešné LC, ověřit stabilitu vůči filtrování nálezů a testovat shodu s nezávislými specialisty \[P09, P14, P22\].

# **Orientační obsah**

1\. Normativní rámec  •  2\. Podstata dat NDOP  •  3\. Srovnání metod  •  4\. Doporučená architektura  •  5\. Kritéria a pravidlové rozhodování  •  6\. Prostorová data a nejistota  •  7\. Trendy a effort bias  •  8\. Pilot a validace  •  9\. Implementace a výstupy  •  10\. Anotovaná bibliografie

# **1 | Normativní rámec: co algoritmus smí rozhodnout**

Referenčními dokumenty jsou IUCN Red List Categories and Criteria, verze 3.1, a Guidelines for Using the IUCN Red List Categories and Criteria, verze 16 (březen 2024); pro ČR dále Guidelines for Application ... at Regional and National Levels, verze 4.0 (2012) \[I1–I3\]. Kritéria IUCN slouží k posouzení rizika vymizení, nikoli k určování právního režimu druhové ochrany.

Automatizace musí rozlišovat (i) spočtený parametr, (ii) ověřené naplnění dílčí podmínky, (iii) předběžnou kategorii a (iv) finální schválené regionální hodnocení. Každý krok musí mít vlastní datový podklad, časové vymezení a původ rozhodnutí. Při naplnění několika kritérií se zdokumentují všechna relevantní kritéria a použije nejvyšší oprávněná kategorie ohrožení.

| Nejčastější metodická chyba Kritérium B není pravidlo „AOO \< 500 km² ⇒ EN“. Musí být splněn příslušný práh B1 a/nebo B2 A ZÁROVEŇ alespoň dvě z podmínek: (a) silná fragmentace nebo omezený počet ohrožením definovaných locations, (b) pokračující pokles, (c) extrémní fluktuace \[I1–I3\]. |
| :---- |

*Tabulka 1\. Prahové hodnoty kritéria B (verze 3.1).*

| Kategorie | B1: EOO | B2: AOO | B(a): locations | Podmínka navíc |
| :---- | :---- | :---- | :---- | :---- |
| CR | \< 100 km² | \< 10 km² | \= 1 | Alespoň 2 z (a), (b), (c) |
| EN | \< 5 000 km² | \< 500 km² | ≤ 5 | Alespoň 2 z (a), (b), (c) |
| VU | \< 20 000 km² | \< 2 000 km² | ≤ 10 | Alespoň 2 z (a), (b), (c) |

„Location“ není bod nálezu, kolonie, mapovací čtverec ani automaticky odvozený cluster. Jde o geograficky nebo ekologicky odlišenou oblast, v níž může jedna závažná hrozba rychle zasáhnout všechny přítomné jedince. Hodnocení silné fragmentace má rovněž vlastní biologické podmínky. Automatický počet klastrů smí být pouze podpůrnou metrikou.

Regionální metodika vyžaduje odděleně určit, zda je taxon pro hodnocení způsobilý (původní a etablovaný výskyt, nikoli nahodilý zatoulaný jedinec), následně nejprve spočítat národní kategorii a teprve poté posoudit vliv populací za hranicemi a možný „rescue effect“ \[I2\]. RE, DD, NA a NE nejsou náhradou za nesplněné prahy B.

# **2 | Co přesně představují data NDOP**

AOPK ČR uvádí, že NDOP zahrnuje georeferencované a datované nálezy živočichů, rostlin i hub: vlastní průzkumy AOPK, data externích institucí i veřejnosti a převzaté údaje z publikací, sbírek či zpráv. Přesnost historických a současných údajů se zásadně liší; těžiště sběru tvoří druhy významné pro ochranu přírody \[S1–S2\]. To je důležitá vlastnost, nikoli datová chyba: systém však nelze považovat za reprezentativní náhodný monitoring.

*Tabulka 2\. Minimální vrstva analytických dat; názvy polí jsou navržené, nikoliv ověřené názvy atributů NDOP.*

| Oblast | Minimální atributy / odvozené příznaky | Význam pro IUCN |
| :---- | :---- | :---- |
| Identita | taxon\_id, taxon\_name, accepted\_taxon\_id, determination\_quality | Taxonomická validace; řazení synonym a agregátů |
| Prostor | geometry, typ zákresu, precision\_m, CRS, region, 2×2 km cell | EOO/AOO; prostorová nejistota a duplicity |
| Čas | date\_from, date\_to, date\_precision, observation\_year | Periodizace; nemožnost určit přesný rok |
| Původ a kvalita | record\_id, source\_id, original\_id, observer, validator, evidence, status | Audit, detekce převzatých duplicit a zpochybnitelných záznamů |
| Průzkumné úsilí | event\_id, visit\_id, protocol, checklist\_id, effort, target\_group | Model detekovatelnosti; podklad pro legitimní nedetekce |
| Biologie | life\_stage, reproduction\_status, count, count\_unit, native\_status | Vhodnost pro populační odhady; pravidla NA/RE |
| Analýza | include\_B, include\_trend, exclusion\_reason, dataset\_snapshot | Reprodukovatelnost a zdokumentované výjimky |

Před zavedením je nutné porovnat navržené položky s reálným exportním schématem NDOP a oprávněním ke zpracování citlivých lokalit. Z dostupné veřejné dokumentace nelze potvrdit, že všechny potřebné atributy (zejména event\_id, checklist a effort) v NDOP skutečně existují a jsou systematicky vyplňovány.

# **3 | Srovnání existujících přístupů pro NDOP**

*Tabulka 3\. Hodnocení použitelnosti nástrojů (analytický úsudek, nikoli empiricky naměřená přesnost na NDOP).*

| Nástroj | Přednost | Omezení | Role |
| :---- | :---- | :---- | :---- |
| ConR \[P01\] | Dávkové předběžné hodnocení; EOO/AOO, lokality a další metriky | Kategorie závisí na vstupních předpokladech; revize nutná; CRAN distribuce přerušena | Referenční benchmark \+ ověřené dílčí funkce |
| redlistr \[P04\] | Prostorové a časové parametry; aktuální verze 2.1.0 (07/2026) | Není hotový národní expertní workflow | Výpočet a QA prostorových a trendových ukazatelů |
| sRedList \[P02\] | Soubory výskytů lze importovat; habitat, fragmentace, expertní workflow | Webové prostředí; prověřit správu citlivých dat a interoperabilitu | Podpora specialistů a kontroly případů |
| red \[P03\] | GIS, EOO/AOO, SDM a nejistoty | Vlastní metodické rozhodování nutno doprogramovat | Alternativní kontrola metod |
| GeoCAT \[P05\] | Přehledná kontrola EOO/AOO pro jeden druh | Nevhodný jako hlavní dávkový motor | Nezávislá kontrola vzorku druhů |
| Rapid LC \[P06\] | Triáž zjevně rozšířených druhů | Hrozí mylná LC u málo mapovaných druhů | Pouze nízkoprioritní předvýběr k revizi |
| IUCNN / ML \[P07–P09\] | Prioritizace druhů a detekce nesouladů | Není důkaz splnění konkrétních kritérií | Sekundární „red flag“ model, ne rozhodovací motor |

Poznámka k aktuálnosti: ConR je podle autorského repozitáře dostupné z GitHubu, nikoli standardně z CRAN (kvůli vyřazení starších závislostí). redlistr 2.1.0 již používá sf a terra. sRedList včetně uploadu nálezových dat a podpory národních seznamů existuje jako provozní platforma \[T1–T3\].

# **4 | Doporučená architektura řešení**

1. DATOVÝ SNAPSHOT: export NDOP s časovým razítkem a verzí taxonomického číselníku; beze změn uchovat originální podklad a auditní stopu.  
2. VALIDAČNÍ VRSTVA: taxonomie, prostor, datum, věrohodnost, původnost, duplicitní publikace či sbírkové údaje; automatické „flagy“, nikoli nevratné mazání.  
3. VRSTVA NÁLEZOVÝCH UDÁLOSTÍ: odděleně vytvořit unikátní záznamy výskytu, jednotlivé návštěvy/průzkumné akce a případné kompletní druhové seznamy. Neslučovat tyto tři entity.  
4. GEOPROCESSING: EOO, AOO, alternativní scénáře přesnosti, atributy hranice ČR, změny biotopu a prostorově explicitní hrozby.  
5. TRENDOVÝ MODUL: podle dostupnosti dat occupancy/detection model, standardizované abundance indexy, případně omezené indikátory změny při důsledném označení jejich interpretace.  
6. PRAVIDLOVÝ IUCN ENGINE: po jednotlivých kritériích a podkritériích uložit „splněno / nesplněno / neznámé“, hodnotu, důkaz, autora ověření a interval nejistoty.  
7. EXPERTNÍ ROZHRANÍ: verifikace threats, fragmentation, locations, regionálního rescue effect a závěrečná změna kategorie s povinným odůvodněním.  
8. EXPORT: druhová karta, tabulka pro červený seznam, standardizované GIS vrstvy, protokol nejistot a historii všech rozhodnutí.

| Zásada oddělených odpovědností Počítač vypočítá, co lze vypočítat z doložených údajů. Expert rozhodne o tom, co není v datech přímo pozorované. Software obojí zaznamená včetně identifikace konkrétního důkazu. Toto je důležitější než výběr konkrétního R balíčku. |
| :---- |

# **5 | Matice kritérií IUCN: co je u NDOP automatizovatelné**

*Tabulka 4\. Reálný rozsah automatizace při dostupnosti převážně nálezových dat.*

| Kritérium | Automatická část | Co nelze bez další evidence | Praktická priorita |
| :---- | :---- | :---- | :---- |
| A: pokles | Trendy abundancí z monitoringu; model obsazenosti; změny EOO/AOO/biotopu | Převod obsazenosti na pokles dospělých jedinců; generace; příčiny; správná dílčí varianta | Vysoká jen u skupin s dostatečnými řadami |
| B1: EOO | Konvexní obálka, odhady v čase, citlivost na záznamy | (a) locations/fragmentace; (b) pokračující pokles; (c) extrémní fluktuace | Vysoká |
| B2: AOO | 2×2 km rastr; pozorovaná a korigovaná AOO; nejistota | Stejné podmínky jako B1; nezjištěné obsazené buňky | Velmi vysoká |
| C: malé populace | Součty dospělých z validního populačního monitoringu | Úplnost územního pokrytí, subpopulace, velikost národní populace | Nízká ve většině hmyzu |
| D1 a D2 | Pro D2 prostorová omezenost a scénáře hrozeb; pro D1 jen při odhadu počtu jedinců | Vážná pravděpodobná hrozba pro D2; správný odhad dospělých pro D1 | Střední (D2 pouze s expertní evidencí) |
| E: analýza vyhynutí | Modely životaschopnosti při kompletních datech | Parametrizace a kalibrace kvantitativního modelu | Výjimečná |

Kritérium D2 (pouze VU) typicky pracuje s velmi omezenou AOO (obvykle \< 20 km²) nebo počtem locations (obvykle ≤ 5\) v kombinaci s věrohodnými dopady hrozeb, které by taxon mohly ve velmi krátké době posunout do CR nebo EX; samotná hodnota AOO D2 neprokazuje \[I1\].

# **6 | Prostorové hodnocení B: doporučená implementace**

## **6.1 Filtrace a určení skutečného areálu**

* Před výpočtem oddělit výskyty aktuální, historické, nepůvodní, nahodilé, sporně determinované a prostorově neurčité. Pravidlo „aktuální výskyt“ není univerzální „10 let“ pro všechny taxony; nutno jej stanovit biologicky a dokumentovat.  
* Do národního hodnocení nevkládat automaticky všechny zahraniční záznamy ani bezhlavě ořezávat globální areál hranicí ČR. Regionální EOO se počítá z rozšíření v hodnoceném regionu podle doporučení IUCN; přeshraniční kontext se uplatní zejména při regionální úpravě.  
* U pár bodů (zejména 1–2) nedovozovat EOO přímo z plochy nulové konvexní obálky; vyžádat vhodnou aproximaci či ponechat parametr jako nejistý. U výběžků areálu zvažovat reálnost krajních nálezů.  
* Zachovávat číselné hodnoty bez umělého zaokrouhlování a vedle nich uvádět varianty (jisté/pravděpodobné výskyty; scénáře nejisté polohy). Je-li EOO vypočtené menší než AOO, podle metodiky IUCN se EOO pro hodnocení upraví tak, aby nebylo menší než AOO; zdokumentovat původní i upravenou hodnotu.

## **6.2 AOO: standardní 2 × 2 km a důležitost úplnosti**

Vytvořit jedinou pevně definovanou síť 2 × 2 km v metrickém souřadnicovém systému; spočítat počet obsazených buněk × 4 km². Síť, její počátek, transformaci souřadnic a analytickou verzi archivovat. Doplňkově spočítat citlivost na posun sítě a lokalizační nejistotu; nesmí se měnit měřítko AOO mezi druhy nebo obdobími. Při polygonových nebo hrubě lokalizovaných údajích nepočítat centroid jako jisté obsazení jediného čtverce \[I1, P22, P25\].

AOO určená jen ze zjištěných buněk je u málo mapovaných druhů často dolní mez, nikoli dostatečný odhad skutečné AOO. Marsh et al. (2023) ukazují, že čtvercová AOO může být téměř funkcí počtu záznamů. Vhodný je vedle pozorované AOO i alternativní odhad přes habitat-maskovanou mapu × prevalenci, pokud jsou pro danou skupinu podklady \[P22, P25\].

## **6.3 Prostorová podkritéria B(a–c)**

B(a): nepřevádět shluk nálezů ve vzdálenosti 100 m, 500 m či 1 km na počet locations. Výpočet může odvodit počet lokálních výskytových clusterů nebo izolaci fragmentů, ale pro IUCN locations je nutné definovat hrozbu a rozsah jediného možného zásahu. B(b): průběžný pokles může být přímo pozorován, odvozen nebo předpovězen; prokazuje se cílenými trendy stanoviště, AOO, EOO, lokalit či dospělých jedinců. B(c): extrémní fluktuace se týkají magnitude výkyvů parametrů dle IUCN, nikoli obyčejné variability návštěvnosti databáze.

# **7 | Časové trendy: nejsložitější část pro NDOP**

## **7.1 Proč prosté „před / po“ selhává**

Srovnání počtu nálezů nebo obsazených buněk ve dvou časových intervalech zaměňuje ekologický trend se změnou pozorovatelského úsilí, metodik a taxonomických znalostí. Navíc trpí „target-species bias“: ohrožený druh může být cíleně hledán častěji než před 15 lety. Isaac et al. (2014), Maes et al. (2015), van Strien et al. (2013) a Powney et al. (2019) nabízejí ověřené postupy, jak tyto problémy alespoň částečně řešit \[P16, P17, P20, P21\].

## **7.2 Datové úrovně použitelné pro trend**

*Tabulka 5\. Praktická stupnice analytické kvality (navržená pro pilot; nelze ji zaměnit za oficiální IUCN klasifikaci).*

| Úroveň | Jaká data jsou k dispozici | Přípustný závěr |
| :---- | :---- | :---- |
| T0 | Izolované nálezy, bez evidence návštěv a průzkumného úsilí | Mapa pozorovaných výskytů; nikoli populační trend |
| T1 | Opakované nálezy \+ přibližný proxy effort (počet všech záznamů relevantní skupiny) | Explorativní trend / priorita došetření; velké riziko zkreslení |
| T2 | Opakované návštěvy míst, konzistentní protokol nebo věrohodné checklisty | Hierarchický occupancy/detection model; trend obsazenosti s nejistotou |
| T3 | Standardizované každoroční početní transekty, pasti, definovaná jednotka úsilí | Index abundance; potenciální podklad A1/A2(b) podle splnění podmínek, C/D při zvláštní evidenci |

Absence záznamu není nedetekce z provedené návštěvy; negativní pozorování vzniká až při známém průzkumném úsilí v relevantním místě a termínu pro danou taxonomickou skupinu. Pokud NDOP běžně neuchovává kompletní druhové seznamy a vazbu nález–akce, je potřeba tyto informace doplnit z konkrétních monitorovacích projektů či metodicky rekonstruovat pouze u vhodných zdrojů.

## **7.3 Doporučený model**

U opakovaných návštěv vyhodnotit roční pravděpodobnost obsazenosti ψ(i,t) a podmíněnou detekci p(i,t,v). Proměnné detekce: pozorovatel, datum, počasí, délka akce, metoda, zdroj; proměnné obsazenosti: biotop, region, nadmořská výška, rok, prostorové efekty. U dat se silnou prostorovou autokorelací či mnoha druhy je vhodný hierarchický více-druhový model. Alternativou jsou standardizované trendové modely a Frescalo dle možnosti vstupních dat \[P16, P17\].

Důležité omezení: pokles occupancy indexu neprokazuje automaticky stejně velký pokles počtu dospělých jedinců pro A2(b). Pro A2(c) se může z poklesu AOO/EOO/kvality stanovišť usuzovat na redukci populace, ale vazba a její míra musí být biologicky podložena. Fox et al. (2019) prokázali velkou citlivost desetiletých trendů motýlů a můr na zvolený začátek období; proto doporučujeme reportovat i delší kontext a intervaly nejistoty \[P18, P28\].

## **7.4 Časové periody a citlivost**

Pro hodnocení vydávané v roce 2026 lze jako příklad uzavřeného intervalu použít 2016–2025, je-li rok 2026 neúplně pokryt. Pro kritérium A je ovšem rozhodující delší z 10 let nebo 3 generací a volba intervalu i typu A1/A2/A3/A4 musí splnit formální definici IUCN. Srovnání 2006–2015 vs. 2016–2025 uvádět jen jako doplňkovou diagnostiku, nikoli automatický ekvivalent A2. U vícegeneračních či nepravidelně přítomných druhů nastavit individuální výjimku.

# **8 | Návrh rozhodovacího algoritmu a evidence**

Každá podmínka v modelu musí mít tři logické stavy TRUE / FALSE / UNKNOWN. Hodnota UNKNOWN není FALSE: není přípustné automaticky přiřadit LC jen proto, že chybí podklady pro prokázání ohroženosti. Výstup obsahuje odděleně potvrzenou kategorii, podmíněný návrh kategorie a potřebu došetření.

*Schéma rozhodování pro kritérium B (pseudokód, pouze ilustrace; finální logiku je nutné odzkoušet proti IUCN Guidelines).*

for taxon in assessed\_taxa:  
    inputs \= validated\_eoo\_aoo(taxon, assessed\_region)  
    for level in \[CR, EN, VU\]:  
        range\_ok \= (EOO \< B1\_threshold\[level\]) OR (AOO \< B2\_threshold\[level\])  
        a \= confirmed\_severe\_fragmentation OR confirmed\_threat\_defined\_locations(level)  
        b \= evidenced\_continuing\_decline  \# e.g. habitat, AOO, EOO, locations  
        c \= evidenced\_extreme\_fluctuations  
        qualifying \= range\_ok AND at\_least\_two\_confirmed(a, b, c)  
        \# unknown evidence \-\> provisional 'possible', never silently FALSE  
        store(level, qualifying, evidence, unknowns, uncertainty)  
    apply\_other\_independently\_evaluated\_criteria(A, C, D, E)  
    assess\_regional\_adjustment\_and\_document()  
    require\_expert\_approval\_before\_final\_list()

*Tabulka 6\. Příklad dvou hypotetických druhů (čistě ilustrační).*

| Příklad | Vypočteno | Doloženo | Závěr |
| :---- | :---- | :---- | :---- |
| Druh X | EOO 1 700 km²; AOO 84 km² | 2 ohrožením definované locations \+ prokázaný pokračující pokles biotopu | Může splnit EN B1ab(iii)+B2ab(iii), po revizi a regionální úpravě |
| Druh Y | EOO 1 700 km²; AOO 84 km² | Locations neznámé, pokračující pokles neprokázán | Z malého EOO/AOO NELZE sám odvodit EN; nutné doplnit evidenci |

V druhové kartě má být pro každé kritérium zobrazeno: zdrojové ID nálezů, podklady o metodě a hrozbě, odhad, interval/varianty, provedená pravidla, autorské rozhodnutí, verze použité metodiky a seznam otevřených námitek. Nálezová data citlivých druhů se v publikovaném exportu mohou agregovat, avšak auditní výpočty potřebují autorizované přesné podklady.

# **9 | Pilotní validace, harmonogram a kritéria úspěchu**

Doporučená kombinace: (a) dobře monitorovaná skupina denních motýlů nebo jiný systematicky sledovaný soubor, (b) obtížněji mapovaná skupina brouků, např. saproxyličtí brouci. Přesné skupiny určí dostupnost reprezentativních dat a ochota příslušných specialistů provést nezávislé posouzení.

*Tabulka 7\. Pilotní postup a ověřitelné výstupy.*

| Fáze | Úkol | Výsledek / rozhodovací bod |
| :---- | :---- | :---- |
| 1\. Audit | Vzorek dat \+ skutečná struktura NDOP; synonymie, typy zákresů, lokalizace, vztah event–record | Datový slovník, analýza chyb a podíl záznamů použitelné pro B a pro trendy |
| 2\. Prototyp B | EOO, AOO, varianty nejistoty, hrozby, rules engine; porovnání ConR/redlistr | Reprodukovatelné podklady pro 2 pilotní skupiny |
| 3\. Trendy | Rozdělení dat na T0–T3; model detection/occupancy, jen kde validní | Odhady trendů s intervaly a zásadními předpoklady |
| 4\. Ověření | Zaslepené hodnocení experty \+ rozbor neshod | Matice záměn CR/EN/VU vs. NT/LC/DD; citlivost, specificita, neshody |
| 5\. Rozšíření | Formalizace workflow, školení, export druhových karet | Pilotní metodika \+ návrh údržby a správy verzí |

Měřit zvlášť falešné „ohrožené“ a falešné „neohrožené“ klasifikace; nestačí obecná přesnost přes všechny druhy. Testovat stabilitu při vyřazení neověřených pozorování, historických zdrojů, změně prostorové přesnosti, rozdílech mezi krajinnými oblastmi i při výpadku části dat. U prediktivních ML modelů důsledně oddělit trénovací a testovací druhy a taxonomicky či prostorově blokovat validaci \[P09, P22, P23\].

| Rozhodnutí o nasazení Jako závaznou kategorii smí systém zveřejnit jen expertně schválený výsledek. U ostatních vracet „návrh / vyžaduje kontrolu“ nebo „nedostatek údajů k vyhodnocení“, nikoliv automaticky DD či LC. V pilotu jde nejprve o zkrácení analytické přípravy a větší konzistenci, nikoli o co nejvyšší podíl automaticky přidělených kategorií. |
| :---- |

# **10 | Doporučené řešení a otevřená rozhodnutí pro AOPK ČR**

Navrhuji budovat centrální metodický „assessment engine“ v R (sf, terra, dplyr, případně redlistr/ConR) nad kontrolovanými exporty NDOP, s referenční verzí všech vstupů. Vlastní rozhodovací logiku navrhuji psát jako transparentní sadu testovaných pravidel podle verzovaných kritérií IUCN. Uživatelé GIS mohou nad těmito výsledky pracovat v QGIS; sRedList lze využít k odbornému doplnění parametrů a kontrole vybraných druhů.

* Datová architektura: je možné spojit druhové nálezy se skutečnou „návštěvou“ a kompletním seznamem zaznamenaných druhů? Pokud ne, trendové modely se spustí jen pro externí monitoringové soubory.  
* Standard aktuálnosti: potřebuje každá taxonomická skupina jiné pravidlo, a jak jej propojit s generací, diapauzou a schopností opakované detekce?  
* Zdroj threats: kdo udrží standardizovaný číselník hrozeb na lokalitách či biotopech, časovou platnost a vazbu hrozba → threat-defined location?  
* Regionální proces: kdo formálně schvaluje způsobilost taxonu, rescue effect, změny kategorie a periodicitu aktualizace?  
* Způsob validace: jaký nezávislý referenční soubor druhů umožní srovnání bez toho, aby algoritmus pouze reprodukoval historické subjektivní hodnocení?

Praktické pořadí investic: 1\) kvalitní datové rozhraní NDOP a auditní stopa, 2\) kritérium B s nejistotou, 3\) strukturovaný formulář důkazů pro podkritéria, 4\) trendové modely pro vybrané druhové skupiny, 5\) volitelná predikce ML jako systém priorit. Nezačínat samostatným neurálním klasifikátorem kategorií.

# **11 | Anotovaná bibliografie s aktivními odkazy**

Citace v textu odkazují na následující zdroje. DOI nebo oficiální URL jsou uvedeny jako aktivní odkazy; položky jsou zařazeny podle metodické role. U studií založených na modelových taxonech je nutné rozlišit doložený závěr autorů a jeho navrhovaný přenos do NDOP.

## **OFICIÁLNÍ RÁMEC**

**\[I1\]** IUCN Standards and Petitions Committee (2024). Guidelines for Using the IUCN Red List Categories and Criteria. Version 16\. IUCN. [Otevřít zdroj ↗](https://nrl.iucnredlist.org/resources/redlistguidelines)

Využitelnost pro NDOP: Základ definic kritérií A–E, nejistoty, EOO/AOO, locations, pokračujícího poklesu a evidence.

**\[I2\]** IUCN (2012). Guidelines for Application of IUCN Red List Criteria at Regional and National Levels. Version 4.0. [Otevřít zdroj ↗](https://iucn.org/resources/publication/guidelines-application-iucn-red-list-criteria-regional-and-national-levels)

Využitelnost pro NDOP: Národní hodnocení, způsobilost taxonů a regionální korekce podle sousedních populací.

**\[I3\]** IUCN (2001, 2nd ed. 2012). IUCN Red List Categories and Criteria. Version 3.1. [Otevřít zdroj ↗](https://portals.iucn.org/library/content/iucn-red-list-categories-and-criteria-version-31-second-edition)

Využitelnost pro NDOP: Formální prahy a logické podmínky jednotlivých kritérií.

## **HROMADNÉ NÁSTROJE A AUTOMATICKÉ HODNOCENÍ**

**\[P01\]** Dauby, G. et al. (2017). ConR: An R package to assist large-scale multispecies preliminary conservation assessments using distribution data. Ecology and Evolution 7: 11292–11303. [Otevřít zdroj ↗](https://doi.org/10.1002/ece3.3704)

Využitelnost pro NDOP: Nejdůležitější dávkový referenční postup pro výpočet EOO/AOO a předběžnou kategorii.

**\[P02\]** Cazalis, V. et al. (2024). Accelerating and standardising IUCN Red List assessments with sRedList. Biological Conservation 298: 110761\. [Otevřít zdroj ↗](https://doi.org/10.1016/j.biocon.2024.110761)

Využitelnost pro NDOP: Standardizace dat a výstupů včetně národních hodnocení, biotopových trendů a fragmentace.

**\[P03\]** Cardoso, P. (2017). red – an R package to facilitate species red list assessments according to the IUCN criteria. Biodiversity Data Journal 5: e20530. [Otevřít zdroj ↗](https://doi.org/10.3897/BDJ.5.e20530)

Využitelnost pro NDOP: Odhady prostorových veličin a SDM; alternativní výpočtový nástroj.

**\[P04\]** Lee, C. K. F. et al. (2019). Redlistr: Tools for the IUCN Red Lists of ecosystems and threatened species in R. Ecography 42: 1050–1055. [Otevřít zdroj ↗](https://doi.org/10.1111/ecog.04143)

Využitelnost pro NDOP: Geometrické parametry, změny rozšíření a populační trendy; důraz na reprodukovatelnost.

**\[P05\]** Bachman, S. et al. (2011). Supporting Red List threat assessments with GeoCAT: geospatial conservation assessment tool. ZooKeys 150: 117–126. [Otevřít zdroj ↗](https://doi.org/10.3897/zookeys.150.2109)

Využitelnost pro NDOP: Snadná GIS kontrola EOO/AOO a vizuální kontrola výskytů.

**\[P06\]** Bachman, S. et al. (2020). Rapid Least Concern: towards automating Red List assessments. Biodiversity Data Journal 8: e47018. [Otevřít zdroj ↗](https://doi.org/10.3897/BDJ.8.e47018)

Využitelnost pro NDOP: Automatická triáž pravděpodobně málo ohrožených druhů pro expertní došetření.

**\[P07\]** Zizka, A., Andermann, T. & Silvestro, D. (2022). IUCNN – Deep learning approaches to approximate species’ extinction risk. Diversity and Distributions 28: 227–241. [Otevřít zdroj ↗](https://doi.org/10.1111/ddi.13450)

Využitelnost pro NDOP: Predikce výsledné kategorie pomocí neuronových sítí; nevysvětluje sama splnění kritérií.

**\[P08\]** Zizka, A. et al. (2021). Automated conservation assessment of the orchid family with deep learning. Conservation Biology 35: 897–908. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.13616)

Využitelnost pro NDOP: Velkoplošná taxonomická aplikace prediktivního přístupu.

**\[P09\]** Walker, B. E. et al. (2023). Evidence-based guidelines for automated conservation assessments of plant species. Conservation Biology 37: e13992. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.13992)

Využitelnost pro NDOP: Empirické srovnání šesti metod, typů čištění dat a jejich chybných klasifikací.

**\[P10\]** Henry, E. G. et al. (2024). Modelling the probability of meeting IUCN Red List criteria to support reassessments. Global Change Biology 30: e17119. [Otevřít zdroj ↗](https://doi.org/10.1111/gcb.17119)

Využitelnost pro NDOP: Predikce pravděpodobnosti splnění konkrétních kritérií namísto pouhého výstupu kategorie.

**\[P11\]** Bachman, S. P. et al. (2024). Extinction risk predictions for the world’s flowering plants to support their conservation. New Phytologist 242: 797–808. [Otevřít zdroj ↗](https://doi.org/10.1111/nph.19592)

Využitelnost pro NDOP: Bayesovské predikce ohrožení s kvantifikací nejistoty, zejména jako priorizační pomůcka.

**\[P12\]** Caetano, G. H. de O. et al. (2022). Automated assessment reveals that the extinction risk of reptiles is widely underestimated across space and phylogeny. PLOS Biology 20: e3001544. [Otevřít zdroj ↗](https://doi.org/10.1371/journal.pbio.3001544)

Využitelnost pro NDOP: Prediktivní identifikace přehlížených rizik v taxonomicky rozsáhlé skupině.

**\[P13\]** Borgelt, J. et al. (2022). More than half of data deficient species predicted to be threatened by extinction. Communications Biology 5: 679\. [Otevřít zdroj ↗](https://doi.org/10.1038/s42003-022-03638-9)

Využitelnost pro NDOP: Modelová priorizace druhů DD; nenahrazuje jejich formální hodnocení.

## **HMYZ, REGIONÁLNÍ HODNOCENÍ A ČASOVÉ TRENDY**

**\[P14\]** Ong, X. R. et al. (2026). Towards red listing understudied tropical insects: A case study of the dung beetles of Sabah, Malaysian Borneo. Insect Conservation and Diversity 19: 511–529. [Otevřít zdroj ↗](https://doi.org/10.1111/icad.70045)

Využitelnost pro NDOP: Přímé testování B u 159 druhů brouků; velký rozdíl mezi automatickým návrhem a expertním uzavřením.

**\[P15\]** Maes, D. et al. (2012). Applying IUCN Red List criteria at a small regional level: A test case with butterflies in Flanders (north Belgium). Biological Conservation 145: 258–266. [Otevřít zdroj ↗](https://doi.org/10.1016/j.biocon.2011.11.021)

Využitelnost pro NDOP: Regionální hodnocení motýlů, vliv prostorového rozlišení a vzájemná srovnatelnost.

**\[P16\]** Maes, D. et al. (2015). The use of opportunistic data for IUCN Red List assessments. Biological Journal of the Linnean Society 115: 690–706. [Otevřít zdroj ↗](https://doi.org/10.1111/bij.12530)

Využitelnost pro NDOP: Hodnocení IUCN z nesystematických faunistických údajů a korekce nedostatků dat.

**\[P17\]** van Strien, A. J., van Swaay, C. A. M. & Termaat, T. (2013). Opportunistic citizen science data of animal species produce reliable estimates of distribution trends if analysed with occupancy models. Journal of Applied Ecology 50: 1450–1458. [Otevřít zdroj ↗](https://doi.org/10.1111/1365-2664.12158)

Využitelnost pro NDOP: Odhady occupancy a detekovatelnosti při nestejné návštěvnosti lokalit.

**\[P18\]** Fox, R. et al. (2019). Insect population trends and the IUCN Red List process. Journal of Insect Conservation 23: 269–278. [Otevřít zdroj ↗](https://doi.org/10.1007/s10841-018-0117-1)

Využitelnost pro NDOP: Citlivost desetiletých populačních trendů motýlů a můr na volbu počátečního roku.

**\[P19\]** Cardoso, P. et al. (2011). Adapting the IUCN Red List criteria for invertebrates. Biological Conservation 144: 2432–2440. [Otevřít zdroj ↗](https://doi.org/10.1016/j.biocon.2011.06.020)

Využitelnost pro NDOP: Metodické limity a specifika IUCN kritérií při použití na bezobratlé.

**\[P20\]** Powney, G. D. et al. (2019). Widespread losses of pollinating insects in Britain. Nature Communications 10: 1018\. [Otevřít zdroj ↗](https://doi.org/10.1038/s41467-019-08974-9)

Využitelnost pro NDOP: Hierarchické modely obsazenosti pro rozsáhlé nehomogenní záznamy hmyzu.

## **VADY DAT, AOO, NEJISTOTY A DALŠÍ METODY**

**\[P21\]** Isaac, N. J. B. et al. (2014). Statistics for citizen science: extracting signals of change from noisy ecological data. Methods in Ecology and Evolution. [Otevřít zdroj ↗](https://doi.org/10.1111/2041-210X.12254)

Využitelnost pro NDOP: Srovnání korekcí mapovacího úsilí; důrazně varuje před jednoduchými indikátory trendu.

**\[P22\]** Marsh, C. J. et al. (2023). The effect of sampling effort and methodology on range size estimates of poorly-recorded species for IUCN Red List assessments. Biodiversity and Conservation 32: 1105–1123. [Otevřít zdroj ↗](https://doi.org/10.1007/s10531-023-02543-9)

Využitelnost pro NDOP: Prokazuje velkou závislost odhadované AOO na počtu známých lokalit a výběru metody.

**\[P23\]** Ribeiro, B. R. et al. (2022). Issues with species occurrence data and their impact on extinction risk assessments. Biological Conservation 273: 109674\. [Otevřít zdroj ↗](https://doi.org/10.1016/j.biocon.2022.109674)

Využitelnost pro NDOP: Dopad časových, prostorových a taxonomických chyb při předběžném posuzování.

**\[P24\]** Zizka, A. et al. (2019). CoordinateCleaner: Standardized cleaning of occurrence records from biological collection databases. Methods in Ecology and Evolution 10: 744–751. [Otevřít zdroj ↗](https://doi.org/10.1111/2041-210X.13152)

Využitelnost pro NDOP: Reprodukovatelná kontrola souřadnic a dalších základních chyb.

**\[P25\]** Anderson, R. P. (2023). Integrating habitat-masked range maps with quantifications of prevalence to estimate area of occupancy in IUCN assessments. Conservation Biology 37: e14019. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.14019)

Využitelnost pro NDOP: Vyvažuje podhodnocení AOO ze známých nálezů a nadhodnocení ze všech potenciálních biotopů.

**\[P26\]** Gallagher, R. et al. (2025). Incorporating citizen science into IUCN Red List assessments. Conservation Biology 39: e14329. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.14329)

Využitelnost pro NDOP: Praktické principy zapojení občanských vědeckých dat do různých kritérií.

**\[P27\]** Falaschi, M. et al. (2025). Differences in estimates of extinction risk between occupancy and abundance data. Conservation Biology 39: e70020. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.70020)

Využitelnost pro NDOP: Dokládá, že trendy abundance a occupancy mohou dávat odlišné výsledky IUCN.

**\[P28\]** Santini, L. et al. (2019). Applying habitat and population-density models to land-cover time series to inform IUCN Red List assessments. Conservation Biology 33: 1084–1093. [Otevřít zdroj ↗](https://doi.org/10.1111/cobi.13279)

Využitelnost pro NDOP: Modelování trendů ze změn krajinného pokryvu, kvality biotopu a hustoty populace.

**\[P29\]** Newton, A. C. (2010). Use of a Bayesian network for Red Listing under uncertainty. Environmental Modelling & Software 25: 15–23. [Otevřít zdroj ↗](https://doi.org/10.1016/j.envsoft.2009.07.016)

Využitelnost pro NDOP: Bayesovský rozhodovací model při neúplné znalosti parametrů.

**\[P30\]** Akçakaya, H. R. et al. (2000). Making Consistent IUCN Classifications under Uncertainty. Conservation Biology 14: 1001–1013. [Otevřít zdroj ↗](https://doi.org/10.1046/j.1523-1739.2000.99125.x)

Využitelnost pro NDOP: Zakládající práce k transparentnímu zacházení s nejistotou hodnot.

## **DATOVÉ A SOFTWAROVÉ ZDROJE**

**\[S1\]** AOPK ČR. Nálezová data – charakteristika NDOP. Informační systém ochrany přírody. [Otevřít zdroj ↗](https://portal.nature.cz/w/nalezova-data)

Využitelnost pro NDOP: Výslovně uvádí různé typy původu dat, soustředění na ohrožené druhy a nestejnou přesnost údajů.

**\[S2\]** AOPK ČR. Nálezová data druhů – metadata datové sady NDOP. [Otevřít zdroj ↗](https://portal.nature.cz/cs/w/nalezova-data-druhu)

Využitelnost pro NDOP: Původ záznamů, georeference a podmínky poskytování dat.

**\[T1\]** Dauby, G. / ConR – zdrojový repozitář a návod k instalaci. [Otevřít zdroj ↗](https://github.com/gdauby/ConR)

Využitelnost pro NDOP: Stav dostupnosti balíčku, dokumentace funkcí a závislosti.

**\[T2\]** CRAN. redlistr, verze 2.1.0 (14. 7\. 2026). [Otevřít zdroj ↗](https://cran.r-project.org/package=redlistr)

Využitelnost pro NDOP: Aktuální verze výpočtového nástroje a jeho závislosti.

**\[T3\]** sRedList – oficiální web platformy. [Otevřít zdroj ↗](https://sredlist.eu/)

Využitelnost pro NDOP: Upload vlastních nálezů, verze platformy, funkce pro národní hodnocení.

**\[T4\]** IUCN Red List – sRedList (assessment tools). [Otevřít zdroj ↗](https://nrl.iucnredlist.org/resources/sredlist)

Využitelnost pro NDOP: Metodické zařazení sRedList do používaných podpůrných nástrojů IUCN.

# **12 | Omezení rešerše a další ověření**

Rešerše byla sestavena jako cílený výběr přímo relevantních recenzovaných prací a oficiálních zdrojů; nejde o systematický přehled podle PRISMA. Bibliografické a metodické údaje byly ověřovány proti stránkám vydavatelů a oficiálním popisům nástrojů k uvedenému datu. Některé balíčky rychle mění dostupnost a funkce; při zahájení implementace je nutné jejich technický stav znovu ověřit.

Nebyla zpřístupněna interní struktura NDOP ani konkrétní export, takže návrh polí, dostupnosti opakovaných návštěv a detailu biotopových map je pracovní hypotéza. Před rozhodnutím o technickém řešení je nezbytný datový audit. Používání citlivých souřadnic v externí webové službě musí projít institucionálním posouzením.