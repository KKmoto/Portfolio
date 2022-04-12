--SELECT * FROM dbo.covid_jpn_prefecture$

---- Total Positive Cases vs Total Fatalities

-- Death percentage by Prefecture

SELECT Prefecture, Date, Tested, Positive, Fatal, 
	CASE
		WHEN Fatal = 0
		THEN 0
		ELSE (Fatal/Positive)*100 
	END as FatalityPercentage
FROM dbo.covid_jpn_prefecture$
ORDER BY 1, 2


---- Total Cases vs Population

-- *Population is in Thousands

SELECT prefec.Prefecture, prefec.Date, 
	Positive, Value*1000 as PrefecturePopulation, Fatal, 	CASE
		WHEN Fatal = 0
		THEN 0
		ELSE (Fatal/Positive)*100 
	END as FatalityPercentage, 
	CASE
		WHEN Positive = 0
		THEN 0
		ELSE (Positive/(Value*1000))*100 
	END as PositivePercentage
FROM dbo.covid_jpn_prefecture$ prefec
	inner join dbo.covid_jpn_metadata metadata
	on prefec.Prefecture = metadata.Prefecture
WHERE Item = 'Total' and Category = 'Population'
ORDER BY 1,2


---- Look at Prefectures with Highest Infection rate in comparison to Population

SELECT prefec.Prefecture, MAX(Positive) as HighestInfectionCount, 
	Value*1000 as PrefecturePopulation,  MAX((Positive/(Value*1000)))*100 as PositivePercentage
FROM dbo.covid_jpn_prefecture$ prefec
	inner join dbo.covid_jpn_metadata metadata
	on prefec.Prefecture = metadata.Prefecture
WHERE Item = 'Total' and Category = 'Population'
GROUP BY prefec.Prefecture, Value
ORDER BY PositivePercentage DESC


---- Look at Prefectures with Highest Death rate in comparison to Population

SELECT dbo.covid_jpn_prefecture$.Prefecture, MAX(Fatal) as HighestDeathCount, 
	Value*1000 as PrefecturePopulation,  MAX((Fatal/(Value*1000)))*100 as DeathPercentage
FROM dbo.covid_jpn_prefecture$
	inner join dbo.covid_jpn_metadata 
	on dbo.covid_jpn_prefecture$.Prefecture = dbo.covid_jpn_metadata.Prefecture
WHERE Item = 'Total' and Category = 'Population'
GROUP BY dbo.covid_jpn_prefecture$.Prefecture, Value
ORDER BY DeathPercentage DESC


---- Combine Covid_jpn_total data to find nationwide totals over time

-- Create TEMP TABLE for subsequent queries and future visualiation

DROP TABLE IF exists Japan_Totals
CREATE TABLE Japan_Totals
(
Date datetime,
Positive numeric,
Tested numeric,
Symptomatic numeric,
Asymptomatic numeric,
Symptoms_unknown numeric,
Hosp_require numeric,
Hosp_mild numeric,
Hosp_severe numeric,
Hosp_unknown numeric,
Hosp_waiting numeric,
Discharged numeric,
Fatal numeric,
Vaccinated_1st numeric,
Vaccinated_2nd numeric,
Vaccinated_3rd numeric
)

INSERT INTO Japan_Totals
SELECT Date, 
	SUM(Positive) as tot_Positive, SUM(Tested) as tot_Tested, SUM(Symptomatic) as tot_Symptomatic, SUM(Asymptomatic) as tot_Asymptomatic, 
	SUM(Sym_unknown) as tot_Sym_unknown, SUM(Hosp_require) as tot_Hosp_require, SUM(Hosp_mild) as tot_Hosp_mild, SUM(Hosp_severe) as tot_Hosp_severe, 
	SUM(Hosp_unknown) as tot_Hosp_unkown, SUM(Hosp_waiting) as tot_Hosp_waiting, SUM(Discharged) as tot_Discharged, SUM(Fatal) as tot_Fatal,
	SUM(Vaccinated_1st) as tot_Vaccinated_1st, SUM(Vaccinated_2nd) as tot_Vaccinated_2nd, SUM(Vaccinated_3rd) as tot_Vaccinated_3rd
FROM dbo.covid_jpn_total
GROUP BY Date

SELECT * FROM dbo.Japan_Totals


-- Correct error in data entry

UPDATE dbo.Japan_Totals
SET Positive = 6670208
WHERE Date = '2022-04-04'


SELECT * FROM Japan_Totals
order by Date desc


---- Total Japanese Population vs Vaccinated Individuals (At least one dose)

--Population Calculation with VIEW
DROP VIEW [Japan_Population]

CREATE VIEW Japan_Population 
AS
	SELECT SUM(Value*1000) as Population
	FROM dbo.covid_jpn_metadata
	WHERE Item = 'Total' and Category = 'Population'
	GROUP BY Date

SELECT * FROM Japan_Population


---- Find Running Total of First Vaccinations over time

-- CREATE VIEW for First Vaccination Calculation and later Visualizations
DROP VIEW [Japan_Population]
CREATE VIEW OneVaccinatedRunningTotal
AS
	SELECT Date, Vaccinated_1st, SUM(Vaccinated_1st) OVER (ORDER BY Date) as RunningVaccinatedTotal
	FROM Japan_Totals
	

--   Total First Vaccinations Administered

CREATE VIEW TotalFirstVaccinationsAdministered
AS
SELECT JPtotal.Date, JPtotal.Vaccinated_1st as NumberDailyFirstVaccination , RunningVaccinatedTotal, 
	CASE 
		WHEN RunningVaccinatedTotal = 0
		THEN 0
		ELSE ((CAST(RunningVaccinatedTotal AS DECIMAL(20,10))/(CAST(126708000 AS DECIMAL(20,10))))*100)
		END AS TotalPopulationVaccinatedPercentage
FROM Japan_Totals JPtotal
		inner JOIN 
	OneVaccinatedRunningTotal Vac
		on JPtotal.Date = Vac.Date 
--ORDER BY TotalPopulationVaccinatedPercentage DESC

