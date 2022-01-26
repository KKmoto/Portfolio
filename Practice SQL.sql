
-- Table creation with a recursive relationship and constratins in password length check

CREATE TABLE employee (
    EmployeeID INT(25) AUTO_INCREMENT,
    EmployeeName VARCHAR(100) NOT NULL,
    BirthDate DATE,
    POSITION VARCHAR(25),
    ManagerID INT(25) DEFAULT NULL,
    PRIMARY KEY(EmployeeID),
    FOREIGN KEY(ManagerID) REFERENCES employee(EmployeeID)

CREATE TABLE customer (
	CustomerID int(25) NOT NULL UNIQUE,
	CustomerName varchar(100) NOT NULL,
	Address varchar(100) NOT NULL,
	City varchar(100) NOT NULL,
	Country varchar(50) NOT NULL,
	Password varchar(50) NOT NULL,
	PRIMARY KEY (CustomerID),
	CONSTRAINT password_8 CHECK (Password>=8)
);


-- Queries involving recursive relationships, joins, comparing customers against other customers

SELECT man.EmployeeName AS "Manager Name"
	,COUNT(WORK.employeeID) AS NOfWorkers
FROM Employee AS man
	,employee AS WORK
WHERE man.employeeID = WORK.managerid
GROUP BY man.employeeID


SELECT SubscriptionType
	,ProductName
	,Price
FROM Subscription s
LEFT JOIN Product p ON s.ProductID = p.ProductID
GROUP BY price DESCSELECT a.customer_ID AS CustomerX
	,b.customer_ID AS CustomerY
	,count(a.film_id) AS nOfOverlappingMovies
FROM (
	SELECT customer.customer_id
		,film.film_id
	FROM customer
		,rental
		,inventory
		,film
	WHERE customer.customer_id = rental.customer_id
		AND inventory.inventory_id = rental.inventory_id
		AND film.film_id = inventory.film_id
	) AS a
JOIN (
	SELECT customer.customer_id
		,film.film_id
	FROM customer
		,rental
		,inventory
		,film
	WHERE customer.customer_id = rental.customer_id
		AND inventory.inventory_id = rental.inventory_id
		AND film.film_id = inventory.film_id
	) AS b ON a.film_id = b.film_id
WHERE a.customer_id > b.customer_id
GROUP BY CustomerX
	,CustomerY
ORDER BY nOfOverlappingMovies DESC



-------- Video game sales data queries 

--Best selling games on the Wii: 

SELECT name
	,Platform
	,CAST(Global_Sales AS INT) AS totalsales
FROM PortfolioProject.dbo.vgsales v
WHERE Platform = 'Wii'
ORDER BY totalsales desc;


--Best selling games in North America, which aren't nintendo games:

SELECT Name
	,Publisher
	,CAST(NA_Sales AS DECIMAL(5, 2)) AS NAsales
FROM PortfolioProject.dbo.vgsales v
WHERE Publisher <> 'Nintendo'
ORDER BY NAsales DESC

--Best selling mario games by percentage of global sales in their respective year:

SELECT *
FROM (
	SELECT name
		,Year
		,Global_Sales / SUM(global_sales) OVER (PARTITION BY Year) * 100 AS percentofglobalsalesyear
	FROM PortfolioProject.dbo.vgsales
	) AS py
WHERE name LIKE '%Mario%'
ORDER BY percentofglobalsalesyear DESC
