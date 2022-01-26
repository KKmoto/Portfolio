
CREATE TABLE employee(
    EmployeeID INT(25) AUTO_INCREMENT,
    EmployeeName VARCHAR(100) NOT NULL,
    BirthDate DATE,
    POSITION VARCHAR(25),
    ManagerID INT(25) DEFAULT NULL,
    PRIMARY KEY(EmployeeID),
    FOREIGN KEY(ManagerID) REFERENCES employee(EmployeeID)
);

SELECT * FROM (
SELECT name, Year, Global_Sales/SUM(global_sales) over (Partition by Year)*100 as percentofglobalsalesyear
FROM PortfolioProject.dbo.vgsales
) as py
WHERE name LIKE '%Mario%'
Order by percentofglobalsalesyear desc