DROP DATABASE IF EXISTS lab131
go

CREATE DATABASE lab131
GO

DROP DATABASE IF EXISTS lab132
go

CREATE DATABASE lab132
GO

USE lab131
go

DROP TABLE if EXISTS Sellers;
go

-- Таблица Seller
CREATE TABLE Sellers (
    SellerID INT PRIMARY KEY NOT NULL CHECK (SellerID <= 4),
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NOT NULL,
    Address NVARCHAR(100) NOT NULL
);
go 

USE lab132
go

DROP TABLE if EXISTS Sellers;
go

-- Таблица Seller
CREATE TABLE Sellers (
    SellerID INT PRIMARY KEY NOT NULL CHECK (SellerID > 4),
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NOT NULL,
    Address NVARCHAR(100) NOT NULL
);
go 

use lab131
GO

DROP VIEW IF EXISTS SellersView
GO
CREATE VIEW SellersView AS
SELECT * FROM lab131.dbo.Sellers UNION ALL SELECT * FROM lab132.dbo.Sellers
GO

use lab132
GO

DROP VIEW IF EXISTS SellersView
GO
CREATE VIEW SellersView AS
SELECT * FROM lab131.dbo.Sellers UNION ALL SELECT * FROM lab132.dbo.Sellers
GO

use lab131
-- проверка работы Insert
insert into SellersView VALUES
    (1,'123456789012', 'Seller A', 'Description for Seller A', 'Address A'),
    (2,'234567890123', 'Seller B', 'Description for Seller B', 'Address B'),
    (3,'345678901234', 'Seller C', 'Description for Seller C', 'Address C'),
    (4,'456789012345', 'Seller D', 'Description for Seller D', 'Address D'),
    (5,'567890123456', 'Seller E', 'Description for Seller E', 'Address E'),
    (6,'567890123451', 'Seller F', 'Description for Seller F', 'Address F'),
    (7,'567890123459', 'Seller G', 'Description for Seller G', 'Address G')
go

-- проверка работы Select
select * from SellersView
GO

use lab131
select * from Sellers
GO

use lab132
select * from Sellers
GO

-- проверка работы Update
UPDATE SellersView SET Address = 'Updated A adress' WHERE Name = 'Seller A'
GO

-- тут переместится в вторую базу
UPDATE SellersView SET SellerID = 8 WHERE SellerID = 1
GO




-- проверка работы Delete
DELETE FROM SellersView WHERE Address = 'Address E'
GO