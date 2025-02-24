use master
go 

IF DB_ID (N'lab6') IS NOT NULL
DROP DATABASE lab5;
GO

-- создание базы данных
CREATE DATABASE lab6
ON ( 
    NAME = lab6, 
    FILENAME = '/home/data/lab6.mdf',
    SIZE = 10, 
    MAXSIZE = 15, 
    FILEGROWTH = 5
    )
LOG 
ON ( 
    NAME = lab6_log, 
    FILENAME = '/home/log/lab6log.log',
    SIZE = 5, 
    MAXSIZE = 20, 
    FILEGROWTH = 5
    );
GO

use lab6
GO


-- проверка наличия таблицы Customers
if OBJECT_ID(N'Customers') is NOT NULL
	DROP Table Customers;
go

-- создание таблицы Customers
CREATE TABLE Customers
(
    CustomerID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    Email VARCHAR(254) NOT NULL UNIQUE,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    Phone VARCHAR(15) NOT NULL UNIQUE,
    BirthDate DATE NULL
);
go

-- создание функции для проверки email
CREATE FUNCTION as_isValidEmail(@EMAIL nvarchar(254))
RETURNS bit as
BEGIN     
  DECLARE @bitRetVal as Bit
  IF (@EMAIL <> '' AND @EMAIL NOT LIKE '_%@__%.__%')
     SET @bitRetVal = 0  -- Invalid
  ELSE 
    SET @bitRetVal = 1   -- Valid
  RETURN @bitRetVal
END
go 


-- добавление функции проверки email
ALTER TABLE Customers ADD CONSTRAINT chkRowEmail CHECK (dbo.as_isValidEmail(Email) = 1);
GO

-- неправильный email
INSERT INTO Customers (Email, FirstName, LastName, Phone) 
VALUES ('wqwq@.com', 'aaaaa', 'bbbbb', '798484848')
go 

-- правильный email
INSERT INTO Customers (Email, FirstName, LastName, Phone) 
VALUES ('wqwq@rjk.com', 'aaaaa', 'bbbbb', '798484849')
go 

-- добавим дату создания с дефолтной датой с помощью встроенной функции 

ALTER TABLE Customers
ADD createDate DATETIME DEFAULT(getdate())
go


-- добавим строку данных
INSERT INTO Customers (Email, FirstName, LastName, Phone) 
VALUES ('wqrqwtyqf@rjk.com', 'aaaaa', 'bbbbb', '798484834901')
go

-- возвращаем только что созданный IDENTITY
SELECT @@IDENTITY as 'Identity'
go

SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];  
GO 

SELECT IDENT_CURRENT ('Customers') AS Current_Identity;  
GO  


-- смотрим таблицу Customers
SELECT * FROM Customers
GO

-- cоздание таблицы с первичным ключом на основе
-- глобального уникального идентификатора


if OBJECT_ID(N'Sellers') is NOT NULL
	DROP Table Sellers;
go

-- Таблица Seller
CREATE TABLE Sellers (
    SellerID UNIQUEIDENTIFIER DEFAULT NEWID() PRIMARY KEY,
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    Address NVARCHAR(100) NULL
);
go 



-- Здесь создается последовательность seq 

CREATE SEQUENCE seq
    START WITH 1
    INCREMENT BY 1;
GO 



-- создаем таблицу с seq для генерации уникальных значений для столбца ProductID
CREATE TABLE Products (
    ProductID INT PRIMARY KEY DEFAULT NEXT VALUE FOR seq,
    SellerID UNIQUEIDENTIFIER NOT NULL,
    CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Sellers(SellerID),
    SKU VARCHAR(20) NOT NULL UNIQUE,
    ProductName NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    OriginCountry VARCHAR(30) NULL,
    Price NUMERIC(8,2) NOT NULL,
    StockQuantity INT NOT NULL
);
go 

-- добавляем данные

-- Вставляем данные в Sellers
INSERT INTO Sellers (OGRN, Name, Description, Address)
VALUES ('1234567890123453', 'Seller 1', 'First Seller', 'Address 1'),
       ('987654321098763', 'Seller 2', 'Second Seller', 'Address 2'),
       ('987654321098761', 'Seller 3', 'Second Seller', 'Address 3'),
       ('987654321098762', 'Seller 3', 'Second Seller', 'Address 3');

GO

-- Вставляем данные в Products
INSERT INTO Products (SellerID, SKU, ProductName, Description, OriginCountry, Price, StockQuantity)
VALUES ((SELECT SellerID FROM Sellers WHERE Name = 'Seller 1'), 'SKU1235', 'Product 1', 'First product', 'Country 1', 100.00, 10),
       ((SELECT SellerID FROM Sellers WHERE Name = 'Seller 2'), 'SKU4565', 'Product 2', 'Second product', 'Country 2', 200.00, 20);
GO

-- смотрим таблицу Sellers
SELECT * FROM Sellers;
GO


-- смотрим таблицу Products
SELECT * FROM Products;
GO

-- тестирование no action
DELETE FROM Sellers WHERE Name = 'Seller 1';
GO

-- изменение на cascade
if OBJECT_ID(N'FK_SellerID') is NOT NULL
    ALTER TABLE Products
    DROP CONSTRAINT FK_SellerID;

ALTER TABLE Products
ADD CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Sellers(SellerID) ON DELETE CASCADE ON UPDATE CASCADE;
GO


-- тестирование cascade
DELETE FROM Sellers WHERE Name = 'Seller 2';
go


-- Обновление SellerID на NULL для всех строк, где он не существует в Sellers
UPDATE Products
SET SellerID = NULL
WHERE SellerID NOT IN (SELECT SellerID FROM Sellers);

-- изменение на set null
if OBJECT_ID(N'FK_SellerID') is NOT NULL
    ALTER TABLE Products
    DROP CONSTRAINT FK_SellerID;

ALTER TABLE Products
ALTER COLUMN SellerID UNIQUEIDENTIFIER NULL;

ALTER TABLE Products
ADD CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Sellers(SellerID) ON DELETE SET NULL ON UPDATE SET NULL;
GO

-- тестирование set null
DELETE FROM Sellers WHERE Name = 'Seller 2';
GO

-- изменение на set default
if OBJECT_ID(N'FK_SellerID') is NOT NULL
    ALTER TABLE Products
    DROP CONSTRAINT FK_SellerID;

ALTER TABLE Products
ADD CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Sellers(SellerID) ON DELETE SET DEFAULT ON UPDATE SET DEFAULT;

ALTER TABLE Products
ADD CONSTRAINT DF_SellerID DEFAULT NEWID() FOR SellerID;
GO

-- тестирование set default
DELETE FROM Sellers WHERE Name = 'Seller 2';