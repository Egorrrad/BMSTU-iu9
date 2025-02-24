USE lab6
Go

-- создание таблиц


DROP TABLE if EXISTS Sellers;
go

-- Таблица Seller
CREATE TABLE Sellers (
    SellerID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    Address NVARCHAR(100) NULL
);
go 

DROP TABLE if EXISTS Products;
go
-- Таблица Products
CREATE TABLE Products (
    ProductID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    SellerID INT NOT NULL,
    CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Sellers(SellerID),
    SKU VARCHAR(20) NOT NULL UNIQUE,
    ProductName NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    OriginCountry VARCHAR(30) NULL,
    Price NUMERIC(8,2) NOT NULL,
    StockQuantity INT NOT NULL
);
go 


DROP TABLE if EXISTS Orders
Go 

-- Таблица Orders
CREATE TABLE Orders (
    OrderID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    OrderNumber VARCHAR(20) UNIQUE NOT NULL,
    CustomerID INT NOT NULL,
    Address NVARCHAR(200) NOT NULL,
    CreateDate DATETIME DEFAULT(getdate()) NOT NULL,
    DeliveryDate DATE NOT NULL,
    Discount TINYINT NULL,
    PaymentType TINYINT NOT NULL,
    PaymentStatus BIT NOT NULL,
    DeliveryCost INT NOT NULL,
    CONSTRAINT FK_Customer_Order FOREIGN KEY (CustomerID) REFERENCES Customers(CustomerID)
);
GO

-- добавляем данные
INSERT INTO Sellers
    (OGRN, Name, Description, Address)
VALUES
    ('1234567890123453', 'Seller 5', 'First Seller', 'Address 1'),
    ('9876543210987632', 'Seller 6', 'Second1 Seller', 'Address 2'),
    ('9876543210987611', 'Seller 7', 'Second2 Seller', 'Address 3'),
    ('9876543210987625', 'Seller 8', 'Second3 Seller', 'Address 4');
GO


SELECT * from Sellers
GO

INSERT INTO Products
    (SellerID, SKU, ProductName, Description, OriginCountry, Price, StockQuantity)
VALUES
    ((SELECT SellerID
        FROM Sellers
        WHERE OGRN = '9876543210987632'), 'SKU123548302', 'Product 12', 'First product', 'Country 1', 140.00, 3),
    ((SELECT SellerID
        FROM Sellers
        WHERE OGRN = '9876543210987611'), 'SKU456529304', 'Product 56', 'Second product', 'Country 2', 210.00, 6);

SELECT * from Products
GO


SELECT * from Customers
GO


INSERT into Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, PaymentStatus, DeliveryCost) 
VALUES ('1234567890123456721', 3, 'wqwqwwqqwwq', CONVERT(date,N'11-28-2024'), 5, 0, 0, 100),
        ('123456789012347821', 4, 'wqwqwwqqwwq', CONVERT(date,N'11-30-2024'), 5, 0, 1, 100)
GO


SELECT * from Orders
go

/*
1. Для одной из таблиц пункта 2 задания 7 создать триггеры на вставку, удаление и добавление, при выполнении заданных условий один из триггеров должен инициировать возникновение ошибки
(RAISERROR / THROW).
*/
DROP TRIGGER IF EXISTS OrderTr1
GO
-- INSERT TRIGGER
CREATE TRIGGER OrderTr1
    ON Orders
    INSTEAD OF INSERT
    AS
    BEGIN
        IF ((SELECT COUNT(*) FROM inserted WHERE DeliveryDate < CAST(GETDATE() AS DATE)) > 0)
            RAISERROR ('The delivery date cannot be earlier than todays date.', 1, 1);
        ELSE
            -- если дата доставки корректная, вставляем данные
            INSERT INTO Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, DeliveryCost)
            SELECT OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, DeliveryCost
            FROM inserted;
    END;
GO

-- тестирование добавления заказа с некоректной датой
INSERT into Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, DeliveryCost) 
VALUES ('12345678901234567894', 3, 'wqwqwwqqwwq', CONVERT(date,N'11-11-2024'), 5, 0, 100)
GO

DROP TRIGGER IF EXISTS OrderTr2
GO
-- UPDATE TRIGGER
CREATE TRIGGER OrderTr2
ON Orders
INSTEAD OF UPDATE
AS
BEGIN
    IF UPDATE(CustomerID)
    BEGIN
        RAISERROR('Updating CustomerID in Orders table is not allowed!', 16, 1);
        RETURN;
    END;

    -- Обновление данных с использованием MERGE
    MERGE INTO Orders AS target
    USING (SELECT OrderID, OrderNumber, Address, CreateDate, DeliveryDate, Discount, PaymentType, DeliveryCost FROM inserted) AS source
    ON (target.OrderID = source.OrderID)
    WHEN MATCHED THEN
        UPDATE SET
            target.OrderNumber = source.OrderNumber,
            target.Address = source.Address,
            target.CreateDate = source.CreateDate,
            target.DeliveryDate = source.DeliveryDate,
            target.Discount = source.Discount,
            target.PaymentType = source.PaymentType,
            target.DeliveryCost = source.DeliveryCost;
END;

GO


-- тестирование обновления покупателя
UPDATE Orders SET CustomerID = 3 WHERE OrderID = 1
GO

-- тестирование обновления скидки
UPDATE Orders SET Discount = 20 WHERE OrderID = 1
GO

DROP TRIGGER IF EXISTS OrderTr3
GO
-- DELETE TRIGGER
CREATE TRIGGER OrderTr3
    ON Orders
    INSTEAD OF  DELETE
    AS 
    IF (SELECT COUNT(*) FROM Orders WHERE PaymentStatus = 1)>0
        RAISERROR('You cant delete a paid order!', 1, 1)
    ELSE
        DELETE FROM Orders WHERE OrderID in (SELECT OrderID FROM deleted)
GO 

-- тестирование удаления оплаченного заказа
DELETE from Orders WHERE PaymentStatus = 1
GO

/*
Для представления пункта 2 задания 7 создать триггеры на вставку, удаление и добавление, обеспечивающие возможность выполнения операций с данными непосредственно через представление.
*/

-- вариант для связи 1 к 1
DROP TABLE IF EXISTS SellersDetails
GO
DROP TABLE IF EXISTS SellersBase
GO
-- Создание основной таблицы SellersBase
CREATE TABLE SellersBase (
    SellerID INT IDENTITY(1,1) PRIMARY KEY NOT NULL, 
    OGRN VARCHAR(20) NOT NULL UNIQUE,               
    Name NVARCHAR(50) NOT NULL
);
GO

-- Создание таблицы с расширенными данными SellersDetails
CREATE TABLE SellersDetails (
    SellerID INT PRIMARY KEY NOT NULL,              -- Связь с SellersBase
    Description NVARCHAR(MAX) NULL,                 
    Address NVARCHAR(100) NULL,                     
    FOREIGN KEY (SellerID) REFERENCES SellersBase(SellerID) 
);
GO


DROP VIEW IF EXISTS SellersView
GO
-- Создание представления SellersView
CREATE VIEW SellersView AS
SELECT 
    sb.SellerID,
    sb.OGRN,
    sb.Name,
    sd.Description,
    sd.Address
FROM 
    SellersBase sb
INNER JOIN 
    SellersDetails sd ON sb.SellerID = sd.SellerID;

GO

-- Триггер для вставки в представление SellersView
DROP TRIGGER IF EXISTS trg_Insert_Sellers;
GO

CREATE TRIGGER trg_Insert_Sellers
ON SellersView
INSTEAD OF INSERT
AS
BEGIN
    IF EXISTS(SELECT 1  FROM inserted INNER JOIN SellersBase on SellersBase.OGRN = inserted.OGRN)
        BEGIN
            RAISERROR('SellersBase is already exists!', 16, 1);
            RETURN;
         END;

    -- Вставка в таблицу SellersBase
    WITH BaseInsert AS (
        SELECT OGRN, Name
        FROM inserted
    )
    MERGE INTO SellersBase AS target
    USING BaseInsert AS source
    ON target.OGRN = source.OGRN
    WHEN NOT MATCHED THEN
        INSERT (OGRN, Name) VALUES (source.OGRN, source.Name);

    -- Вставка в таблицу SellersDetails
    WITH DetailsInsert AS (
        SELECT 
            sb.SellerID,
            i.Description,
            i.Address
        FROM inserted i
        INNER JOIN SellersBase sb ON i.OGRN = sb.OGRN
    )
    MERGE INTO SellersDetails AS target
    USING DetailsInsert AS source
    ON target.SellerID = source.SellerID
    WHEN NOT MATCHED THEN
        INSERT (SellerID, Description, Address)
        VALUES (source.SellerID, source.Description, source.Address);
END;
GO

-- Триггер для обновления в представлении SellersView
DROP TRIGGER IF EXISTS trg_Update_Sellers;
GO

CREATE TRIGGER trg_Update_Sellers
ON SellersView
INSTEAD OF UPDATE
AS
BEGIN
    IF UPDATE(SellerID)
    BEGIN
        RAISERROR('Updating SellerID is not allowed!', 16, 1);
        RETURN;
    END;

    -- Обновление таблицы SellersBase
    WITH BaseUpdate AS (
        SELECT SellerID, OGRN, Name
        FROM inserted
    )
    MERGE INTO SellersBase AS target
    USING BaseUpdate AS source
    ON target.SellerID = source.SellerID
    WHEN MATCHED THEN
        UPDATE SET 
            target.OGRN = source.OGRN,
            target.Name = source.Name;

    -- Обновление таблицы SellersDetails
    WITH DetailsUpdate AS (
        SELECT SellerID, Description, Address
        FROM inserted
    )
    MERGE INTO SellersDetails AS target
    USING DetailsUpdate AS source
    ON target.SellerID = source.SellerID
    WHEN MATCHED THEN
        UPDATE SET 
            target.Description = source.Description,
            target.Address = source.Address;
END;
GO

-- Триггер для удаления в представлении SellersView
DROP TRIGGER IF EXISTS trg_Delete_Sellers;
GO

CREATE TRIGGER trg_Delete_Sellers
ON SellersView
INSTEAD OF DELETE
AS
BEGIN
    -- Удаление из таблицы SellersDetails
    WITH ToDelete AS (
        SELECT SellerID
        FROM deleted
    )
    DELETE FROM SellersDetails
    WHERE SellerID IN (SELECT SellerID FROM ToDelete);

    -- Удаление из таблицы SellersBase
    DELETE FROM SellersBase
    WHERE SellerID IN (SELECT SellerID FROM deleted);
END;
GO

SELECT * from SellersView

-- пример обновления SellerID
update SellersView
set SellerID = SellerID+1
SELECT * from SellersView

-- Пример вставки данных
INSERT INTO SellersView (OGRN, Name, Description, Address)
VALUES ('12345678901234567800', 'Example Seller88', 'A great seller!', '456 New Address'),
        ('12345678901234567899', 'Example Seller88', 'A great seller!', '456 New Address')

-- Пример обновления данных
UPDATE SellersView
SET Name = 'Updated Seller'
WHERE SellerID = 2;


-- Пример удаления данных
DELETE FROM SellersView WHERE SellerID = 100