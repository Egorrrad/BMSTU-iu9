DROP DATABASE IF EXISTS lab151
GO

CREATE DATABASE lab151
GO

DROP DATABASE IF EXISTS lab152
GO

CREATE DATABASE lab152
GO

use lab151
GO

DROP TABLE if EXISTS Customers
Go 

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

use lab152
GO

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
    DeliveryCost INT NOT NULL
);
GO

-- добавляем данные
INSERT INTO lab151.dbo.Customers (Email, FirstName, LastName, Phone) 
VALUES 
    ('wqwq@rjk.com', 'aaaaa', 'bbbbb', '798484849'),
    ('wqrqwtyqf@rjk.com', 'aaaaa', 'bbbbb', '798484834901')
go 


select * from lab151.dbo.Customers
go


-- Триггер для вставки в Orders
DROP TRIGGER IF EXISTS trg_Insert_Orders;
GO

CREATE TRIGGER trg_Insert_Orders
ON Orders
FOR INSERT
AS
BEGIN
    IF EXISTS (SELECT CustomerID FROM inserted EXCEPT SELECT CustomerID FROM lab151.dbo.Customers)
    BEGIN
			RAISERROR('Customer not exist!', 16, 1)
			ROLLBACK
	END

END;
GO

-- тестирование Insert (работает)
INSERT into Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, PaymentStatus, DeliveryCost) 
VALUES ('1234567890123456739', 1, 'wqwqwwqqwwq', CONVERT(date,N'11-28-2024'), 5, 0, 0, 100),
        ('123456789012347839', 2, 'wqwqwwqqwwq', CONVERT(date,N'11-30-2024'), 5, 0, 1, 100)
GO

-- должно выдавать ошибку
INSERT into Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, PaymentStatus, DeliveryCost) 
VALUES ('1234567890123456730', 1, 'wqwqwwqqwwq', CONVERT(date,N'11-28-2024'), 5, 0, 0, 100),
        ('123456789012347830', 5, 'wqwqwwqqwwq', CONVERT(date,N'11-30-2024'), 5, 0, 1, 100)
GO

-- Триггер для обновления Orders
DROP TRIGGER IF EXISTS trg_Update_Orders;
GO

CREATE TRIGGER trg_Update_Orders
ON Orders
FOR UPDATE
AS
BEGIN
    IF UPDATE(CustomerID)
    BEGIN
        RAISERROR('Updating CustomerID in Orders table is not allowed!', 16, 1);
        ROLLBACK
    END;
END;
GO

-- тестирование обновления покупателя
UPDATE Orders SET CustomerID = 1 WHERE OrderID = 3
GO
UPDATE Orders SET OrderID = 1 WHERE OrderID = 3
GO

-- тестирование обновления скидки
UPDATE Orders SET Discount = 20 WHERE OrderID = 3
GO

-- Триггер для удаления Orders
DROP TRIGGER IF EXISTS trg_Delete_Orders
GO


CREATE TRIGGER trg_Delete_Orders
    ON Orders
    FOR  DELETE
    AS 
    IF (SELECT COUNT(*) FROM deleted WHERE PaymentStatus = 1)>0
    BEGIN
        RAISERROR('You cant delete a paid order!', 16, 1)
        ROLLBACK
    END
GO 


-- тестирование удаления оплаченного заказа
DELETE from Orders WHERE PaymentStatus = 1
GO

select * from lab152.dbo.Orders
go

use lab151
GO

-- Триггер для удаления Customers
DROP TRIGGER IF EXISTS trg_Delete_Customers
GO


CREATE TRIGGER trg_Delete_Customers
    ON Customers
    INSTEAD OF  DELETE
    AS
    BEGIN 
        RAISERROR('You cant delete Customer!', 16, 1)
        ROLLBACK
    END
GO 

-- тестирование удаления покупателя
DELETE from Customers WHERE CustomerID = 1
GO

use lab151
go

DROP VIEW IF EXISTS Orders
GO
CREATE VIEW Orders AS
    SELECT * FROM lab152.dbo.Orders
GO

use lab152
go

DROP VIEW IF EXISTS Customers
GO
CREATE VIEW Customers AS
    SELECT * FROM lab151.dbo.Customers
GO


select * from lab151.dbo.Customers
go

select * from lab152.dbo.Orders
go