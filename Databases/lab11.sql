-- создаем базу данных

DROP DATABASE IF EXISTS lab11
GO

CREATE DATABASE lab11
on (
	NAME = lab11,
	FILENAME = '/home/lab11.mdf',
	SIZE = 11,
	MAXSIZE = UNLIMITED,
	FILEGROWTH = 5
)
log on (
	NAME = lab11log,
	FILENAME = '/home/lab11log.ldf',
	SIZE = 5,
	MAXSIZE = 20,
	FILEGROWTH = 5
);
GO 

use lab11;
GO

DROP TABLE IF EXISTS OrderItem
DROP TABLE IF EXISTS Orders
DROP TABLE IF EXISTS Customer
DROP TABLE IF EXISTS Product
DROP TABLE IF EXISTS Seller
Go

-- создание таблиц 
-- Таблица Customer
CREATE TABLE Customer
(
    CustomerID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    Email VARCHAR(254) NOT NULL UNIQUE,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    Phone VARCHAR(12) NOT NULL UNIQUE,
    BirthDate DATE NULL
);
go


-- Таблица Seller
CREATE TABLE Seller (
    SellerID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(100) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    Address NVARCHAR(200) NOT NULL
);
go


-- Таблица  Product
CREATE TABLE Product (
    ProductID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    SellerID INT NOT NULL,
    SKU VARCHAR(20) NOT NULL UNIQUE,
    ProductName NVARCHAR(100) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    OriginCountry VARCHAR(3) NOT NULL,
    Price NUMERIC(8,2) NOT NULL,
    StockQuantity INT NOT NULL,
    CONSTRAINT FK_SellerID FOREIGN KEY (SellerID) REFERENCES Seller(SellerID),

);
go

-- Таблица Orders
CREATE TABLE Orders (
    OrderID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    OrderNumber VARCHAR(20) UNIQUE NOT NULL,
    CustomerID INT NOT NULL,
    Address NVARCHAR(200) NOT NULL,
    CreateDate DATETIME DEFAULT(getdate()) NOT NULL,
    DeliveryDate DATE NOT NULL,
    PaymentType TINYINT NOT NULL,
    PaymentStatus BIT NOT NULL,
    DeliveryCost INT NOT NULL,
    Discount TINYINT NULL,
    CONSTRAINT FK_CustomerID FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID)
);
GO

-- Таблица OrderItem
CREATE TABLE OrderItem (
    OrderItemID INT IDENTITY(1,1) PRIMARY KEY NOT NULL,
    OrderID INT NOT NULL,
    ProductID INT NOT NULL,
    Quantity INT NOT NULL,
    ActualPrice NUMERIC(8,2) NOT NULL,
    CONSTRAINT FK_OrderID FOREIGN KEY (OrderID) REFERENCES Orders(OrderID)
    ON DELETE CASCADE,
    CONSTRAINT FK_ProductID FOREIGN KEY (ProductID) REFERENCES Product(ProductID)
);
GO



-- создание функции для проверки коректности email
DROP FUNCTION IF EXISTS as_isValidEmail
Go 
CREATE FUNCTION as_isValidEmail(@EMAIL nvarchar(254))
RETURNS bit as
BEGIN     
  IF (@EMAIL <> '' AND @EMAIL NOT LIKE '_%@__%.__%')
     RETURN 0  -- Invalid
  RETURN 1
END
go

-- функция для проверки коректности телефонв
DROP FUNCTION IF EXISTS IsValidRussianPhoneNumber
Go 
CREATE FUNCTION IsValidRussianPhoneNumber (@PhoneNumber VARCHAR(12))
RETURNS BIT
AS
BEGIN
    IF LEFT(@PhoneNumber, 2) <> '+7' or ISNUMERIC(SUBSTRING(@PhoneNumber, 3, 10)) = 0 or LEN(@PhoneNumber) <> 12
        RETURN 0;
    RETURN 1;
END;
GO


-- добавлениие функции проверки email для Customer
ALTER TABLE Customer ADD CONSTRAINT checkEmail CHECK (dbo.as_isValidEmail(Email) = 1);
GO

-- добавление проверки phone
ALTER TABLE Customer ADD CONSTRAINT checkPhone CHECK (dbo.IsValidRussianPhoneNumber(Phone) = 1);
GO

-- добавлениие проверки коректности DeliveryDate для Orders
ALTER TABLE Orders ADD CONSTRAINT checkDeliveryDate CHECK (DeliveryDate >= getdate());
GO


-- создание хранимых процедур
DROP PROCEDURE IF EXISTS CreateOrderWithItems
Go 
DROP TYPE IF EXISTS OrderItemsType
Go 
CREATE TYPE OrderItemsType AS TABLE (
    ProductID INT NOT NULL,
    Quantity INT NOT NULL,
    ActualPrice NUMERIC(8, 2) NOT NULL
);
GO

CREATE PROCEDURE CreateOrderWithItems
    @OrderNumber VARCHAR(20),
    @CustomerID INT,
    @Address NVARCHAR(200),
    @DeliveryDate DATE,
    @PaymentType TINYINT,
    @PaymentStatus BIT,
    @DeliveryCost INT,
    @OrderItems OrderItemsType READONLY -- Таблица для передачи позиций
AS
BEGIN
    BEGIN TRAN;
    BEGIN TRY
        -- 1. Создание заказа
        INSERT INTO Orders (OrderNumber, CustomerID, Address, DeliveryDate, PaymentType, PaymentStatus, DeliveryCost)
        VALUES (@OrderNumber, @CustomerID, @Address, @DeliveryDate, @PaymentType, @PaymentStatus, @DeliveryCost);

        -- Получаем сгенерированный идентификатор заказа
        DECLARE @NewOrderID INT = SCOPE_IDENTITY();

        -- 2. Добавление позиций в заказ
        INSERT INTO OrderItem (OrderID, ProductID, Quantity, ActualPrice)
        SELECT @NewOrderID, ProductID, Quantity, ActualPrice
        FROM @OrderItems;

        -- Завершаем транзакцию
        COMMIT;
        PRINT 'succesfull creating order and items';
    END TRY
    BEGIN CATCH
        -- Откат транзакции при ошибке
        ROLLBACK;
        RAISERROR ('succesfull creating order and items',1,1);
    END CATCH;
END;
GO



-- создание тригеров
-- запрет удаления покупателя
DROP TRIGGER IF EXISTS CustomerDelete
Go 
CREATE TRIGGER CustomerDelete 
    ON Customer 
    INSTEAD OF DELETE
    AS RAISERROR('You cant delete customer!', 1, 1)
go

DROP TRIGGER IF EXISTS OrderDelete
Go
-- запрет удаления оплаченного заказа
DROP TRIGGER IF EXISTS OrderDelete
GO
CREATE TRIGGER OrderDelete
    ON Orders
    INSTEAD OF  DELETE
    AS 
    IF (SELECT COUNT(*) FROM deleted WHERE PaymentStatus = 1) > 0
        RAISERROR('You cant delete a paid order!', 1, 1)
    ELSE
        DELETE FROM Orders WHERE OrderID in (SELECT OrderID FROM deleted)
GO

DROP TRIGGER IF EXISTS SellerDelete
Go
-- запрет удаления продавца
CREATE TRIGGER SellerDelete 
    ON Seller
    INSTEAD OF DELETE
    AS RAISERROR('You cant delete seller!', 1, 1)
go

DROP TRIGGER IF EXISTS OrderItemDelete
Go
-- запрет удаления сразу всех позиций в заказе, в заказе должна остаться хотя бы одна позиция
CREATE TRIGGER OrderItemDelete 
    ON OrderItem
    AFTER DELETE AS 
    BEGIN
        IF EXISTS (
            SELECT 1
            FROM Orders o
            WHERE o.OrderID IN (
                SELECT DISTINCT OrderID
                FROM DELETED
            )
            AND NOT EXISTS (
                SELECT 1
                FROM OrderItem oi
                WHERE oi.OrderID = o.OrderID
                AND oi.OrderItemID NOT IN (SELECT OrderItemID FROM DELETED)
            )
        )
        BEGIN
            -- Если хотя бы один заказ останется без позиций, блокируем удаление
            RAISERROR ('You cannot delete all items from an order. There must be at least one item left in the order.', 2, 1);
            ROLLBACK TRANSACTION;
        END;
    END;
GO

-- запрет добавления заказа без позиций
DROP TRIGGER IF EXISTS OrderInsert
Go
CREATE TRIGGER OrderInsert
    on Orders
    AFTER INSERT AS
        BEGIN
            -- Проверка, существуют ли связанные записи в OrderItem
            IF NOT EXISTS (
                SELECT 1
                FROM inserted i
                JOIN OrderItem oi ON i.OrderID = oi.OrderID
            )
            BEGIN
                -- Если нет связанных записей, отменить операцию
                RAISERROR ('You cant create order without items. Order must include at least one order item.', 2, 1);
                ROLLBACK TRANSACTION;
            END;
        END;
GO


-- создание представлений
DROP VIEW IF EXISTS OrdersAndItemsView
GO

CREATE VIEW OrdersAndItemsView as 
SELECT OrderNumber, CustomerID, Address, DeliveryDate, PaymentType, PaymentStatus, DeliveryCost, ProductID, Quantity, ActualPrice FROM Orders as o INNER JOIN OrderItem as i on o.OrderID = i.OrderID
GO

select * from OrdersAndItemsView
go

DROP VIEW IF EXISTS SellersProducts
GO

CREATE VIEW SellersProducts as 
SELECT s.SellerID, s.OGRN, s.Name, p.ProductID, p.ProductName, p.SKU, p.Price, p.StockQuantity  FROM Seller as s LEFT JOIN Product as p on s.SellerID = p.SellerID
go 

select * from SellersProducts
go

-- создание индекса
CREATE INDEX Customer_Idx on Customer (Email)
INCLUDE (FirstName, Phone)

-- тестирование
-- добавление данных для Customer, Seller и Product
INSERT INTO Customer (Email, FirstName, LastName, Phone)
VALUES ('www@gmail.com', 'name1', 'lastname1', '+79848484809'),
        ('ww1@gmail.com', 'name2', 'lastname2', '+79848484891'),
        ('ww2@gmail.com', 'name3', 'lastname3', '+79848484892'),
        ('ww3@gmail.com', 'name4', 'lastname4', '+79848484893');


INSERT INTO Seller (OGRN, Name, Description, Address)
VALUES ('123456789012345', 'Seller 1', 'First Seller', 'Address 1'),
       ('987654321098763', 'Seller 2', 'Second Seller', 'Address 2'),
       ('987654321098761', 'Seller 3', 'Third Seller', 'Address 3'),
       ('987654321098762', 'Seller 4', 'Forth Seller', 'Address 4');
GO

-- добавление некоректных данных для Customer
INSERT INTO Customer (Email, FirstName, LastName, Phone)
VALUES ('wqwq@.com', 'aaaaa', 'bbbbb', '+79848484811') -- некоректный email
GO

INSERT INTO Customer (Email, FirstName, LastName, Phone)
VALUES ('wqwq@gmail.com', 'aaaaa', 'bbbbb', '+7984848480') -- некоректный phone
GO



select * from Customer
GO
select * from Seller
GO

INSERT INTO Product (SellerID, SKU, ProductName, Description, OriginCountry, Price, StockQuantity)
VALUES ((SELECT SellerID FROM Seller WHERE Name = 'Seller 1'), 'SKU1235', 'Product 1', 'First product', 'RUS', 100.00, 10),
       ((SELECT SellerID FROM Seller WHERE Name = 'Seller 2'), 'SKU4565', 'Product 2', 'Second product', 'RUS', 200.00, 20),
       (3, 'SKU4566', 'Product 3', 'Third product', 'RUS', 230.00, 13),
       (4, 'SKU4567', 'Product 4', 'Forth product', 'RUS', 50.00, 28);

GO

select * from Product
Go

-- добавление заказов
-- Создаём табличную переменную для передачи позиций
DECLARE @OrderItems OrderItemsType;

-- Добавляем позиции в табличную переменную
INSERT INTO @OrderItems (ProductID, Quantity, ActualPrice)
VALUES 
    (1, 2, 100.50),  -- Первая позиция
    (2, 1, 150.00);  -- Вторая позиция

-- Вызываем хранимую процедуру
EXEC CreateOrderWithItems
    @OrderNumber = 'ORD-005',
    @CustomerID = 2,
    @Address = '456 New Street',
    @DeliveryDate = '2025-01-25',
    @PaymentType = 1,
    @PaymentStatus = 1,
    @DeliveryCost = 500,
    @OrderItems = @OrderItems;
GO

-- теперь пробуем добавить просто заказ без позиций
INSERT INTO Orders (OrderNumber, CustomerID, Address, DeliveryDate, Discount, PaymentType, PaymentStatus, DeliveryCost)
    VALUES ('1234567890123456799', 2, 'wqwqwwqqwwq', CONVERT(date,N'11-28-2025'), 5, 0, 0, 100),
    ('1234567890123456721', 3, 'wqwqwwqqwwq', CONVERT(date,N'11-28-2025'), 5, 0, 0, 100);
go
select * from Orders
go


SELECT * from OrdersAndItemsView

-- изменение ключа у Customer
UPDATE Customer SET CustomerID = 10 WHERE CustomerID = 1 -- не будет работать, так как identity
GO
-- для других таблиц изменение ключа должно работать также

DELETE FROM Customer WHERE CustomerID = 1 -- должна появиться ошибка
DELETE FROM Seller WHERE SellerID = 1 -- тоже должна быть ошибка
GO

UPDATE Orders SET PaymentStatus = 0 WHERE OrderID = 3
GO

DELETE FROM Orders WHERE OrderID = 3 -- должны каскадно удалиться все позиции, связанные с этим заказом, если заказ не оплачен
SELECT * FROM Orders
SELECT * FROM OrderItem

DELETE FROM OrderItem WHERE OrderID = 3 -- должна появиться ошибка, так как нельзя удалять все позиции из заказа
GO

SELECT * FROM OrderItem

DELETE FROM OrderItem WHERE OrderItemID = 4 -- должно нормально удалиться
GO

SELECT * FROM OrderItem

DELETE FROM Product WHERE ProductID = 2 -- должна появиться ошибка, если он содержится в какой-либо из позиций
go

SELECT * from Orders ORDER BY DeliveryDate
GO

SELECT c.FirstName , SUM(i.ActualPrice*i.Quantity) AS price_sum 
FROM OrderItem as i INNER JOIN Orders AS o ON i.OrderID = o.OrderID INNER JOIN Customer as c ON c.CustomerID = o.CustomerID
GROUP BY c.FirstName
HAVING SUM(i.ActualPrice*i.Quantity) > 40
ORDER BY price_sum DESC
GO

-- использование RIGHT JOIN / FULL OUTER JOIN
SELECT s.SellerID, s.OGRN, s.Name, p.ProductID, p.ProductName, p.SKU, p.Price, p.StockQuantity FROM Product as p RIGHT JOIN Seller as s on p.SellerID = s.SellerID
go

SELECT s.SellerID, s.OGRN, s.Name, p.ProductID, p.ProductName, p.SKU, p.Price, p.StockQuantity FROM Product as p FULL OUTER JOIN Seller as s on p.SellerID = s.SellerID
go

-- удаление базы данных
USE master
go

DROP DATABASE IF EXISTS lab11
GO