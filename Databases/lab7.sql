use lab6
GO


-- удаление представления, если оно уже есть
drop VIEW IF EXISTS customers_all
GO

-- создание представления
CREATE VIEW customers_all
AS
    SELECT
        CustomerID, Email, FirstName, LastName, Phone
    FROM Customers
GO

-- проверка работы представления
select *
from customers_all


-- добавляем данные


-- Вставляем данные в Sellers

INSERT INTO Sellers
    (OGRN, Name, Description, Address)
VALUES
    ('1234567890123453', 'Seller 5', 'First Seller', 'Address 1'),
    ('9876543210987632', 'Seller 6', 'Second1 Seller', 'Address 2'),
    ('9876543210987611', 'Seller 7', 'Second2 Seller', 'Address 3'),
    ('9876543210987625', 'Seller 8', 'Second3 Seller', 'Address 4');


-- Вставляем данные в Products

INSERT INTO Products
    (SellerID, SKU, ProductName, Description, OriginCountry, Price, StockQuantity)
VALUES
    ((SELECT SellerID
        FROM Sellers
        WHERE Name = 'Seller 1'), 'SKU12354830', 'Product 12', 'First product', 'Country 1', 140.00, 3),
    ((SELECT SellerID
        FROM Sellers
        WHERE Name = 'Seller 2'), 'SKU45652930', 'Product 56', 'Second product', 'Country 2', 210.00, 6);


-- удаляем представление, если оно уже есть
DROP VIEW IF EXISTS products_sellers
GO

-- создание представления для связанных таблиц
CREATE VIEW products_sellers
WITH
    SCHEMABINDING
AS
    SELECT p.ProductID, p.ProductName as Product, p.SKU, p.Price as Price, s.Name as Seller, p.StockQuantity as OnStock
    FROM dbo.Products p INNER JOIN dbo.Sellers s ON p.SellerID = s.SellerID
GO

-- удаляем индекс, если он уже есть
DROP INDEX IF EXISTS Customer_Idx on Customers


-- создание индекса
CREATE INDEX Customer_Idx on Customers (Email)
INCLUDE (FirstName, Phone)

SELECT Email, FirstName, Phone from Customers where Email like '%.ru'
SELECT  Phone from Customers where Email = 'ya.ru'


-- удаляем индекс, если он уже есть
DROP INDEX IF EXISTS products_sellers_idx ON products_sellers


-- cоздание индексированного представления
CREATE UNIQUE CLUSTERED INDEX products_sellers_idx ON products_sellers (
    ProductID
);