DROP DATABASE IF EXISTS lab141
go

CREATE DATABASE lab141
GO

DROP DATABASE IF EXISTS lab142
go

CREATE DATABASE lab142
GO

USE lab141
go

DROP TABLE IF EXISTS SellersBase
GO
-- Создание основной таблицы SellersBase
CREATE TABLE SellersBase (
    SellerID INT PRIMARY KEY NOT NULL, 
    OGRN VARCHAR(20) NOT NULL UNIQUE,               
    Name NVARCHAR(50) NOT NULL
);
GO

USE lab142
go

DROP TABLE IF EXISTS SellersDetails
GO
-- Создание таблицы с расширенными данными SellersDetails
CREATE TABLE SellersDetails (
    SellerID INT PRIMARY KEY NOT NULL,              -- Связь с SellersBase
    Description NVARCHAR(MAX) NULL,                 
    Address NVARCHAR(100) NULL
);
GO

use lab141
GO

DROP VIEW IF EXISTS SellersView
GO
CREATE VIEW  SellersView AS
    SELECT s1.SellerID, s1.OGRN, s1.Name, s2.Description, s2.Address FROM lab141.dbo.SellersBase AS s1
    INNER JOIN lab142.dbo.SellersDetails AS s2
    ON s1.SellerID = s2.SellerID
GO



-- Триггер для вставки в представление SellersView
DROP TRIGGER IF EXISTS trg_Insert_SellersView;
GO

CREATE TRIGGER trg_Insert_SellersView
ON SellersView
INSTEAD OF INSERT
AS
BEGIN
    INSERT INTO lab141.dbo.SellersBase (SellerID, OGRN, Name)
    SELECT SellerID, OGRN, Name FROM inserted

    INSERT INTO lab142.dbo.SellersDetails (SellerID, Description, Address)
    SELECT SellerID, Description, Address FROM inserted

END;
GO

-- Триггер для обновления представления SellersView
DROP TRIGGER IF EXISTS trg_Insert_SellersView;
GO

CREATE TRIGGER trg_Update_SellersView ON SellersView
INSTEAD OF UPDATE
AS
BEGIN
    IF UPDATE(SellerID)
    BEGIN
        RAISERROR('Updating SellerID is not allowed!', 16, 1);
        RETURN;
    END;

    UPDATE lab141.dbo.SellersBase
    SET OGRN = inserted.OGRN, Name = inserted.Name
    FROM inserted 
    WHERE lab141.dbo.SellersBase.SellerID = inserted.SellerID

    UPDATE lab142.dbo.SellersDetails
    SET Description = inserted.Description, Address = inserted.Address
    FROM inserted 
    WHERE lab142.dbo.SellersDetails.SellerID = inserted.SellerID
END
GO

-- Триггер для удаления из представления SellersView
DROP TRIGGER IF EXISTS trg_Insert_SellersView;
GO

CREATE TRIGGER trg_Delete_SellersView ON SellersView
INSTEAD OF DELETE
AS
    DELETE FROM lab141.dbo.SellersBase WHERE SellerID IN (SELECT SellerID FROM deleted)
    DELETE FROM lab142.dbo.SellersDetails WHERE SellerID IN (SELECT SellerID FROM deleted)
GO

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

-- проверка работы Update
UPDATE SellersView SET Address = 'Updated A adress' WHERE Name = 'Seller A'
GO

-- тут ошибка должна быть 
UPDATE SellersView SET SellerID = 8 WHERE SellerID = 1
GO

-- проверка работы Delete
DELETE FROM SellersView WHERE Address = 'Address E'
GO

-- проверка Select
use lab141
SELECT * FROM SellersView
GO


select * from lab141.dbo.SellersBase
GO

select * from lab142.dbo.SellersDetails
GO