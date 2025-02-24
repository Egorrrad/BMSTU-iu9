-- leftPart
use master;
go

DROP DATABASE IF EXISTS lab10
go

CREATE DATABASE lab10
go 

use lab10;
go 

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

DROP FUNCTION if EXISTS getLocksInfo
go

-- функция для просмотра блокировок
CREATE FUNCTION getLocksInfo()
RETURNS TABLE
AS
RETURN
(
    SELECT 
    l.request_session_id, 
    l.request_type,
    CASE s.transaction_isolation_level 
        WHEN 1 THEN 'READ UNCOMMITTED'
        WHEN 2 THEN 'READ COMMITTED'
        WHEN 3 THEN 'REPEATABLE READ'
        WHEN 4 THEN 'SERIALIZABLE'
    END AS transaction_isolation_level, 
    l.request_mode, 
    l.resource_type,
    l.request_status, 
    l.resource_database_id, 
    DB_NAME(l.resource_database_id) AS database_name
    FROM 
        sys.dm_tran_locks l
    JOIN 
        sys.dm_exec_sessions s ON l.request_session_id = s.session_id
);
GO 

-- по очереди устанавливаем различные уровни блокировки и тестируем поведение
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED

SET TRANSACTION ISOLATION LEVEL READ COMMITTED

SET TRANSACTION ISOLATION LEVEL REPEATABLE READ

SET TRANSACTION ISOLATION LEVEL SERIALIZABLE

-- добавляем данные
INSERT INTO Sellers (OGRN, Name, Description, Address) VALUES
('123456789012', 'Seller A', 'Description for Seller A', 'Address A'),
('234567890123', 'Seller B', 'Description for Seller B', 'Address B'),
('345678901234', 'Seller C', 'Description for Seller C', 'Address C'),
('456789012345', 'Seller D', 'Description for Seller D', 'Address D'),
('567890123456', 'Seller E', 'Description for Seller E', 'Address E');
GO

-- грязное чтение (dirty read)
BEGIN TRANSACTION
    select Name, Address from Sellers
    UPDATE Sellers SET Address = 'Updated A adress' WHERE Name = 'Seller A'

    
    select Name, Address from Sellers

    WAITFOR DELAY '00:00:10'
    ROLLBACK

    select Name, Address from Sellers
    select * from getLocksInfo()
GO


-- невоспроизводимое чтение (non-repeatable read)
BEGIN TRANSACTION
    select Name, Address from Sellers
    UPDATE Sellers SET Address = 'Updated3 E adress' WHERE Name = 'Seller E'
    select Name, Address from Sellers
    select * from getLocksInfo()
COMMIT TRANSACTION
GO


-- фантомное чтение (phantom read)
BEGIN TRANSACTION
    select * from Sellers
    WAITFOR DELAY '00:00:10'
    select * from Sellers
    select * from getLocksInfo()
COMMIT TRANSACTION
go

-- rightPart
use lab10;
go 

-- грязное чтение (dirty read)
BEGIN TRANSACTION
    select Name, Address from Sellers
    select * from getLocksInfo()
COMMIT TRANSACTION
GO


-- невоспроизводимое чтение (non-repeatable read)
BEGIN TRANSACTION
    select Name, Address from Sellers
    WAITFOR DELAY '00:00:10'
    select Name, Address from Sellers
    select * from getLocksInfo()
COMMIT TRANSACTION
GO

-- фантомное чтение (phantom read)
BEGIN TRANSACTION
    select * from Sellers
    INSERT INTO Sellers (OGRN, Name, Description, Address) VALUES
        ('678901234567', 'Seller F', 'Description for Seller F', 'Address F'),
        ('789012345678', 'Seller G', 'Description for Seller G', 'Address G')
    select * from Sellers
    select * from getLocksInfo()
COMMIT TRANSACTION
GO