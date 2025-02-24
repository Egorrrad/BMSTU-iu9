USE master;

GO
IF DB_ID (N'lab5') IS NOT NULL
DROP DATABASE lab5;
GO

-- создание базы данных
CREATE DATABASE lab5
ON ( 
    NAME = lab5, 
    FILENAME = '/home/data/lab5.mdf',
    SIZE = 10, 
    MAXSIZE = 15, 
    FILEGROWTH = 5
    )
LOG 
ON ( 
    NAME = lab5_log, 
    FILENAME = '/home/log/lab5log.log',
    SIZE = 5, 
    MAXSIZE = 20, 
    FILEGROWTH = 5
    );
GO

use lab5
go

if OBJECT_ID(N'Customers') is NOT NULL
	DROP Table Customers;
go

-- создание таблицы 
CREATE TABLE Customers
(
    CustomerID INT PRIMARY KEY NOT NULL,
    Email VARCHAR(254) NOT NULL UNIQUE,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    Phone VARCHAR(15) NOT NULL UNIQUE,
    BirthDate DATE NULL
);
go

-- создание файловой группы и файла данных
alter database lab5
add filegroup filegrouplab5
go

alter database lab5
add file(
	NAME = mydb_lab5,
	FILENAME = '/home/data/mydb_lab5.ndf',
	SIZE = 20MB,
	MAXSIZE = 100MB,
	FILEGROWTH = 5MB
)

to filegroup filegrouplab5
go


alter database lab5 modify  filegroup filegrouplab5 default
go

if OBJECT_ID(N'Sellers') is NOT NULL
	DROP Table Sellers;
go

-- Таблица Seller
CREATE TABLE Sellers (
    SellerID INT PRIMARY KEY,
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    Address NVARCHAR(100) NULL
);
go 


-- создание новой файловой группы
ALTER DATABASE lab5
ADD FILEGROUP new_filegrouplab5;
GO

ALTER DATABASE lab5
ADD FILE (
    NAME = mydb_lab5_new,
    FILENAME = '/home/data/mydb_lab5_new.ndf',
    SIZE = 20MB,
    MAXSIZE = 100MB,
    FILEGROWTH = 5MB
) 
TO FILEGROUP new_filegrouplab5;
GO

-- создаем таблицу в новой файловой группе
CREATE TABLE Sellers_New (
    SellerID INT PRIMARY KEY,
    OGRN VARCHAR(20) NOT NULL UNIQUE,
    Name NVARCHAR(50) NOT NULL,
    Description NVARCHAR(MAX) NULL,
    Address NVARCHAR(100) NULL
) ON new_filegrouplab5;
GO

-- копируем данные
INSERT INTO Sellers_New (SellerID, OGRN, Name, Description, Address)
SELECT SellerID, OGRN, Name, Description, Address
FROM Sellers;
GO

-- переименуем таблицу
EXEC sp_rename 'Sellers_New', 'Sellers';
GO

alter database lab5
	modify filegroup [primary] default;
go

drop table Sellers
go 


ALTER DATABASE lab5
REMOVE FILE mydb_lab5;
go 

ALTER DATABASE lab5
REMOVE FILEGROUP filegrouplab5;

-- создание схемы

CREATE SCHEMA store
go 

-- перемещение таблицы

ALTER SCHEMA store transfer dbo.Customers
go 

-- удаление таблицы и схемы
DROP TABLE store.Customers
DROP SCHEMA store
GO

