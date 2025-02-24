USE lab6
GO

-- удаление процедуры, если она уже создана
DROP PROCEDURE IF EXISTS SellersGetCursor;
GO

-- создание процедуры 
CREATE PROCEDURE SellersGetCursor
    @currency_cursor CURSOR VARYING OUTPUT
AS
SET @currency_cursor = CURSOR
FORWARD_ONLY STATIC FOR
SELECT SellerID, OGRN, Name, Description, Address
FROM Sellers;
OPEN @currency_cursor;
GO


-- проверка работы созданной процедуры
DECLARE @MyCursor CURSOR;
EXEC SellersGetCursor @currency_cursor = @MyCursor
OUTPUT;

FETCH NEXT FROM @MyCursor;
WHILE (@@FETCH_STATUS = 0)
BEGIN;
    FETCH NEXT FROM @MyCursor;
END;
CLOSE @MyCursor;
DEALLOCATE @MyCursor;  
GO

-- удаление функции, если она уже создана
DROP FUNCTION IF EXISTS GetCountProductOfSeller;
GO

-- создание пользовательской скалярной функции
CREATE FUNCTION GetCountProductOfSeller(@SellerID uniqueidentifier)
RETURNS int
AS -- возвращает количество типов товаров у продавца
BEGIN
    DECLARE @ret int;
    SELECT @ret =  COUNT(*)
    FROM Products p
    WHERE p.SellerID = @SellerID;
    IF (@ret IS NULL)
    SET @ret = 0
    RETURN @ret
END;
GO

-- добавление в процедуру столбца, формируемого пользовательской функцией 
ALTER PROCEDURE SellersGetCursor
    @currency_cursor CURSOR VARYING OUTPUT
AS
SET @currency_cursor = CURSOR
FORWARD_ONLY STATIC FOR
SELECT SellerID, OGRN, Name, Description, Address, dbo.GetCountProductOfSeller(SellerID) as ProductsTypes
FROM Sellers;
OPEN @currency_cursor;
GO

-- удаление функции, если она уже создана
DROP FUNCTION IF EXISTS NonZeroProductOfSeller;
GO

-- создание еще одной пользовательской скалярной функции
CREATE FUNCTION NonZeroProductOfSeller(@Counts int)
RETURNS BIT
AS -- возвращает 1, если у продавца не 0 товаров, и 0 иначе
BEGIN
    DECLARE @result BIT;
    SET @result = 1;
    IF (@Counts = 0)
    SET @result = 0
    RETURN @result
END;
GO

-- удаление процедуры, если она уже создана
DROP PROCEDURE IF EXISTS ProcessSellersWithProductsCursor;
GO

-- создание процедуры, использующей другую процедуру
CREATE PROCEDURE ProcessSellersWithProductsCursor
AS
DECLARE @MyCursor1 CURSOR;
DECLARE  @SellerID uniqueidentifier, @OGRN nvarchar(100), @Name nvarchar(100), @Description nvarchar(1000), @Address varchar(200), @ProductCount INT
EXEC SellersGetCursor @currency_cursor = @MyCursor1
OUTPUT;

FETCH NEXT FROM @MyCursor1 INTO @SellerID, @OGRN, @Name, @Description, @Address, @ProductCount;
WHILE (@@FETCH_STATUS = 0)
BEGIN
    IF (dbo.NonZeroProductOfSeller(@ProductCount) = 1)
    PRINT N'Продавец ' + @Name + N' с ОГРН ' + @OGRN + N' имеет ' + CAST ( @ProductCount AS VARCHAR(100))   + N' типов товаров';
    FETCH NEXT FROM @MyCursor1 INTO @SellerID, @OGRN, @Name, @Description, @Address, @ProductCount;
END
CLOSE @MyCursor1;
DEALLOCATE @MyCursor1;
GO


-- проверка работы созданной процедуры
EXEC ProcessSellersWithProductsCursor;
GO

-- удаление функции, если она уже создана
DROP FUNCTION IF EXISTS GetSelersProductWithCounts;
GO

-- создание пользовательской табличной функции
CREATE FUNCTION GetSelersProductWithCounts ()
RETURNS table AS
RETURN 
(
	SELECT SellerID, OGRN, Name, Description, Address, dbo.GetCountProductOfSeller(SellerID) as ProductsTypes
    FROM Sellers
)
GO

-- удаление функции, если она уже создана
DROP FUNCTION IF EXISTS GetSelersProductWithCounts1;
GO

-- создание пользовательской табличной функции в полной форме
CREATE FUNCTION GetSelersProductWithCounts1 ()
RETURNS @ResultTable TABLE 
(
    SellerID uniqueidentifier,
    OGRN NVARCHAR(100),
    Name NVARCHAR(100),
    Description NVARCHAR(400),
    Address NVARCHAR(200),
    ProductsTypes INT
)
AS
BEGIN
    INSERT INTO @ResultTable
    SELECT 
        SellerID, 
        OGRN, 
        Name, 
        Description, 
        Address, 
        dbo.GetCountProductOfSeller(SellerID) AS ProductsTypes
    FROM Sellers;
    RETURN;
END;
GO

-- изменение процедуры для формирования выборки с помощью табличной функции 
ALTER PROCEDURE SellersGetCursor
    @currency_cursor CURSOR VARYING OUTPUT
AS
SET @currency_cursor = CURSOR
FORWARD_ONLY STATIC FOR
SELECT *
FROM dbo.GetSelersProductWithCounts();
OPEN @currency_cursor;
GO