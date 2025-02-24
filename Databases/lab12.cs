using System;
using System.Configuration;
using System.Data;
using System.Data.SqlClient;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("=== Тестирование программы ===");

        try
        {
            // Связный уровень
            Console.WriteLine("\n=== Связный уровень ===");
            TestConnected();  // Тестирование работы с данными через подключение

            // Несвязный уровень
            Console.WriteLine("\n=== Несвязный уровень ===");
            TestDisconnected();  // Тестирование работы с данными без постоянного подключения
        }
        catch (Exception err)
        {
            Console.WriteLine($"Произошла ошибка в приложении: {err.Message}");  // Обработка ошибок
        }

        Console.WriteLine("\nТестирование завершено!");
    }

    // Тестирование работы с данными в связном уровне
    static void TestConnected()
    {
        SellersRepositoryConnected repo = new SellersRepositoryConnected();

        try
        {
            // Получение и вывод всех записей перед тестом
            Console.WriteLine("\nСуществующие записи (до теста):");
            repo.GetSellers(reader =>
            {
                Console.WriteLine($"ID: {reader["SellerID"]}, OGRN: {reader["OGRN"]}, Name: {reader["Name"]}");
            });

            // Добавление новой записи в базу данных
            Console.WriteLine("\nДобавляем новую запись...");
            repo.InsertSeller(2, "11223344556677889900", "New Seller", "Test Description", "123 Main St");

            // Обновление записи с ID = 2
            Console.WriteLine("\nОбновляем запись с ID = 1...");
            repo.UpdateSeller(2, "Updated Seller Name", "456 Another St");

            // Удаление записи с ID = 1
            Console.WriteLine("\nУдаляем запись с ID = 10...");
            repo.DeleteSeller(10);

            // Получение и вывод оставшихся записей
            Console.WriteLine("\nОставшиеся записи:");
            repo.GetSellers(reader =>
            {
                Console.WriteLine($"ID: {reader["SellerID"]}, OGRN: {reader["OGRN"]}, Name: {reader["Name"]}");
            });
        }
        catch (Exception err)
        {
            Console.WriteLine($"Ошибка в связном уровне: {err.Message}");  // Обработка ошибок в связном уровне
        }
    }

    // Тестирование работы с данными в несвязном уровне
    static void TestDisconnected()
    {
        SellersRepositoryDisconnected repo = new SellersRepositoryDisconnected();

        try
        {
            // Получение всех записей до изменений и вывод на консоль
            Console.WriteLine("\nСуществующие записи (до теста):");
            DataTable sellers = repo.GetSellers();
            PrintTable(sellers);

            // Добавление новой записи в базу данных
            Console.WriteLine("\nДобавляем новую запись...");
            repo.InsertSeller(3, "99887766554433221100", "Another Seller", "Another Description", "456 Market St");

            // Обновление записи с ID = 3
            Console.WriteLine("\nОбновляем запись с ID = 3...");
            repo.UpdateSeller(3, "Updated Name Disconnected", "789 Another St");

            // Удаление записи с ID = 3
            Console.WriteLine("\nУдаляем запись с ID = 12...");
            repo.DeleteSeller(12);

            // Получение оставшихся записей после изменений и вывод на консоль
            Console.WriteLine("\nОставшиеся записи:");
            sellers = repo.GetSellers();
            PrintTable(sellers);
        }
        catch (Exception err)
        {
            Console.WriteLine($"Ошибка в несвязном уровне: {err.Message}");  // Обработка ошибок в несвязном уровне
        }
    }

    // Метод для печати содержимого DataTable
    static void PrintTable(DataTable table)
    {
        foreach (DataRow row in table.Rows)
        {
            Console.WriteLine($"ID: {row["SellerID"]}, OGRN: {row["OGRN"]}, Name: {row["Name"]}");
        }
    }
}

// Класс для работы с данными в связном режиме
public class SellersRepositoryConnected
{
    private readonly string _connectionString;

    public SellersRepositoryConnected()
    {
        // Получаем строку подключения из конфигурации
        _connectionString = ConfigurationManager.ConnectionStrings["DefaultConnection"].ConnectionString;
    }

    // Получение всех продавцов и выполнение действия с каждой строкой
    public void GetSellers(Action<SqlDataReader> handleRow)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "SELECT * FROM Sellers";  // SQL-запрос для получения всех данных
            SqlCommand command = new SqlCommand(query, connection);

            connection.Open();  // Открытие соединения с базой данных
            using (SqlDataReader reader = command.ExecuteReader())  // Выполнение запроса и получение данных
            {
                while (reader.Read())  // Чтение каждой строки данных
                {
                    handleRow(reader);  // Обработка каждой строки
                }
            }
        }
    }

    // Вставка нового продавца в базу данных
    public void InsertSeller(int id, string ogrn, string name, string description, string address)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "INSERT INTO Sellers (SellerID, OGRN, Name, Description, Address) VALUES (@SellerID, @OGRN, @Name, @Description, @Address)";
            SqlCommand command = new SqlCommand(query, connection);

            AddParameters(command, id, ogrn, name, description, address);  // Добавление параметров для запроса

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на вставку
        }
    }

    // Обновление данных продавца в базе данных
    public void UpdateSeller(int sellerId, string name, string address)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "UPDATE Sellers SET Name = @Name, Address = @Address WHERE SellerID = @SellerID";  // SQL-запрос на обновление
            SqlCommand command = new SqlCommand(query, connection);

            // Добавление параметров для обновления записи
            var paramName = new SqlParameter { ParameterName = "@Name", Value = name, SqlDbType = SqlDbType.NVarChar, Size = 50 };
            var paramAddress = new SqlParameter { ParameterName = "@Address", Value = address ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar, Size = 100 };
            var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };

            command.Parameters.Add(paramName);
            command.Parameters.Add(paramAddress);
            command.Parameters.Add(paramSellerID);

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на обновление
        }
    }

    // Удаление продавца по ID
    public void DeleteSeller(int sellerId)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "DELETE FROM Sellers WHERE SellerID = @SellerID";  // SQL-запрос на удаление
            SqlCommand command = new SqlCommand(query, connection);

            // Добавление параметра для удаления записи
            var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };

            command.Parameters.Add(paramSellerID);

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на удаление
        }
    }

    // Добавление параметров к SQL-команде
    private void AddParameters(SqlCommand command, int sellerId, string ogrn, string name, string description, string address)
    {
        var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };
        var paramOGRN = new SqlParameter { ParameterName = "@OGRN", Value = ogrn, SqlDbType = SqlDbType.VarChar, Size = 20 };
        var paramName = new SqlParameter { ParameterName = "@Name", Value = name, SqlDbType = SqlDbType.NVarChar, Size = 50 };
        var paramDescription = new SqlParameter { ParameterName = "@Description", Value = description ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar };
        var paramAddress = new SqlParameter { ParameterName = "@Address", Value = address ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar, Size = 100 };

        command.Parameters.Add(paramSellerID);
        command.Parameters.Add(paramOGRN);
        command.Parameters.Add(paramName);
        command.Parameters.Add(paramDescription);
        command.Parameters.Add(paramAddress);
    }
}

// Класс для работы с данными в несвязном режиме
public class SellersRepositoryDisconnected
{
    private readonly string _connectionString;

    public SellersRepositoryDisconnected()
    {
        // Получаем строку подключения из конфигурации
        _connectionString = ConfigurationManager.ConnectionStrings["DefaultConnection"].ConnectionString;
    }

    // Получение всех продавцов и возврат как DataTable
    public DataTable GetSellers()
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "SELECT * FROM Sellers";  // SQL-запрос для получения всех данных
            SqlDataAdapter adapter = new SqlDataAdapter(query, connection);

            DataTable sellersTable = new DataTable();  // Создание DataTable для хранения данных
            adapter.Fill(sellersTable);  // Заполнение DataTable

            return sellersTable;  // Возвращение таблицы с данными
        }
    }

    // Вставка нового продавца в базу данных
    public void InsertSeller(int id, string ogrn, string name, string description, string address)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "INSERT INTO Sellers (SellerID, OGRN, Name, Description, Address) VALUES (@SellerID, @OGRN, @Name, @Description, @Address)";
            SqlCommand command = new SqlCommand(query, connection);

            AddParameters(command, id, ogrn, name, description, address);  // Добавление параметров для запроса

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на вставку
        }
    }

    // Обновление данных продавца в базе данных
    public void UpdateSeller(int sellerId, string name, string address)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "UPDATE Sellers SET Name = @Name, Address = @Address WHERE SellerID = @SellerID";  // SQL-запрос на обновление
            SqlCommand command = new SqlCommand(query, connection);

            // Добавление параметров для обновления записи
            var paramName = new SqlParameter { ParameterName = "@Name", Value = name, SqlDbType = SqlDbType.NVarChar, Size = 50 };
            var paramAddress = new SqlParameter { ParameterName = "@Address", Value = address ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar, Size = 100 };
            var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };

            command.Parameters.Add(paramName);
            command.Parameters.Add(paramAddress);
            command.Parameters.Add(paramSellerID);

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на обновление
        }
    }

    // Удаление продавца по ID
    public void DeleteSeller(int sellerId)
    {
        using (SqlConnection connection = new SqlConnection(_connectionString))
        {
            string query = "DELETE FROM Sellers WHERE SellerID = @SellerID";  // SQL-запрос на удаление
            SqlCommand command = new SqlCommand(query, connection);

            // Добавление параметра для удаления записи
            var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };

            command.Parameters.Add(paramSellerID);

            connection.Open();  // Открытие соединения
            command.ExecuteNonQuery();  // Выполнение команды на удаление
        }
    }

    // Добавление параметров к SQL-команде
    private void AddParameters(SqlCommand command, int sellerId, string ogrn, string name, string description, string address)
    {
        var paramSellerID = new SqlParameter { ParameterName = "@SellerID", Value = sellerId, SqlDbType = SqlDbType.Int };
        var paramOGRN = new SqlParameter { ParameterName = "@OGRN", Value = ogrn, SqlDbType = SqlDbType.VarChar, Size = 20 };
        var paramName = new SqlParameter { ParameterName = "@Name", Value = name, SqlDbType = SqlDbType.NVarChar, Size = 50 };
        var paramDescription = new SqlParameter { ParameterName = "@Description", Value = description ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar };
        var paramAddress = new SqlParameter { ParameterName = "@Address", Value = address ?? (object)DBNull.Value, SqlDbType = SqlDbType.NVarChar, Size = 100 };

        command.Parameters.Add(paramSellerID);
        command.Parameters.Add(paramOGRN);
        command.Parameters.Add(paramName);
        command.Parameters.Add(paramDescription);
        command.Parameters.Add(paramAddress);
    }
}