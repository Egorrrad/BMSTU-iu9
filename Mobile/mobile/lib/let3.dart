import 'package:flutter/cupertino.dart';
import 'package:mysql1/mysql1.dart';

class DatabaseStates extends StatefulWidget {
  const DatabaseStates({super.key});

  @override
  State<DatabaseStates> createState() => DatabaseState();
}

class DatabaseState extends State<DatabaseStates> {
  int _currentScreen = 0; // 0 - авторизация, 1 - форма, 2 - таблица

  // Данные авторизации
  String _host = '';
  String _database = '';
  String _login = '';
  String _password = '';
  String _surname = '';

  // Данные формы
  String _name = '';
  String _email = '';
  String _age = '';

  // Данные таблицы
  List<Map<String, dynamic>> _tableData = [];
  MySqlConnection? _connection;
  bool _isLoading = false;
  bool _isTableCreated = false;

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        middle: Text(_getAppBarTitle()),
        trailing: _currentScreen == 2
            ? CupertinoButton(
          padding: EdgeInsets.zero,
          child: const Icon(CupertinoIcons.arrow_left_circle_fill),
          onPressed: _goBack,
        )
            : null,
      ),
      child: SafeArea(
        child: Stack(
          children: [
            _buildCurrentScreen(),
            if (_isLoading)
              Container(
                color: CupertinoColors.black.withOpacity(0.3),
                child: const Center(
                  child: CupertinoActivityIndicator(radius: 16),
                ),
              ),
          ],
        ),
      ),
    );
  }

  String _getAppBarTitle() {
    switch (_currentScreen) {
      case 0: return 'Авторизация';
      case 1: return 'Добавить запись';
      case 2: return 'Таблица данных (${_tableData.length})';
      default: return 'Летучка 3';
    }
  }

  Widget _buildCurrentScreen() {
    switch (_currentScreen) {
      case 0: return _buildAuthScreen();
      case 1: return _buildFormScreen();
      case 2: return _buildTableScreen();
      default: return _buildAuthScreen();
    }
  }

  Widget _buildAuthScreen() {
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Column(
        children: [
          _buildCupertinoTextField('HOST', _host, (value) => _host = value, placeholder: 'students.yss.su'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('БД', _database, (value) => _database = value, placeholder: 'iu9mobile'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Login', _login, (value) => _login = value, placeholder: 'iu9mobile'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Password', _password, (value) => _password = value,
              isPassword: true, placeholder: 'bmstubmstu123'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Surname', _surname, (value) => _surname = value, placeholder: 'Ваша фамилия'),
          const SizedBox(height: 24),
          CupertinoButton.filled(
            child: const Text('Подключиться'),
            onPressed: _handleAuth,
          ),
        ],
      ),
    );
  }

  Widget _buildFormScreen() {
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Column(
        children: [
          _buildCupertinoTextField('Name', _name, (value) => _name = value, placeholder: 'Введите имя'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Email', _email, (value) => _email = value, placeholder: 'Введите email'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Age', _age, (value) => _age = value,
              keyboardType: TextInputType.number, placeholder: 'Введите возраст'),
          const SizedBox(height: 24),
          Row(
            children: [
              Expanded(
                child: CupertinoButton.filled(
                  child: const Text('Добавить'),
                  onPressed: _handleFormSubmit,
                ),
              ),
              const SizedBox(width: 12),
              Expanded(
                child: CupertinoButton(
                  child: const Text('Очистить'),
                  onPressed: _handleFormClear,
                ),
              ),
            ],
          ),
          const SizedBox(height: 12),
          CupertinoButton(
            child: const Text('Показать таблицу'),
            onPressed: _loadTableData,
          ),
        ],
      ),
    );
  }

  Widget _buildTableScreen() {
    return Column(
      children: [
        // Заголовок таблицы
        Container(
          decoration: BoxDecoration(
            color: CupertinoColors.systemGrey6,
            border: Border(
              bottom: BorderSide(color: CupertinoColors.systemGrey4),
            ),
          ),
          child: Padding(
            padding: const EdgeInsets.symmetric(vertical: 12, horizontal: 16),
            child: Row(
              children: [
                _buildTableHeaderText('ID', 1),
                _buildTableHeaderText('Name', 2),
                _buildTableHeaderText('Email', 2.5),
                _buildTableHeaderText('Age', 1),
                _buildTableHeaderText('', 0.8),
              ],
            ),
          ),
        ),

        // Данные таблицы
        Expanded(
          child: _tableData.isEmpty
              ? Center(
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Icon(
                  CupertinoIcons.table,
                  size: 64,
                  color: CupertinoColors.systemGrey3,
                ),
                const SizedBox(height: 16),
                Text(
                  'Нет данных в таблице',
                  style: CupertinoTheme.of(context).textTheme.textStyle.copyWith(
                    color: CupertinoColors.systemGrey,
                  ),
                ),
                const SizedBox(height: 8),
                CupertinoButton(
                  child: const Text('Обновить'),
                  onPressed: _loadTableData,
                ),
              ],
            ),
          )
              : CupertinoScrollbar(
            child: ListView.separated(
              itemCount: _tableData.length,
              separatorBuilder: (context, index) => Container(
                height: 0.5,
                color: CupertinoColors.systemGrey4,
              ),
              itemBuilder: (context, index) {
                final item = _tableData[index];
                return _buildTableRow(item, index);
              },
            ),
          ),
        ),

        // Кнопки управления
        Padding(
          padding: const EdgeInsets.all(16.0),
          child: Row(
            children: [
              Expanded(
                child: CupertinoButton(
                  child: const Text('Обновить'),
                  onPressed: _loadTableData,
                ),
              ),
              const SizedBox(width: 12),
              Expanded(
                child: CupertinoButton.filled(
                  child: const Text('Назад'),
                  onPressed: _goBack,
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildTableHeaderText(String text, double flex) {
    return Expanded(
      flex: (flex * 10).round(),
      child: Text(
        text,
        style: TextStyle(
          fontWeight: FontWeight.w600,
          color: CupertinoColors.label.resolveFrom(context),
          fontSize: 14,
        ),
      ),
    );
  }

  Widget _buildTableRow(Map<String, dynamic> item, int index) {
    return Container(
      color: index.isEven ? CupertinoColors.systemBackground : CupertinoColors.systemGrey6,
      child: Padding(
        padding: const EdgeInsets.symmetric(vertical: 12, horizontal: 16),
        child: Row(
          children: [
            _buildTableCell(item['id'].toString(), 1, TextAlign.center),
            _buildTableCell(item['name']?.toString() ?? '-', 2, TextAlign.left),
            _buildTableCell(item['email']?.toString() ?? '-', 2.5, TextAlign.left),
            _buildTableCell(item['age']?.toString() ?? '-', 1, TextAlign.center),
            Expanded(
              flex: 8,
              child: CupertinoButton(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 4),
                minSize: 20,
                child: Icon(
                  CupertinoIcons.delete,
                  size: 18,
                  color: CupertinoColors.destructiveRed,
                ),
                onPressed: () => _showDeleteConfirmation(item['id']),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildTableCell(String text, double flex, TextAlign align) {
    return Expanded(
      flex: (flex * 10).round(),
      child: Text(
        text,
        textAlign: align,
        style: CupertinoTheme.of(context).textTheme.textStyle.copyWith(
          fontSize: 14,
        ),
        overflow: TextOverflow.ellipsis,
      ),
    );
  }

  Widget _buildCupertinoTextField(String label, String initialValue, Function(String) onChanged,
      {bool isPassword = false, TextInputType keyboardType = TextInputType.text, String placeholder = ''}) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          label,
          style: TextStyle(
            color: CupertinoColors.label.resolveFrom(context),
            fontSize: 14,
            fontWeight: FontWeight.w500,
          ),
        ),
        const SizedBox(height: 4),
        CupertinoTextField(
          obscureText: isPassword,
          keyboardType: keyboardType,
          onChanged: onChanged,
          placeholder: placeholder.isNotEmpty ? placeholder : 'Введите $label',
          controller: TextEditingController(text: initialValue),
          padding: const EdgeInsets.all(12),
          decoration: BoxDecoration(
            color: CupertinoColors.systemGrey6,
            border: Border.all(color: CupertinoColors.systemGrey4),
            borderRadius: BorderRadius.circular(8),
          ),
        ),
      ],
    );
  }

  Future<void> _handleAuth() async {
    if (_host.isEmpty || _database.isEmpty || _login.isEmpty || _surname.isEmpty) {
      _showAlert('Ошибка', 'Заполните все обязательные поля');
      return;
    }

    setState(() => _isLoading = true);

    try {
      final settings = ConnectionSettings(
        host: _host,
        port: 3306,
        user: _login,
        password: _password,
        db: _database,
      );

      _connection = await MySqlConnection.connect(settings);


      if (!_isTableCreated) {
        await _createPersonalTable();
        _isTableCreated = true;
      }

      _showAlert('Успех', 'Подключение к базе данных установлено\nТаблица: ${_getTableName()}');
      setState(() => _currentScreen = 1);

    } catch (e) {
      _showAlert('Ошибка подключения', 'Проверьте параметры подключения: $e');
    } finally {
      setState(() => _isLoading = false);
    }
  }


  String _getTableName() {
    final cleanSurname = _surname.replaceAll(RegExp(r'[^a-zA-Zа-яА-Я0-9]'), '_');
    return 'users_${cleanSurname.toLowerCase()}';
  }

  Future<void> _createPersonalTable() async {
    try {
      final tableName = _getTableName();


      await _connection!.query('''
        CREATE TABLE IF NOT EXISTS $tableName (
          id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
          name VARCHAR(255) NOT NULL,
          email VARCHAR(255) NOT NULL,
          age INT NOT NULL,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
      ''');

      print('Таблица создана: $tableName');

    } catch (e) {
      print('Ошибка создания таблицы: $e');
      rethrow;
    }
  }

  Future<void> _handleFormSubmit() async {
    if (_name.isEmpty || _email.isEmpty || _age.isEmpty) {
      _showAlert('Ошибка', 'Заполните все поля формы');
      return;
    }

    if (_connection == null) {
      _showAlert('Ошибка', 'Нет подключения к базе данных');
      return;
    }

    setState(() => _isLoading = true);

    try {
      final tableName = _getTableName();
      final result = await _connection!.query(
        'INSERT INTO $tableName (name, email, age) VALUES (?, ?, ?)',
        [_name, _email, int.tryParse(_age) ?? 0],
      );

      _showAlert('Успех', 'Запись добавлена (ID: ${result.insertId})');


      _handleFormClear();

    } catch (e) {
      _showAlert('Ошибка', 'Не удалось добавить запись: $e');
    } finally {
      setState(() => _isLoading = false);
    }
  }

  Future<void> _loadTableData() async {
    if (_connection == null) {
      _showAlert('Ошибка', 'Нет подключения к базе данных');
      return;
    }

    setState(() => _isLoading = true);

    try {
      final tableName = _getTableName();
      final results = await _connection!.query('SELECT id, name, email, age FROM $tableName ORDER BY id DESC');

      setState(() {
        _tableData = results.map((row) {
          return {
            'id': row[0],
            'name': row[1],
            'email': row[2],
            'age': row[3],
          };
        }).toList();
      });

      if (_tableData.isNotEmpty) {
        setState(() => _currentScreen = 2);
      } else {
        _showAlert('Информация', 'Таблица пустая. Добавьте записи.');
      }

    } catch (e) {
      _showAlert('Ошибка', 'Не удалось загрузить данные: $e');
    } finally {
      setState(() => _isLoading = false);
    }
  }

  void _handleFormClear() {
    setState(() {
      _name = '';
      _email = '';
      _age = '';
    });
  }

  void _showDeleteConfirmation(int id) {
    showCupertinoModalPopup(
      context: context,
      builder: (context) => CupertinoActionSheet(
        title: const Text('Удалить запись?'),
        message: const Text('Это действие нельзя отменить'),
        actions: [
          CupertinoActionSheetAction(
            onPressed: () {
              Navigator.pop(context);
              _deleteRecord(id);
            },
            isDestructiveAction: true,
            child: const Text('Удалить'),
          ),
        ],
        cancelButton: CupertinoActionSheetAction(
          onPressed: () => Navigator.pop(context),
          child: const Text('Отмена'),
        ),
      ),
    );
  }

  Future<void> _deleteRecord(int id) async {
    if (_connection == null) {
      _showAlert('Ошибка', 'Нет подключения к базе данных');
      return;
    }

    setState(() => _isLoading = true);

    try {
      final tableName = _getTableName();
      await _connection!.query('DELETE FROM $tableName WHERE id = ?', [id]);

      setState(() {
        _tableData.removeWhere((item) => item['id'] == id);
      });

      _showAlert('Успех', 'Запись удалена');

    } catch (e) {
      _showAlert('Ошибка', 'Не удалось удалить запись: $e');
    } finally {
      setState(() => _isLoading = false);
    }
  }

  void _goBack() {
    setState(() {
      if (_currentScreen == 2) {
        _currentScreen = 1;
      } else if (_currentScreen == 1) {
        _currentScreen = 0;
      }
    });
  }

  void _showAlert(String title, String message) {
    showCupertinoDialog(
      context: context,
      builder: (context) => CupertinoAlertDialog(
        title: Text(title),
        content: Text(message),
        actions: [
          CupertinoDialogAction(
            child: const Text('OK'),
            onPressed: () => Navigator.pop(context),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    _connection?.close();
    super.dispose();
  }
}