import 'dart:io';
import 'package:flutter/material.dart';
import 'package:ftpconnect/ftpconnect.dart';
import 'package:file_picker/file_picker.dart';
import 'package:path_provider/path_provider.dart';
import 'package:open_file/open_file.dart';

class FTPClientScreen extends StatefulWidget {
  @override
  _FTPClientScreenState createState() => _FTPClientScreenState();
}

class _FTPClientScreenState extends State<FTPClientScreen> {
  final FTPConnect _ftpConnect = FTPConnect(
    "students.yss.su",
    user: "ftpiu8",
    pass: "3Ru7yOTA",
  );

  List<FTPEntry> _fileList = [];
  String _currentPath = '/';
  bool _isLoading = false;
  bool _isConnected = false;
  String _statusMessage = 'Не подключено';

  @override
  void initState() {
    super.initState();
    _connectAndList();
  }

  @override
  void dispose() {
    _disconnect();
    super.dispose();
  }

  Future<void> _connectAndList() async {
    setState(() {
      _isLoading = true;
      _statusMessage = 'Подключение...';
    });

    try {
      await _ftpConnect.connect();
      setState(() {
        _isConnected = true;
        _statusMessage = 'Подключено';
      });
      await _listFiles();
    } catch (e) {
      setState(() {
        _statusMessage = 'Ошибка подключения: ${e.toString()}';
        _isConnected = false;
      });
      _showSnackBar('Ошибка подключения: ${e.toString()}', isError: true);
    } finally {
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _disconnect() async {
    try {
      if (_isConnected) {
        await _ftpConnect.disconnect();
        setState(() {
          _isConnected = false;
          _statusMessage = 'Отключено';
        });
      }
    } catch (e) {
      print('Ошибка отключения: $e');
    }
  }

  Future<void> _listFiles() async {
    if (!_isConnected) {
      _showSnackBar('Не подключено к серверу', isError: true);
      return;
    }

    setState(() {
      _isLoading = true;
      _statusMessage = 'Загрузка списка файлов...';
    });

    try {
      final files = await _ftpConnect.listDirectoryContent();
      setState(() {
        _fileList = files;
        _statusMessage = 'Найдено файлов: ${files.length}';
      });
    } catch (e) {
      setState(() {
        _statusMessage = 'Ошибка загрузки списка';
      });
      _showSnackBar('Ошибка загрузки списка: ${e.toString()}', isError: true);
    } finally {
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _navigateToDirectory(String dirName) async {
    setState(() {
      _isLoading = true;
      _statusMessage = 'Переход в директорию...';
    });

    try {
      await _ftpConnect.changeDirectory(dirName);
      _currentPath = await _ftpConnect.currentDirectory() ?? _currentPath;
      await _listFiles();
    } catch (e) {
      _showSnackBar('Ошибка перехода в директорию: ${e.toString()}', isError: true);
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _navigateUp() async {
    if (_currentPath == '/') {
      _showSnackBar('Вы уже в корневой директории');
      return;
    }

    setState(() {
      _isLoading = true;
      _statusMessage = 'Переход назад...';
    });

    try {
      await _ftpConnect.changeDirectory('..');
      _currentPath = await _ftpConnect.currentDirectory() ?? _currentPath;
      await _listFiles();
    } catch (e) {
      _showSnackBar('Ошибка перехода: ${e.toString()}', isError: true);
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _downloadFile(FTPEntry file) async {
    setState(() {
      _isLoading = true;
      _statusMessage = 'Скачивание ${file.name}...';
    });

    try {
      // Используем временную директорию для скачивания
      final directory = await getTemporaryDirectory();
      final downloadPath = '${directory.path}/FTP_Downloads';
      final downloadDir = Directory(downloadPath);

      if (!await downloadDir.exists()) {
        await downloadDir.create(recursive: true);
      }

      final localFile = File('$downloadPath/${file.name}');

      bool success = await _ftpConnect.downloadFileWithRetry(
        file.name,
        localFile,
        pRetryCount: 3,
      );

      if (success) {
        _showSnackBar('Файл скачан: ${localFile.path}', isError: false);
        setState(() {
          _statusMessage = 'Файл скачан успешно';
        });

        // Открываем файл сразу после скачивания
        _openFile(localFile.path);
      } else {
        _showSnackBar('Не удалось скачать файл', isError: true);
      }
    } catch (e) {
      _showSnackBar('Ошибка скачивания: ${e.toString()}', isError: true);
    } finally {
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _openFile(String filePath) async {
    try {
      final result = await OpenFile.open(filePath);
      if (result.type != ResultType.done) {
        _showSnackBar('Не удалось открыть файл: ${result.message}', isError: true);
      }
    } catch (e) {
      _showSnackBar('Не удалось открыть файл: $e', isError: true);
    }
  }

  Future<void> _uploadFile() async {
    try {
      FilePickerResult? result = await FilePicker.platform.pickFiles();

      if (result != null) {
        setState(() {
          _isLoading = true;
          _statusMessage = 'Загрузка файла...';
        });

        File file = File(result.files.single.path!);

        bool success = await _ftpConnect.uploadFileWithRetry(
          file,
          pRetryCount: 3,
        );

        if (success) {
          _showSnackBar('Файл загружен успешно', isError: false);
          await _listFiles();
        } else {
          _showSnackBar('Не удалось загрузить файл', isError: true);
        }

        setState(() {
          _isLoading = false;
        });
      }
    } catch (e) {
      _showSnackBar('Ошибка загрузки: ${e.toString()}', isError: true);
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<void> _createFolder() async {
    TextEditingController folderNameController = TextEditingController();

    bool? result = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Создать папку'),
        content: TextField(
          controller: folderNameController,
          decoration: InputDecoration(
            hintText: 'Введите название папки',
            border: OutlineInputBorder(),
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context, false),
            child: Text('Отмена'),
          ),
          TextButton(
            onPressed: () => Navigator.pop(context, true),
            child: Text('Создать'),
          ),
        ],
      ),
    );

    if (result == true && folderNameController.text.isNotEmpty) {
      setState(() {
        _isLoading = true;
        _statusMessage = 'Создание папки...';
      });

      try {
        await _ftpConnect.makeDirectory(folderNameController.text);
        _showSnackBar('Папка создана успешно', isError: false);
        await _listFiles();
      } catch (e) {
        _showSnackBar('Ошибка создания папки: ${e.toString()}', isError: true);
        setState(() {
          _isLoading = false;
        });
      }
    }
  }

  Future<void> _deleteFile(FTPEntry file) async {
    bool? confirm = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Удалить файл?'),
        content: Text('Вы уверены, что хотите удалить "${file.name}"?'),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context, false),
            child: Text('Отмена'),
          ),
          TextButton(
            onPressed: () => Navigator.pop(context, true),
            child: Text('Удалить', style: TextStyle(color: Colors.red)),
          ),
        ],
      ),
    );

    if (confirm == true) {
      setState(() {
        _isLoading = true;
        _statusMessage = 'Удаление ${file.name}...';
      });

      try {
        await _ftpConnect.deleteFile(file.name);
        _showSnackBar('Файл удален', isError: false);
        await _listFiles();
      } catch (e) {
        _showSnackBar('Ошибка удаления: ${e.toString()}', isError: true);
        setState(() {
          _isLoading = false;
        });
      }
    }
  }

  Future<void> _previewFile(FTPEntry file) async {
    // Для предпросмотра просто открываем файл
    setState(() {
      _isLoading = true;
      _statusMessage = 'Подготовка файла...';
    });

    try {
      final directory = await getTemporaryDirectory();
      final tempPath = '${directory.path}/temp_preview';
      final tempDir = Directory(tempPath);

      if (!await tempDir.exists()) {
        await tempDir.create(recursive: true);
      }

      final localFile = File('$tempPath/${file.name}');

      bool success = await _ftpConnect.downloadFileWithRetry(
        file.name,
        localFile,
        pRetryCount: 3,
      );

      setState(() {
        _isLoading = false;
      });

      if (success) {
        _openFile(localFile.path);
      } else {
        _showSnackBar('Не удалось загрузить файл для просмотра', isError: true);
      }
    } catch (e) {
      _showSnackBar('Ошибка: ${e.toString()}', isError: true);
      setState(() {
        _isLoading = false;
      });
    }
  }

  void _showSnackBar(String message, {bool isError = false}) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(message),
        backgroundColor: isError ? Colors.red : Colors.green,
        duration: Duration(seconds: 3),
      ),
    );
  }

  IconData _getFileIcon(FTPEntry entry) {
    final isDir = entry.name.endsWith('/') ||
        (entry.size == null || entry.size == 0) && !entry.name.contains('.');

    if (isDir) {
      return Icons.folder;
    }

    final ext = entry.name.split('.').last.toLowerCase();
    switch (ext) {
      case 'txt': return Icons.description;
      case 'pdf': return Icons.picture_as_pdf;
      case 'jpg': case 'jpeg': case 'png': case 'gif': return Icons.image;
      case 'zip': case 'rar': return Icons.folder_zip;
      default: return Icons.insert_drive_file;
    }
  }

  String _formatFileSize(int? size) {
    if (size == null) return 'N/A';
    if (size < 1024) return '$size B';
    if (size < 1024 * 1024) return '${(size / 1024).toStringAsFixed(1)} KB';
    return '${(size / (1024 * 1024)).toStringAsFixed(1)} MB';
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text('FTP Клиент', style: TextStyle(fontSize: 18)),
            Text(
              _currentPath,
              style: TextStyle(fontSize: 12, fontWeight: FontWeight.normal),
              overflow: TextOverflow.ellipsis,
            ),
          ],
        ),
        backgroundColor: Colors.blue,
        actions: [
          IconButton(
            icon: Icon(_isConnected ? Icons.cloud_done : Icons.cloud_off),
            onPressed: _isConnected ? _disconnect : _connectAndList,
            tooltip: _isConnected ? 'Отключиться' : 'Подключиться',
          ),
          IconButton(
            icon: Icon(Icons.refresh),
            onPressed: _isConnected && !_isLoading ? _listFiles : null,
            tooltip: 'Обновить',
          ),
          IconButton(
            icon: Icon(Icons.create_new_folder),
            onPressed: _isConnected && !_isLoading ? _createFolder : null,
            tooltip: 'Создать папку',
          ),
        ],
      ),
      body: Column(
        children: [
          // Статус-бар
          Container(
            width: double.infinity,
            padding: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
            color: Colors.blue[50],
            child: Row(
              children: [
                Icon(
                  _isConnected ? Icons.check_circle : Icons.error,
                  color: _isConnected ? Colors.green : Colors.grey,
                  size: 16,
                ),
                SizedBox(width: 8),
                Expanded(
                  child: Text(
                    _statusMessage,
                    style: TextStyle(fontSize: 12),
                    overflow: TextOverflow.ellipsis,
                  ),
                ),
              ],
            ),
          ),

          // Индикатор загрузки
          if (_isLoading)
            LinearProgressIndicator(
              backgroundColor: Colors.grey[300],
              valueColor: AlwaysStoppedAnimation<Color>(Colors.blue),
            ),

          // Кнопка "Назад"
          if (_currentPath != '/')
            ListTile(
              leading: Icon(Icons.arrow_upward, color: Colors.blue),
              title: Text('..', style: TextStyle(fontWeight: FontWeight.bold)),
              onTap: _isLoading ? null : _navigateUp,
            ),

          // Список файлов
          Expanded(
            child: _fileList.isEmpty
                ? Center(
              child: Text(
                _isConnected ? 'Папка пуста' : 'Не подключено',
                style: TextStyle(color: Colors.grey, fontSize: 16),
              ),
            )
                : ListView.builder(
              itemCount: _fileList.length,
              itemBuilder: (context, index) {
                final file = _fileList[index];
                final isDirectory = file.name.endsWith('/') ||
                    ((file.size == null || file.size == 0) && !file.name.contains('.'));

                return Card(
                  margin: EdgeInsets.symmetric(horizontal: 8, vertical: 4),
                  child: ListTile(
                    leading: Icon(
                      _getFileIcon(file),
                      color: isDirectory ? Colors.amber : Colors.blue,
                      size: 32,
                    ),
                    title: Text(
                      file.name,
                      style: TextStyle(
                        fontWeight: isDirectory
                            ? FontWeight.bold
                            : FontWeight.normal,
                      ),
                      overflow: TextOverflow.ellipsis,
                    ),
                    subtitle: isDirectory
                        ? Text('Папка')
                        : Text(_formatFileSize(file.size)),
                    trailing: isDirectory
                        ? Icon(Icons.chevron_right)
                        : PopupMenuButton<String>(
                      onSelected: (value) {
                        if (value == 'download') {
                          _downloadFile(file);
                        } else if (value == 'preview') {
                          _previewFile(file);
                        } else if (value == 'delete') {
                          _deleteFile(file);
                        }
                      },
                      itemBuilder: (context) => [
                        PopupMenuItem(
                          value: 'download',
                          child: Row(
                            children: [
                              Icon(Icons.download, size: 20),
                              SizedBox(width: 8),
                              Text('Скачать'),
                            ],
                          ),
                        ),
                        PopupMenuItem(
                          value: 'preview',
                          child: Row(
                            children: [
                              Icon(Icons.visibility, size: 20),
                              SizedBox(width: 8),
                              Text('Открыть'),
                            ],
                          ),
                        ),
                        PopupMenuItem(
                          value: 'delete',
                          child: Row(
                            children: [
                              Icon(Icons.delete, size: 20, color: Colors.red),
                              SizedBox(width: 8),
                              Text('Удалить', style: TextStyle(color: Colors.red)),
                            ],
                          ),
                        ),
                      ],
                    ),
                    onTap: isDirectory && !_isLoading
                        ? () => _navigateToDirectory(file.name)
                        : null,
                  ),
                );
              },
            ),
          ),
        ],
      ),
      floatingActionButton: _isConnected
          ? FloatingActionButton.extended(
        onPressed: _isLoading ? null : _uploadFile,
        icon: Icon(Icons.upload_file),
        label: Text('Загрузить файл'),
        backgroundColor: Colors.green,
      )
          : null,
    );
  }
}