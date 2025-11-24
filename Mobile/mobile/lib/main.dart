import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';

import 'lab1.dart' as lab1;
import 'lab2.dart' as lab2;
import 'lab4.dart' as lab4;
import 'lab52.dart' as lab52;
import 'lab51.dart' as lab51;
import 'lab53.dart' as lab53;
import 'lab6.dart' as lab6;
import 'lab7.dart' as lab7;
import 'lab8.dart' as lab8;

import 'rk1.dart' as rk1;

import 'let3.dart' as let3;
import 'let4.dart' as let4;
import 'let5.dart' as let5;


void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const CupertinoApp(
      title: 'Лабораторные работы',
      theme: CupertinoThemeData(
        primaryColor: CupertinoColors.systemBlue,
        brightness: Brightness.light,
      ),
      home: MainScreen(),
    );
  }
}

class MainScreen extends StatefulWidget {
  const MainScreen({super.key});

  @override
  State<MainScreen> createState() => _MainScreenState();
}

class _MainScreenState extends State<MainScreen> {
  int _selectedLabIndex = 0;
  bool _isMenuVisible = false;

  // Список лабораторных работ
  final List<LabWork> _labWorks = [
    LabWork(
      id: 1,
      title: 'Лабораторная работа 1',
      description: 'Счетчик на Flutter',
    ),
    LabWork(
      id: 2,
      title: 'Лабораторная работа 2',
      description: 'Работа с HTTP',
    ),
    LabWork(
      id: 3,
      title: 'Лабораторная работа 4',
      description: 'Animation controller',
    ),
    LabWork(
      id: 4,
      title: 'Летучка 3',
      description: 'Интерфейс для базы данных',
    ),
    LabWork(
      id: 5,
      title: 'Летучка 4',
      description: 'Интерфейс для MQTT',
    ),
    LabWork(
      id: 6,
      title: 'Лабораторная работа 5.1',
      description: 'Интерфейс для WebSocket1',
    ),
    LabWork(
      id: 7,
      title: 'Лабораторная работа 5.2',
      description: 'Интерфейс для WebSocket2',
    ),
    LabWork(
      id: 8,
      title: 'Лабораторная работа 5.3',
      description: 'Интерфейс для WebSocket3',
    ),
    LabWork(
      id: 9,
      title: 'Лабораторная работа 6',
      description: 'Яндекс карты',
    ),
    LabWork(
      id: 10,
      title: 'РК1',
      description: 'Редактирование 3D моделей',
    ),
    LabWork(
      id: 11,
      title: 'Лабораторная работа 7',
      description: 'Захват объекта',
    ),
    LabWork(
      id: 12,
      title: 'Лабораторная работа 8',
      description: 'Протокол FTP',
    ),
    LabWork(
      id: 13,
      title: 'Летучка 5',
      description: 'Клиент SMTP',
    ),
  ];

  void _toggleMenu() {
    setState(() {
      _isMenuVisible = !_isMenuVisible;
    });
  }

  void _selectLab(int index) {
    setState(() {
      _selectedLabIndex = index;
      _isMenuVisible = false;
    });
  }

  // Функция для получения соответствующего виджета лабораторной работы
  Widget _getLabWidget(int index) {
    switch (index) {
      case 0:
        return MaterialApp(
          home: lab1.MyHomePage(title: 'Лабораторная работа 1'),
          debugShowCheckedModeBanner: false,
        );
      case 1:
        return MaterialApp(
          home: lab2.MyHomePage(title: 'Лабораторная работа 2'),
          debugShowCheckedModeBanner: false,
        );
      case 2:
        return MaterialApp(
          home: lab4.BoardWithCircle(),
          debugShowCheckedModeBanner: false,
        );
      case 3:
        return MaterialApp(
          home: let3.DatabaseStates(),
          debugShowCheckedModeBanner: false,
        );
      case 4:
        return MaterialApp(
          home: let4.MQTTService(),
          debugShowCheckedModeBanner: false,
        );
      case 5:
        return MaterialApp(
          home: lab51.ArithmeticScreen(),
          debugShowCheckedModeBanner: false,
        );
      case 6:
        return MaterialApp(
          home: lab52.ArithmeticScreen(),
          debugShowCheckedModeBanner: false,
        );
      case 7:
        return MaterialApp(
          home: lab53.MyApp(),
          debugShowCheckedModeBanner: false,
        );
        case 8:
         return lab6.YandexMapsWidget();

         
      case 9:
        return MaterialApp(
          home: rk1.SkullApp(),
          debugShowCheckedModeBanner: false,
        );

      case 10:
        return MaterialApp(
          home: lab7.MyApp(),
          debugShowCheckedModeBanner: false,
        );

      case 11:
        return MaterialApp(
          home: lab8.FTPClientScreen(),
          debugShowCheckedModeBanner: false,
        );
      case 12:
        return MaterialApp(
          home: let5.MyApp(),
          debugShowCheckedModeBanner: false,
        );
      default:
        return const Center(
          child: Text('Лабораторная работа не найдена'),
        );
    }
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        leading: CupertinoButton(
          padding: EdgeInsets.zero,
          child: const Icon(CupertinoIcons.sidebar_left),
          onPressed: _toggleMenu,
        ),
        middle: Text(_labWorks[_selectedLabIndex].title),
      ),
      child: Stack(
        children: [
          // Основной контент - отображаем выбранную лабораторную работу
          _getLabWidget(_selectedLabIndex),

          // Затемнение фона когда меню открыто
          if (_isMenuVisible)
            GestureDetector(
              onTap: _toggleMenu,
              child: Container(
                color: CupertinoColors.black.withOpacity(0.3),
              ),
            ),

          // Выдвижное меню
          AnimatedPositioned(
            duration: const Duration(milliseconds: 300),
            curve: Curves.easeInOut,
            left: _isMenuVisible ? 0 : -280,
            top: 0,
            bottom: 0,
            child: _buildSideMenu(),
          ),
        ],
      ),
    );
  }

  Widget _buildSideMenu() {
    return Container(
      width: 280,
      decoration: BoxDecoration(
        color: CupertinoColors.systemBackground,
        border: Border(
          right: BorderSide(
            color: CupertinoColors.systemGrey4,
            width: 0.5,
          ),
        ),
        boxShadow: [
          BoxShadow(
            color: CupertinoColors.black.withOpacity(0.1),
            blurRadius: 8,
            offset: const Offset(2, 0),
          ),
        ],
      ),
      child: Column(
        children: [
          // Заголовок меню
          Container(
            width: double.infinity,
            padding: const EdgeInsets.all(20),
            decoration: BoxDecoration(
              color: CupertinoColors.systemGrey6,
              border: Border(
                bottom: BorderSide(
                  color: CupertinoColors.systemGrey4,
                  width: 0.5,
                ),
              ),
            ),
            child: Text(
              'Выберите лабораторную работу',
              style: TextStyle(
                fontWeight: FontWeight.w600,
                color: CupertinoColors.label,
                fontSize: 16,
              ),
            ),
          ),

          // Список лабораторных работ
          Expanded(
            child: CupertinoScrollbar(
              child: ListView.builder(
                itemCount: _labWorks.length,
                itemBuilder: (context, index) {
                  final lab = _labWorks[index];
                  return _buildMenuItem(lab, index);
                },
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildMenuItem(LabWork lab, int index) {
    final isSelected = index == _selectedLabIndex;

    return CupertinoButton(
      borderRadius: BorderRadius.zero,
      padding: EdgeInsets.zero,
      onPressed: () => _selectLab(index),
      child: Container(
        decoration: BoxDecoration(
          color: isSelected
              ? CupertinoTheme.of(context).primaryColor.withOpacity(0.1)
              : CupertinoColors.systemBackground,
          border: Border(
            left: BorderSide(
              color: isSelected
                  ? CupertinoTheme.of(context).primaryColor
                  : CupertinoColors.systemBackground,
              width: 4.0,
            ),
          ),
        ),
        padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 16),
        child: Row(
          children: [
            Container(
              width: 36,
              height: 36,
              decoration: BoxDecoration(
                color: isSelected
                    ? CupertinoTheme.of(context).primaryColor
                    : CupertinoColors.systemGrey5,
                borderRadius: BorderRadius.circular(8),
              ),
              child: Center(
                child: Text(
                  '${lab.id}',
                  style: TextStyle(
                    color: isSelected
                        ? CupertinoColors.systemBackground
                        : CupertinoColors.systemGrey,
                    fontWeight: FontWeight.w600,
                  ),
                ),
              ),
            ),
            const SizedBox(width: 12),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    lab.title,
                    style: TextStyle(
                      fontWeight: FontWeight.w600,
                      color: isSelected
                          ? CupertinoTheme.of(context).primaryColor
                          : CupertinoColors.label,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Text(
                    lab.description,
                    style: TextStyle(
                      color: isSelected
                          ? CupertinoTheme.of(context).primaryColor.withOpacity(0.8)
                          : CupertinoColors.systemGrey,
                      fontSize: 13,
                    ),
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                  ),
                ],
              ),
            ),
            if (isSelected)
              const Icon(
                CupertinoIcons.checkmark_alt_circle_fill,
                color: CupertinoColors.systemGreen,
                size: 20,
              ),
          ],
        ),
      ),
    );
  }
}

class LabWork {
  final int id;
  final String title;
  final String description;

  LabWork({
    required this.id,
    required this.title,
    required this.description,
  });
}