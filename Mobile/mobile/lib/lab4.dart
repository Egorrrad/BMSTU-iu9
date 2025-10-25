import 'dart:math' as Math;

import 'package:flutter/material.dart';

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Доска с кругом',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: BoardWithCircle(),
    );
  }
}

class BoardWithCircle extends StatefulWidget {
  @override
  _BoardWithCircleState createState() => _BoardWithCircleState();
}

class _BoardWithCircleState extends State<BoardWithCircle> {
  int _n = 10; // Размер доски n x n
  int _m = 20; // Размер квадрата в пикселях
  int _d = 4;  // Диаметр круга в квадратах
  Color _selectedColor = Colors.red;

  final List<Color> _availableColors = [
    Colors.red,
    Colors.green,
    Colors.blue,
    Colors.purple,
    Colors.orange,
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Доска с кругом'),
      ),
      body: SafeArea(
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: <Widget>[
            Expanded(
              child: CustomPaint(
                painter: BoardPainter(_n, _m, _d, _selectedColor),
                child: Container(),
              ),
            ),
            Padding(
              padding: const EdgeInsets.only(left: 16.0, top: 8.0),
              child: Text('Размер доски (n x n): ${_n}'),
            ),
            Slider(
              value: _n.toDouble(),
              min: 5.0,
              max: 30.0,
              label: _n.toString(),
              divisions: 25,
              onChanged: (value) {
                setState(() {
                  _n = value.toInt();
                });
              },
            ),
            Padding(
              padding: const EdgeInsets.only(left: 16.0),
              child: Text('Размер квадрата (m): ${_m}px'),
            ),
            Slider(
              value: _m.toDouble(),
              min: 10.0,
              max: 50.0,
              label: _m.toString(),
              divisions: 40,
              onChanged: (value) {
                setState(() {
                  _m = value.toInt();
                });
              },
            ),
            Padding(
              padding: const EdgeInsets.only(left: 16.0),
              child: Text('Диаметр круга (d): ${_d}'),
            ),
            Slider(
              value: _d.toDouble(),
              min: 1.0,
              max: _n.toDouble(),
              label: _d.toString(),
              divisions: _n - 1,
              onChanged: (value) {
                setState(() {
                  _d = value.toInt();
                });
              },
            ),
            Padding(
              padding: const EdgeInsets.only(left: 16.0),
              child: Text('Цвет заливки:'),
            ),
            SizedBox(
              height: 60,
              child: ListView.builder(
                scrollDirection: Axis.horizontal,
                itemCount: _availableColors.length,
                itemBuilder: (context, index) {
                  return GestureDetector(
                    onTap: () {
                      setState(() {
                        _selectedColor = _availableColors[index];
                      });
                    },
                    child: Container(
                      width: 40,
                      height: 40,
                      margin: EdgeInsets.all(8),
                      decoration: BoxDecoration(
                        color: _availableColors[index],
                        border: Border.all(
                          color: _selectedColor == _availableColors[index]
                              ? Colors.black
                              : Colors.grey,
                          width: 3,
                        ),
                        borderRadius: BorderRadius.circular(20),
                      ),
                    ),
                  );
                },
              ),
            ),
            SizedBox(height: 16),
          ],
        ),
      ),
    );
  }
}

class BoardPainter extends CustomPainter {
  final int n;
  final int m;
  final int d;
  final Color fillColor;

  BoardPainter(this.n, this.m, this.d, this.fillColor);

  @override
  void paint(Canvas canvas, Size size) {
    final paint = Paint()
      ..color = Colors.black
      ..strokeWidth = 1
      ..style = PaintingStyle.stroke;

    final fillPaint = Paint()
      ..color = fillColor
      ..style = PaintingStyle.fill;

    // Центр доски
    double centerX = size.width / 2;
    double centerY = size.height / 2;

    // Общий размер доски
    double boardSize = n * m.toDouble();

    // Начальные координаты для отрисовки (центрирование)
    double startX = centerX - boardSize / 2;
    double startY = centerY - boardSize / 2;

    // Радиус круга в квадратах
    double radius = d / 2;

    // Центр круга в координатах квадратов
    double circleCenterX = n / 2;
    double circleCenterY = n / 2;

    // Рисуем сетку и закрашиваем квадраты, попадающие в круг
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        double x = startX + i * m;
        double y = startY + j * m;

        // Проверяем, попадает ли квадрат в круг
        double distance = Math.sqrt(
            Math.pow(i - circleCenterX + 0.5, 2) +
                Math.pow(j - circleCenterY + 0.5, 2)
        );

        // Если расстояние от центра квадрата до центра круга меньше радиуса
        if (distance <= radius) {
          // Закрашиваем квадрат
          canvas.drawRect(
            Rect.fromLTWH(x, y, m.toDouble(), m.toDouble()),
            fillPaint,
          );
        }

        // Рисуем контур квадрата
        canvas.drawRect(
          Rect.fromLTWH(x, y, m.toDouble(), m.toDouble()),
          paint,
        );
      }
    }
  }

  @override
  bool shouldRepaint(CustomPainter oldDelegate) {
    return true;
  }
}