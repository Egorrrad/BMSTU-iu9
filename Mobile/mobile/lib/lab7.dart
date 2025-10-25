import 'dart:async';
import 'dart:math' as math;

import 'package:flutter/material.dart';
import 'package:flutter_cube/flutter_cube.dart';

void main() => runApp(const SkullApp());

class SkullApp extends StatelessWidget {
  const SkullApp({super.key});

  static const String _title = 'Flutter Code Sample';

  @override
  Widget build(BuildContext context) {
    return const MaterialApp(title: _title, home: MyStatefulWidget());
  }
}

class MyStatefulWidget extends StatefulWidget {
  const MyStatefulWidget({super.key});

  @override
  State<MyStatefulWidget> createState() => _MyStatefulWidgetState();
}

class _MyStatefulWidgetState extends State<MyStatefulWidget>
    with SingleTickerProviderStateMixin {
  Scene? _scene;
  Object? _mainSkull;
  Object? _jaw;
  Object? _lowerTeeth;
  Object? _upperTeeth;
  Object? _skullBase;
  List<Object> _additionalSkullParts = [];
  Map<Object, Vector3> _partDirections = {};
  String _statusMessage = 'Загрузка модели...';
  double _separationAngle = 0.0;
  bool _modelLoaded = false;
  double _explodeAmount = 2.0;
  late AnimationController _animationController;
  late Animation<double> _animation;

  @override
  void initState() {
    super.initState();
    _animationController = AnimationController(
      duration: const Duration(seconds: 2),
      vsync: this,
    );
    _animation =
        Tween<double>(begin: 0.0, end: 30.0).animate(_animationController)
          ..addListener(() {
            setState(() {
              _separationAngle = _animation.value;
            });
          });
  }

  @override
  void dispose() {
    _animationController.dispose();
    super.dispose();
  }

  Object? _findChild(RegExp regex) {
    for (var child in _mainSkull!.children) {
      if (child.name != null && regex.hasMatch(child.name!)) {
        return child;
      }
    }
    return null;
  }

  Vector3 _computeCentroid(Mesh? mesh) {
    if (mesh == null || mesh.vertices.isEmpty) {
      return Vector3(0, 0, 0);
    }
    Vector3 sum = Vector3(0, 0, 0);
    for (var vertex in mesh.vertices) {
      sum += vertex;
    }
    return sum / mesh.vertices.length.toDouble();
  }

  Future<void> _loadModel(Scene scene) async {
    print('Начало загрузки модели...');
    _scene = scene;
    try {
      _mainSkull = Object(fileName: 'assets/skull/myskull1.obj');
      _mainSkull!.scale.setValues(10.0, 10.0, 10.0);
      _mainSkull!.position.setValues(0, 0, 0);
      _mainSkull!.rotation.setValues(-90, 90, 0);
      scene.world.add(_mainSkull!);

      print('Модель загружена');

      // Увеличиваем задержку для полной загрузки
      Future.delayed(Duration(seconds: 5), () {
        if (mounted && _mainSkull != null && _mainSkull!.children.isNotEmpty) {
          _jaw = _findChild(RegExp(r'jaw', caseSensitive: false));

          _lowerTeeth = _findChild(
            RegExp(r'teeth_lower', caseSensitive: false),
          );

          _upperTeeth = _findChild(RegExp(r'teeth$', caseSensitive: false));

          _skullBase = _findChild(RegExp(r'skull_v3$', caseSensitive: false));

          _additionalSkullParts = _mainSkull!.children
              .where(
                (child) =>
                    child.name != null &&
                    RegExp(
                      r'12140_Skull_v3\.\d+',
                      caseSensitive: false,
                    ).hasMatch(child.name!),
              )
              .toList();

          // Вычисляем центроиды и направления для дополнительных частей
          for (var part in _additionalSkullParts) {
            Vector3 centroid = _computeCentroid(part.mesh);
            Vector3 direction = centroid.normalized();
            if (direction.length == 0) {
              math.Random rand = math.Random(part.name.hashCode);
              direction = Vector3(
                rand.nextDouble() * 2 - 1,
                rand.nextDouble() * 2 - 1,
                rand.nextDouble() * 2 - 1,
              ).normalized();
            }
            _partDirections[part] = direction;
          }

          setState(() {
            _modelLoaded = true;
            print('Дочерних объектов: ${_mainSkull?.children.length}');
            print('Названия объектов:');
            _mainSkull?.children.forEach((child) {
              print('  - ${child.name ?? "NULL"}');
            });

            if (_jaw != null) {
              print('Челюсть найдена: ${_jaw?.name}');
            } else {
              print('ВНИМАНИЕ: Челюсть не найдена!');
              _statusMessage = 'Челюсть не найдена в модели';
            }

            if (_lowerTeeth != null) {
              print('Нижние зубы найдены: ${_lowerTeeth?.name}');
            } else {
              print('ВНИМАНИЕ: Нижние зубы не найдены!');
              _statusMessage += '\nНижние зубы не найдены';
            }

            if (_upperTeeth != null) {
              print('Верхние зубы найдены: ${_upperTeeth?.name}');
            } else {
              print('ВНИМАНИЕ: Верхние зубы не найдены!');
              _statusMessage += '\nВерхние зубы не найдены';
            }

            if (_skullBase != null) {
              print('Основная часть найдена: ${_skullBase?.name}');
            } else {
              print('ВНИМАНИЕ: Основная часть не найдена!');
              _statusMessage += '\nОсновная часть не найдена';
            }

            print(
              'Найдено дополнительных частей: ${_additionalSkullParts.length}',
            );
            _additionalSkullParts.forEach((part) {
              print('  - Дополнительная часть: ${part.name}');
            });

            bool allMainPartsFound =
                _jaw != null &&
                _lowerTeeth != null &&
                _upperTeeth != null &&
                _skullBase != null;
            if (allMainPartsFound) {
              _statusMessage =
                  'Модель загружена! Используйте ползунок для разделения частей';
            } else {
              _statusMessage = 'Некоторые части не найдены. Проверьте консоль.';
            }
          });
        } else {
          print(
            'Модель ещё не загружена полностью. Дети: ${_mainSkull?.children.length}',
          );
          setState(() {
            _statusMessage =
                'Модель не загружена полностью. Попробуйте перезапустить.';
          });
        }
      });
    } catch (e) {
      print('Ошибка загрузки: $e');
      setState(() {
        _statusMessage = 'Ошибка: Не удалось загрузить модель';
      });
    }
  }

  void _updateSeparationAnimation() {
    if (_scene == null || _mainSkull == null) return;

    double factor = _separationAngle / 30.0;

    _scene!.camera.position.z = 1.2 - factor * 2.5;
    _scene!.update();

    if (_skullBase != null) {
      _skullBase!.position.y = factor * 0.8;
      _skullBase!.position.z = -factor * 1.2;
      _skullBase!.updateTransform();
    }

    for (var part in _additionalSkullParts) {
      Vector3 direction = _partDirections[part]!;
      part.position.x = direction.x * factor * _explodeAmount;
      part.position.y = direction.y * factor * _explodeAmount;
      part.position.z = direction.z * factor * _explodeAmount;
      part.updateTransform();
    }

    if (_upperTeeth != null) {
      _upperTeeth!.position.y = factor * 1.2;
      _upperTeeth!.position.z = factor * 0.8;
      _upperTeeth!.updateTransform();
    }

    if (_jaw != null) {
      _jaw!.position.y = -factor * 1.5;
      _jaw!.position.z = factor * 0.3;
      _jaw!.rotation.x = _separationAngle;
      _jaw!.updateTransform();
    }

    if (_lowerTeeth != null) {
      _lowerTeeth!.position.y = -factor * 1.3;
      _lowerTeeth!.position.z = factor * 0.6;
      _lowerTeeth!.rotation.x = _separationAngle;
      _lowerTeeth!.updateTransform();
    }

    _mainSkull!.updateTransform();
    _scene!.update();
  }

  void _explode() {
    if (_animationController.isAnimating) {
      _animationController.stop();
    } else {
      _animationController.forward(from: 0.0);
      setState(() {
        _statusMessage = 'Взрыв...';
      });
    }
  }

  void _zoomCamera(double delta) {
    if (_scene != null) {
      _scene!.camera.position.z += delta;
      if (_scene!.camera.position.z < -10) _scene!.camera.position.z = -5;
      if (_scene!.camera.position.z > 50.0) _scene!.camera.position.z = 50.0;
      setState(() {});
    }
  }

  void _resetCamera() {
    if (_scene != null) {
      _scene!.camera.position.setValues(5, 5, 5);
      _scene!.camera.target.setValues(0, 0, 0);
      setState(() {});
    }
  }

  void _resetSeparation() {
    _animationController.reset();
    setState(() {
      _separationAngle = 0.0;
      _statusMessage = _modelLoaded ? 'Череп собран' : _statusMessage;
    });

    if (_skullBase != null) {
      _skullBase!.position.setValues(0, 0, 0);
      _skullBase!.updateTransform();
    }
    for (var part in _additionalSkullParts) {
      part.position.setValues(0, 0, 0);
      part.updateTransform();
    }
    if (_upperTeeth != null) {
      _upperTeeth!.position.setValues(0, 0, 0);
      _upperTeeth!.updateTransform();
    }
    if (_jaw != null) {
      _jaw!.position.setValues(0, 0, 0);
      _jaw!.rotation.setValues(0, 0, 0);
      _jaw!.updateTransform();
    }
    if (_lowerTeeth != null) {
      _lowerTeeth!.position.setValues(0, 0, 0);
      _lowerTeeth!.rotation.setValues(0, 0, 0);
      _lowerTeeth!.updateTransform();
    }
    if (_mainSkull != null && _scene != null) {
      _mainSkull!.updateTransform();
      _scene!.update();
    }
    _resetCamera();
  }

  @override
  Widget build(BuildContext context) {
    _updateSeparationAnimation();

    return Scaffold(
      appBar: AppBar(
        title: const Text('Разделение частей'),
        centerTitle: true,
        actions: [
          IconButton(
            icon: const Icon(Icons.center_focus_strong),
            tooltip: 'Сбросить камеру',
            onPressed: _resetCamera,
          ),
        ],
      ),
      body: Stack(
        children: [
          Container(
            color: Colors.black,
            child: Cube(
              onSceneCreated: (Scene scene) {
                scene.camera.position.setValues(5, 5, 5);
                scene.camera.target.setValues(0, 0, 0);
                scene.light.position.setValues(10, 10, 10);
                _loadModel(scene);
              },
              interactive: true,
            ),
          ),
          if (!_modelLoaded) const Center(child: CircularProgressIndicator()),
          Column(
            mainAxisAlignment: MainAxisAlignment.end,
            children: [
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 20),
                child: Column(
                  children: [
                    Text(
                      'Разделение: ${(_separationAngle / 30 * 100).toInt()}%',
                      style: const TextStyle(color: Colors.white, fontSize: 16),
                    ),
                    Slider(
                      value: _separationAngle,
                      min: 0.0,
                      max: 30.0,
                      divisions: 30,
                      label: '${(_separationAngle / 30 * 100).toInt()}%',
                      activeColor: Colors.red,
                      inactiveColor: Colors.grey,
                      onChanged: (value) {
                        setState(() {
                          _separationAngle = value;
                          if (value == 0.0) {
                            _statusMessage = _modelLoaded
                                ? 'Череп собран'
                                : _statusMessage;
                          } else if (value < 15.0) {
                            _statusMessage = 'Разделение...';
                          } else {
                            _statusMessage = 'Части разделены';
                          }
                        });
                      },
                    ),
                  ],
                ),
              ),
              Padding(
                padding: const EdgeInsets.all(16.0),
                child: Text(
                  _statusMessage,
                  style: const TextStyle(color: Colors.white, fontSize: 16),
                  textAlign: TextAlign.center,
                ),
              ),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  FloatingActionButton(
                    heroTag: 'explode',
                    mini: true,
                    backgroundColor: Colors.orange,
                    child: const Icon(Icons.whatshot),
                    onPressed: _explode,
                    tooltip: 'Взорвать',
                  ),
                  const SizedBox(width: 10),
                  FloatingActionButton(
                    heroTag: 'reset',
                    mini: true,
                    backgroundColor: Colors.green,
                    child: const Icon(Icons.replay),
                    onPressed: _resetSeparation,
                  ),
                  const SizedBox(width: 10),
                  FloatingActionButton(
                    heroTag: 'zoom_in',
                    mini: true,
                    child: const Icon(Icons.zoom_in),
                    onPressed: () => _zoomCamera(-0.3),
                  ),
                  const SizedBox(width: 10),
                  FloatingActionButton(
                    heroTag: 'zoom_out',
                    mini: true,
                    child: const Icon(Icons.zoom_out),
                    onPressed: () => _zoomCamera(0.3),
                  ),
                ],
              ),
              const SizedBox(height: 20),
            ],
          ),
        ],
      ),
    );
  }
}
