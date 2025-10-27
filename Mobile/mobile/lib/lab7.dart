import 'dart:math';

import 'package:ditredi/ditredi.dart';
import 'package:flutter/material.dart';
import 'package:vector_math/vector_math_64.dart' as vector;

class MyApp extends StatefulWidget {
  const MyApp({Key? key}) : super(key: key);

  @override
  State<MyApp> createState() => _MyAppState();
}

class _MyAppState extends State<MyApp> {
  var gripAngle = 0.0;

  // Позиция руки
  var handX = 0.0;
  var handY = 0.0;
  var handZ = 0.0;

  // Текущая позиция черепа
  var skullX = 0.0;
  var skullY = 0.0;
  var skullZ = 15.0;

  // Флаг захвата
  bool _isGrabbed = false;

  final Future<List<Mesh3D>> models = _generatePoints();

  final _controller = DiTreDiController(
    rotationX: 0,
    rotationY: 0,
    light: vector.Vector3(-0.5, -0.5, 0.5),
  );

  double get distanceToSkull {
    double dx = handX - skullX;
    double dy = handY - skullY;
    double dz = handZ - skullZ;
    return sqrt(dx * dx + dy * dy + dz * dz);
  }

  bool _isCollidingBox(vector.Vector3 hand, vector.Vector3 skull) {
    final handHalfSize = vector.Vector3(1.0, 1.0, 10.0);
    final skullHalfSize = vector.Vector3(4.0, 4.0, 4.0);

    final hMin = hand - handHalfSize;
    final hMax = hand + handHalfSize;

    final offset = vector.Vector3(5.0, 5.0, 0.0);
    final sMin = (skull + offset) - skullHalfSize;
    final sMax = (skull + offset) + skullHalfSize;

    final overlapX = hMax.x >= sMin.x && hMin.x <= sMax.x;
    final overlapY = hMax.y >= sMin.y && hMin.y <= sMax.y;
    final overlapZ = hMax.z >= sMin.z && hMin.z <= sMax.z;

    return overlapX && overlapY && overlapZ;
  }

  bool _canTake(vector.Vector3 hand, vector.Vector3 skull) {
    final handHalfSize = vector.Vector3(2.0, 2.0, 8.0);
    final skullHalfSize = vector.Vector3(8.0, 8.5, 8.5);

    final hMin = hand - handHalfSize;
    final hMax = hand + handHalfSize;

    // Границы черепа
    final sMin = skull - skullHalfSize;
    final sMax = skull + skullHalfSize;

    final overlapX = hMax.x >= sMin.x && hMin.x <= sMax.x;
    final overlapY = hMax.y >= sMin.y && hMin.y <= sMax.y;
    final overlapZ = hMax.z >= sMin.z && hMin.z <= sMax.z;

    return overlapX || overlapY || overlapZ;
  }

  void _updateHandPosition(double newX, double newY, double newZ) {
    if (_isGrabbed) {
      setState(() {
        handX = newX;
        handY = newY;
        handZ = newZ;
      });
      return;
    }

    // Проверяем коллизию для новой позиции
    final newHandPos = vector.Vector3(newX, newY, newZ);
    final skullPos = vector.Vector3(skullX, skullY, skullZ);

    if (_isCollidingBox(newHandPos, skullPos)) {
      return;
    }

    setState(() {
      handX = newX;
      handY = newY;
      handZ = newZ;
    });
  }

  @override
  Widget build(BuildContext context) {
    final skullPos = vector.Vector3(skullX, skullY, skullZ);
    final handPos = vector.Vector3(handX, handY, handZ);
    final isColliding = _isCollidingBox(handPos, skullPos);

    if (!_isGrabbed && gripAngle > 8 && _canTake(handPos, skullPos)) {
      setState(() {
        _isGrabbed = true;
        skullX = handX;
        skullY = handY;
        skullZ = handZ + 7.0;
      });
    } else if (_isGrabbed && gripAngle <= 8) {
      setState(() {
        _isGrabbed = false;
      });
    }

    return MaterialApp(
      darkTheme: ThemeData.dark(),
      title: 'DiTreDi Demo - Hand Grabbing Skull',
      theme: ThemeData(primarySwatch: Colors.blue),
      home: Scaffold(
        body: SafeArea(
          child: Flex(
            crossAxisAlignment: CrossAxisAlignment.start,
            direction: Axis.vertical,
            children: [
              FutureBuilder(
                future: models,
                builder:
                    (
                      BuildContext context,
                      AsyncSnapshot<List<Mesh3D>> snapshot,
                    ) {
                      List<Widget> children;
                      if (snapshot.hasData) {
                        double finalSkullX = _isGrabbed ? handX : skullX;
                        double finalSkullY = _isGrabbed ? handY : skullY;
                        double finalSkullZ = _isGrabbed ? handZ + 7.0 : skullZ;

                        if (_isGrabbed) {
                          skullX = finalSkullX;
                          skullY = finalSkullY;
                          skullZ = finalSkullZ;
                        }

                        children = <Widget>[
                          Expanded(
                            child: DiTreDiDraggable(
                              controller: _controller,
                              child: DiTreDi(
                                figures: [
                                  TransformModifier3D(
                                    snapshot.data![5],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(
                                        finalSkullX,
                                        finalSkullY,
                                        finalSkullZ,
                                      )
                                      ..scale(0.58, 0.58, 0.58),
                                  ),

                                  TransformModifier3D(
                                    snapshot.data![0],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(handX, handY, handZ),
                                  ),

                                  TransformModifier3D(
                                    snapshot.data![1],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(handX, handY, handZ)
                                      ..translate(3.05, 1.15, 8.75)
                                      ..translate(-0.2, -0.25, -2.2)
                                      ..rotateX(-(gripAngle * pi / 18))
                                      ..translate(0.2, 0.25, 2.2),
                                  ),

                                  TransformModifier3D(
                                    snapshot.data![2],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(handX, handY, handZ)
                                      ..translate(0.7, 0.0, 9.75)
                                      ..translate(0.0, -0.5, -2.25)
                                      ..rotateX(-(gripAngle * pi / 18))
                                      ..translate(0.0, 0.5, 2.25),
                                  ),

                                  TransformModifier3D(
                                    snapshot.data![3],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(handX, handY, handZ)
                                      ..translate(-2.0, -0.56, 9.1)
                                      ..translate(0.0, -0.25, -2.2)
                                      ..rotateX(-(gripAngle * pi / 18))
                                      ..translate(0.0, 0.25, 2.2),
                                  ),

                                  TransformModifier3D(
                                    snapshot.data![4],
                                    Matrix4.identity()
                                      ..rotateX(-pi / 2)
                                      ..translate(handX, handY, handZ)
                                      ..translate(-4.65, -1.0, 7.15)
                                      ..translate(0.0, 0.0, -1.25)
                                      ..rotateX(-(gripAngle * pi / 18))
                                      ..translate(0.0, 0.0, 1.25),
                                  ),
                                ],
                                controller: _controller,
                              ),
                            ),
                          ),
                          Padding(
                            padding: const EdgeInsets.all(8.0),
                            child: Column(
                              children: [
                                const Text("Drag to rotate. Scroll to zoom"),
                                Text(
                                  _isGrabbed
                                      ? "🦴 Skull is GRABBED! 🦴"
                                      : _canTake(handPos, skullPos)
                                      ? "Close grip to grab!"
                                      : "Move hand closer to skull",
                                  style: TextStyle(
                                    fontWeight: FontWeight.bold,
                                    color: _isGrabbed
                                        ? Colors.green
                                        : _canTake(handPos, skullPos)
                                        ? Colors.yellow
                                        : Colors.orange,
                                    fontSize: 14,
                                  ),
                                ),

                                if (isColliding && !_isGrabbed)
                                  Text(
                                    "COLLISION DETECTED!",
                                    style: TextStyle(
                                      fontWeight: FontWeight.bold,
                                      color: Colors.red,
                                      fontSize: 12,
                                    ),
                                  ),
                              ],
                            ),
                          ),
                          Expanded(
                            child: SingleChildScrollView(
                              child: Column(
                                mainAxisSize: MainAxisSize.min,
                                children: [
                                  const Padding(
                                    padding: EdgeInsets.all(8.0),
                                    child: Text(
                                      "Hand Grip",
                                      style: TextStyle(
                                        fontWeight: FontWeight.bold,
                                        fontSize: 16,
                                      ),
                                    ),
                                  ),
                                  Padding(
                                    padding: const EdgeInsets.symmetric(
                                      horizontal: 16.0,
                                    ),
                                    child: Slider(
                                      value: gripAngle,
                                      min: 0,
                                      max: 12,
                                      divisions: 13,
                                      label: gripAngle.round().toString(),
                                      onChanged: (double value) {
                                        setState(() {
                                          gripAngle = value;
                                        });
                                      },
                                    ),
                                  ),
                                  const Divider(thickness: 2),
                                  const Padding(
                                    padding: EdgeInsets.all(8.0),
                                    child: Text(
                                      "Hand Position",
                                      style: TextStyle(
                                        fontWeight: FontWeight.bold,
                                        fontSize: 16,
                                      ),
                                    ),
                                  ),
                                  Padding(
                                    padding: const EdgeInsets.symmetric(
                                      horizontal: 16.0,
                                    ),
                                    child: Row(
                                      children: [
                                        const SizedBox(
                                          width: 30,
                                          child: Text(
                                            "X:",
                                            textAlign: TextAlign.right,
                                          ),
                                        ),
                                        Expanded(
                                          child: Slider(
                                            value: handX,
                                            min: -20,
                                            max: 20,
                                            onChanged: (value) {
                                              _updateHandPosition(
                                                value,
                                                handY,
                                                handZ,
                                              );
                                            },
                                          ),
                                        ),
                                        SizedBox(
                                          width: 50,
                                          child: Text(handX.toStringAsFixed(1)),
                                        ),
                                      ],
                                    ),
                                  ),
                                  Padding(
                                    padding: const EdgeInsets.symmetric(
                                      horizontal: 16.0,
                                    ),
                                    child: Row(
                                      children: [
                                        const SizedBox(
                                          width: 30,
                                          child: Text(
                                            "Y:",
                                            textAlign: TextAlign.right,
                                          ),
                                        ),
                                        Expanded(
                                          child: Slider(
                                            value: handY,
                                            min: -20,
                                            max: 20,
                                            onChanged: (value) {
                                              _updateHandPosition(
                                                handX,
                                                value,
                                                handZ,
                                              );
                                            },
                                          ),
                                        ),
                                        SizedBox(
                                          width: 50,
                                          child: Text(handY.toStringAsFixed(1)),
                                        ),
                                      ],
                                    ),
                                  ),
                                  Padding(
                                    padding: const EdgeInsets.symmetric(
                                      horizontal: 16.0,
                                    ),
                                    child: Row(
                                      children: [
                                        const SizedBox(
                                          width: 30,
                                          child: Text(
                                            "Z:",
                                            textAlign: TextAlign.right,
                                          ),
                                        ),
                                        Expanded(
                                          child: Slider(
                                            value: handZ,
                                            min: -20,
                                            max: 20,
                                            onChanged: (value) {
                                              _updateHandPosition(
                                                handX,
                                                handY,
                                                value,
                                              );
                                            },
                                          ),
                                        ),
                                        SizedBox(
                                          width: 50,
                                          child: Text(handZ.toStringAsFixed(1)),
                                        ),
                                      ],
                                    ),
                                  ),
                                ],
                              ),
                            ),
                          ),
                        ];
                      } else {
                        children = <Widget>[
                          const Padding(
                            padding: EdgeInsets.all(8.0),
                            child: Text("Loading models..."),
                          ),
                          const CircularProgressIndicator(),
                        ];
                      }
                      return Expanded(
                        child: Column(
                          mainAxisAlignment: MainAxisAlignment.center,
                          children: children,
                        ),
                      );
                    },
              ),
            ],
          ),
        ),
      ),
    );
  }
}

Future<List<Mesh3D>> _generatePoints() async {
  return [
    // Рука
    Mesh3D(await ObjParser().loadFromResources("assets/hand/hand.obj")),
    Mesh3D(await ObjParser().loadFromResources("assets/hand/index.obj")),
    Mesh3D(await ObjParser().loadFromResources("assets/hand/middle.obj")),
    Mesh3D(await ObjParser().loadFromResources("assets/hand/ring.obj")),
    Mesh3D(await ObjParser().loadFromResources("assets/hand/pinky.obj")),
    // Череп
    Mesh3D(
      await ObjParser().loadFromResources("assets/skull/12140_Skull_v3_L2.obj"),
    ),
  ];
}
