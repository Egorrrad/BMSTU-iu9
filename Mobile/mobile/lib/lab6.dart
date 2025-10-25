import 'package:flutter/cupertino.dart';
import 'package:yandex_mapkit/yandex_mapkit.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'dart:typed_data';
import 'dart:ui' as ui;

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return CupertinoApp(
      title: 'Яндекс.Карты - Объекты',
      theme: CupertinoThemeData(
        primaryColor: CupertinoColors.systemRed,
      ),
      home: YandexMapsWidget(),
    );
  }
}

class YandexMapsWidget extends StatefulWidget {
  @override
  _YandexMapsWidgetState createState() => _YandexMapsWidgetState();
}

class _YandexMapsWidgetState extends State<YandexMapsWidget> {
  YandexMapController? _mapController;
  List<Place> _places = [];
  bool _isLoading = true;

  @override
  void initState() {
    super.initState();
    _loadPlaces();
  }

  Future<void> _loadPlaces() async {
    try {
      final response = await http.get(Uri.parse(
          'http://pstgu.yss.su/iu9/mobiledev/lab4_yandex_map/2023.php?x=var14'));

      if (response.statusCode == 200) {
        final List<dynamic> jsonData = json.decode(response.body);
        setState(() {
          _places = jsonData.map((item) => Place.fromJson(item)).toList();
          _isLoading = false;
        });

        if (_places.isNotEmpty) {
          _moveToFirstPlace();
        }
      }
    } catch (e) {
      print('Ошибка загрузки данных: $e');
      setState(() {
        _isLoading = false;
      });
    }
  }

  Future<Uint8List> _createCustomMarker() async {
    final pictureRecorder = ui.PictureRecorder();
    final canvas = ui.Canvas(pictureRecorder);
    final size = ui.Size(80, 80);


    final paint = ui.Paint()
      ..color = CupertinoColors.systemRed
      ..style = ui.PaintingStyle.fill;


    canvas.drawCircle(ui.Offset(40, 40), 25, paint);


    final borderPaint = ui.Paint()
      ..color = CupertinoColors.white
      ..style = ui.PaintingStyle.stroke
      ..strokeWidth = 4;

    canvas.drawCircle(ui.Offset(40, 40), 25, borderPaint);


    final iconPaint = ui.Paint()..color = CupertinoColors.white;


    canvas.drawCircle(ui.Offset(40, 40), 8, iconPaint);


    final path = ui.Path();
    path.moveTo(40, 15);
    path.lineTo(30, 35);
    path.lineTo(50, 35);
    path.close();
    canvas.drawPath(path, iconPaint);

    final image = await pictureRecorder.endRecording().toImage(80, 80);
    final byteData = await image.toByteData(format: ui.ImageByteFormat.png);

    return byteData!.buffer.asUint8List();
  }

  Future<List<PlacemarkMapObject>> _createPlacemarks() async {
    final markerBytes = await _createCustomMarker();

    return _places.asMap().entries.map((entry) {
      final index = entry.key;
      final place = entry.value;

      return PlacemarkMapObject(
        mapId: MapObjectId('place_$index'),
        point: Point(
          latitude: place.latitude,
          longitude: place.longitude,
        ),
        onTap: (self, point) {
          _showPlaceDetails(place);
        },
        icon: PlacemarkIcon.single(
          PlacemarkIconStyle(
            image: BitmapDescriptor.fromBytes(markerBytes),
            scale: 1.2,
          ),
        ),
      );
    }).toList();
  }

  void _moveToFirstPlace() {
    if (_places.isNotEmpty && _mapController != null) {
      final firstPlace = _places.first;
      _mapController!.moveCamera(
        CameraUpdate.newCameraPosition(
          CameraPosition(
            target: Point(
              latitude: firstPlace.latitude,
              longitude: firstPlace.longitude,
            ),
            zoom: 12.0,
          ),
        ),
        animation: const MapAnimation(duration: 1.0),
      );
    }
  }

  void _showPlaceDetails(Place place) {
    showCupertinoModalPopup(
      context: context,
      builder: (context) => Container(
        margin: const EdgeInsets.all(20),
        decoration: BoxDecoration(
          color: CupertinoColors.systemBackground,
          borderRadius: BorderRadius.circular(14),
          boxShadow: [
            BoxShadow(
              color: CupertinoColors.black.withOpacity(0.2),
              blurRadius: 10,
              offset: const Offset(0, 5),
            ),
          ],
        ),
        child: CupertinoActionSheet(
          title: Text(
            place.name,
            style: TextStyle(
              fontWeight: FontWeight.bold,
              fontSize: 18,
            ),
          ),
          message: Container(
            padding: const EdgeInsets.symmetric(vertical: 8),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                _buildInfoRow(CupertinoIcons.location_solid, 'Адрес:', place.address),
                const SizedBox(height: 12),
                _buildInfoRow(CupertinoIcons.phone_solid, 'Телефон:', place.phone),
                const SizedBox(height: 12),
                _buildCoordinatesRow(place),
              ],
            ),
          ),
          actions: [
            CupertinoActionSheetAction(
              onPressed: () {
                Navigator.of(context).pop();
              },
              child: Text(
                'Закрыть',
                style: TextStyle(
                  color: CupertinoColors.systemRed,
                  fontWeight: FontWeight.w600,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildInfoRow(IconData icon, String label, String value) {
    return Row(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Icon(
          icon,
          size: 20,
          color: CupertinoColors.systemGrey,
        ),
        const SizedBox(width: 12),
        Expanded(
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                label,
                style: TextStyle(
                  fontWeight: FontWeight.w600,
                  color: CupertinoColors.secondaryLabel,
                  fontSize: 14,
                ),
              ),
              const SizedBox(height: 4),
              Text(
                value,
                style: TextStyle(
                  fontSize: 16,
                  color: CupertinoColors.label,
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildCoordinatesRow(Place place) {
    return Row(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Icon(
          CupertinoIcons.compass,
          size: 20,
          color: CupertinoColors.systemGrey,
        ),
        const SizedBox(width: 12),
        Expanded(
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                'Координаты:',
                style: TextStyle(
                  fontWeight: FontWeight.w600,
                  color: CupertinoColors.secondaryLabel,
                  fontSize: 14,
                ),
              ),
              const SizedBox(height: 4),
              Text(
                '${place.latitude.toStringAsFixed(6)}, ${place.longitude.toStringAsFixed(6)}',
                style: TextStyle(
                  fontSize: 14,
                  color: CupertinoColors.systemGrey,
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        backgroundColor: CupertinoColors.systemRed,
        middle: Text(
          'Яндекс.Карты - Объекты',
          style: TextStyle(
            color: CupertinoColors.white,
            fontWeight: FontWeight.w600,
          ),
        ),
      ),
      child: Stack(
        children: [
          FutureBuilder<List<PlacemarkMapObject>>(
            future: _createPlacemarks(),
            builder: (context, snapshot) {
              if (snapshot.hasData) {
                return YandexMap(
                  mapObjects: snapshot.data!,
                  onMapCreated: (controller) {
                    _mapController = controller;
                    if (_places.isNotEmpty) {
                      WidgetsBinding.instance.addPostFrameCallback((_) {
                        _moveToFirstPlace();
                      });
                    }
                  },
                );
              } else {
                return YandexMap(
                  onMapCreated: (controller) {
                    _mapController = controller;
                  },
                );
              }
            },
          ),
          if (_isLoading)
            Center(
              child: Column(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  CupertinoActivityIndicator(
                    radius: 20,
                  ),
                  const SizedBox(height: 16),
                  Text(
                    'Загрузка карты...',
                    style: TextStyle(
                      color: CupertinoColors.secondaryLabel,
                      fontSize: 16,
                    ),
                  ),
                ],
              ),
            ),
          Positioned(
            bottom: 20,
            left: 20,
            right: 20,
            child: Container(
              padding: const EdgeInsets.all(16),
              decoration: BoxDecoration(
                color: CupertinoColors.systemBackground,
                borderRadius: BorderRadius.circular(12),
                boxShadow: [
                  BoxShadow(
                    color: CupertinoColors.black.withOpacity(0.15),
                    blurRadius: 8,
                    offset: const Offset(0, 4),
                  ),
                ],
              ),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text(
                    'Найдено объектов:',
                    style: TextStyle(
                      fontSize: 16,
                      fontWeight: FontWeight.w500,
                      color: CupertinoColors.secondaryLabel,
                    ),
                  ),
                  Container(
                    padding: const EdgeInsets.symmetric(
                      horizontal: 12,
                      vertical: 6,
                    ),
                    decoration: BoxDecoration(
                      color: CupertinoColors.systemRed,
                      borderRadius: BorderRadius.circular(20),
                    ),
                    child: Text(
                      '${_places.length}',
                      style: TextStyle(
                        fontSize: 16,
                        fontWeight: FontWeight.w600,
                        color: CupertinoColors.white,
                      ),
                    ),
                  ),
                ],
              ),
            ),
          ),
        ],
      ),
    );
  }
}

class Place {
  final String name;
  final double latitude;
  final double longitude;
  final String address;
  final String phone;

  Place({
    required this.name,
    required this.latitude,
    required this.longitude,
    required this.address,
    required this.phone,
  });

  factory Place.fromJson(Map<String, dynamic> json) {
    final gpsParts = (json['gps'] as String).split(',');
    return Place(
      name: json['name'],
      latitude: double.parse(gpsParts[0].trim()),
      longitude: double.parse(gpsParts[1].trim()),
      address: json['address'],
      phone: json['tel'],
    );
  }
}