import 'dart:io';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as io;
import 'package:shelf_web_socket/shelf_web_socket.dart';
import 'dart:convert';

class ArithmeticServer {
  final Map<String, dynamic> _clients = {};
  var _clientCounter = 0;
  double _currentSliderValue = 50.0;
  final Map<String, double> _clientSliderValues = {};

  void start() async {
    final handler = webSocketHandler((webSocket) {
      final clientId = 'client_${++_clientCounter}';
      _clients[clientId] = webSocket;
      _clientSliderValues[clientId] = _currentSliderValue;

      print('🔌 Client $clientId connected');


      _sendToClient(webSocket, {
        'type': 'connection',
        'status': 'connected',
        'clientId': clientId,
        'message': 'Connected to Arithmetic Server'
      });


      _sendToClient(webSocket, {
        'type': 'slider_value',
        'value': _clientSliderValues[clientId]!
      });

      webSocket.stream.listen(
            (message) {
          _handleMessage(message, clientId, webSocket);
        },
        onDone: () {
          print('Client $clientId disconnected');
          _clients.remove(clientId);
          _clientSliderValues.remove(clientId);
        },
        onError: (error) {
          print('Error with client $clientId: $error');
          _clients.remove(clientId);
          _clientSliderValues.remove(clientId);
        },
      );
    });

    try {
      final server = await io.serve(handler, InternetAddress.anyIPv4, 8080);

      print('Arithmetic WebSocket Server Started!');
      print('Local:  ws://localhost:${server.port}');
      print('Supported operations: +, -, *, /');
      print('Message format: {"operation": "+", "a": 5, "b": 3}');
      print('Slider commands: get_slider_value, update_slider');
      print('Press Ctrl+C to stop the server');

    } catch (e) {
      print('Failed to start server: $e');
      exit(1);
    }
  }

  void _handleMessage(dynamic message, String clientId, dynamic webSocket) {
    try {
      final data = _parseMessage(message.toString());
      if (data == null) {
        _sendError(webSocket, 'Invalid message format');
        return;
      }

      final type = data['type']?.toString();


      if (type == 'get_slider_value') {
        _sendToClient(webSocket, {
          'type': 'slider_value',
          'value': _clientSliderValues[clientId] ?? _currentSliderValue
        });
        return;
      }

      if (type == 'update_slider') {
        final value = double.tryParse(data['value']?.toString() ?? '');
        if (value != null && value >= 0 && value <= 100) {
          _clientSliderValues[clientId] = value;
          _currentSliderValue = value;

          _sendToClient(webSocket, {
            'type': 'slider_updated',
            'value': value,
            'message': 'Slider value updated successfully'
          });

          print('🎚️  Client $clientId updated slider to: $value');


          _broadcastToAll({
            'type': 'slider_updated',
            'value': value,
            'message': 'Slider updated by $clientId',
            'clientId': clientId
          });
        } else {
          _sendError(webSocket, 'Invalid slider value. Must be between 0 and 100');
        }
        return;
      }


      final operation = data['operation']?.toString();
      final a = double.tryParse(data['a']?.toString() ?? '');
      final b = double.tryParse(data['b']?.toString() ?? '');

      if (a == null || b == null) {
        _sendError(webSocket, 'Invalid numbers provided');
        return;
      }

      if (operation == null || !['+', '-', '*', '/'].contains(operation)) {
        _sendError(webSocket, 'Invalid operation. Use +, -, *, or /');
        return;
      }


      final result = _calculate(operation, a, b);

      if (result == null) {
        _sendError(webSocket, 'Calculation error');
        return;
      }


      _sendToClient(webSocket, {
        'type': 'result',
        'operation': '$a $operation $b',
        'result': result,
        'a': a,
        'b': b,
        'operationSymbol': operation
      });

      print('Client $clientId: $a $operation $b = $result');

    } catch (e) {
      print('Error processing message from $clientId: $e');
      _sendError(webSocket, 'Server error: ${e.toString()}');
    }
  }

  double? _calculate(String operation, double a, double b) {
    try {
      switch (operation) {
        case '+':
          return a + b;
        case '-':
          return a - b;
        case '*':
          return a * b;
        case '/':
          if (b == 0) {
            throw Exception('Division by zero');
          }
          return a / b;
        default:
          return null;
      }
    } catch (e) {
      print('Calculation error: $e');
      return null;
    }
  }

  Map<String, dynamic>? _parseMessage(String message) {
    try {

      return json.decode(message);
    } catch (e) {

      try {
        if (!message.startsWith('{') || !message.endsWith('}')) {
          return null;
        }

        final content = message.substring(1, message.length - 1);
        final pairs = content.split(',');
        final result = <String, dynamic>{};

        for (final pair in pairs) {
          final keyValue = pair.split(':');
          if (keyValue.length == 2) {
            final key = keyValue[0].trim().replaceAll('"', '').replaceAll(' ', '');
            var value = keyValue[1].trim().replaceAll('"', '');


            final numValue = num.tryParse(value);
            if (numValue != null) {
              result[key] = numValue;
            } else if (value == 'true') {
              result[key] = true;
            } else if (value == 'false') {
              result[key] = false;
            } else {
              result[key] = value;
            }
          }
        }
        return result;
      } catch (e) {
        print('Parse error: $e');
        return null;
      }
    }
  }

  void _sendToClient(dynamic webSocket, Map<String, dynamic> data) {
    try {
      webSocket.sink.add(json.encode(data));
    } catch (e) {
      print('Error sending to client: $e');
    }
  }

  void _sendError(dynamic webSocket, String error) {
    _sendToClient(webSocket, {
      'type': 'error',
      'message': error
    });
  }

  void _broadcastToAll(Map<String, dynamic> data) {
    _clients.forEach((clientId, webSocket) {
      try {
        webSocket.sink.add(json.encode(data));
      } catch (e) {
        print('Error broadcasting to client $clientId: $e');
      }
    });
  }
}

void main() {
  final server = ArithmeticServer();
  server.start();
}