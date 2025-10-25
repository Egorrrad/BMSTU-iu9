import 'package:flutter/material.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/io.dart';
import 'dart:convert';

class ArithmeticScreen extends StatefulWidget {
  const ArithmeticScreen({super.key});

  @override
  State<ArithmeticScreen> createState() => _ArithmeticScreenState();
}

class _ArithmeticScreenState extends State<ArithmeticScreen> {
  final TextEditingController _aController = TextEditingController();
  final TextEditingController _bController = TextEditingController();

  final String _serverUrl = 'ws://172.20.10.14:8080';

  final FocusNode _aFocusNode = FocusNode();
  final FocusNode _bFocusNode = FocusNode();

  WebSocketChannel? _channel;
  String _connectionStatus = 'Disconnected';
  String _lastResult = '';
  String _lastError = '';
  bool _isConnected = false;
  bool _isCalculating = false;

  double _sliderValue = 50.0;
  bool _isSliderLoading = false;

  @override
  void initState() {
    super.initState();
    _connectToServer();

    _setupFocusListeners();
  }

  void _setupFocusListeners() {
    _aFocusNode.addListener(() {});
    _bFocusNode.addListener(() {});
  }

  @override
  void dispose() {
    _channel?.sink.close();
    _aController.dispose();
    _bController.dispose();
    _aFocusNode.dispose();
    _bFocusNode.dispose();
    super.dispose();
  }

  void _closeKeyboard() {
    _aFocusNode.unfocus();
    _bFocusNode.unfocus();
  }

  void _handleBackgroundTap() {
    _closeKeyboard();
  }

  void _connectToServer() {
    try {
      setState(() {
        _connectionStatus = 'Connecting...';
        _isConnected = false;
        _lastError = '';
      });

      _channel = IOWebSocketChannel.connect(
        _serverUrl,
        protocols: ['http'],
      );

      _channel!.stream.listen(
            (message) {
          _handleServerMessage(message);
        },
        onDone: () {
          if (mounted) {
            setState(() {
              _connectionStatus = 'Disconnected';
              _isConnected = false;
            });
          }
        },
        onError: (error) {
          if (mounted) {
            setState(() {
              _connectionStatus = 'Connection Error';
              _isConnected = false;
              _lastError = 'Connection failed: $error';
            });
          }
        },
      );

      if (mounted) {
        setState(() {
          _connectionStatus = 'Connected';
          _isConnected = true;
        });
      }

    } catch (e) {
      if (mounted) {
        setState(() {
          _connectionStatus = 'Connection Failed';
          _isConnected = false;
          _lastError = 'Failed to connect: $e\n\nMake sure:\n• Server is running on Mac\n• iPhone is on same WiFi\n• Firewall allows port 8080';
        });
      }
    }
  }

  void _handleServerMessage(dynamic message) {
    print('Received: $message');

    try {
      final data = json.decode(message.toString());
      final type = data['type']?.toString() ?? '';

      switch (type) {
        case 'connection':
          final status = data['status']?.toString() ?? '';
          final connectionMessage = data['message']?.toString() ?? 'Connected successfully';
          if (mounted) {
            setState(() {
              _connectionStatus = 'Connected ($status)';
              _lastResult = connectionMessage;
              _isCalculating = false;
            });
          }

          // Запрашиваем значение ползунка при подключении
          _requestSliderValue();
          break;

        case 'result':
          final a = data['a']?.toString() ?? '';
          final b = data['b']?.toString() ?? '';
          final operation = data['operationSymbol']?.toString() ?? '';
          final result = data['result']?.toString() ?? '';

          if (mounted) {
            setState(() {
              _lastResult = '$a $operation $b = $result';
              _lastError = '';
              _isCalculating = false;
            });
          }
          break;

        case 'slider_value':
          final sliderValue = double.tryParse(data['value']?.toString() ?? '50.0') ?? 50.0;
          if (mounted) {
            setState(() {
              _sliderValue = sliderValue;
              _isSliderLoading = false;
            });
          }
          break;

        case 'slider_updated':
          final sliderValue = double.tryParse(data['value']?.toString() ?? '50.0') ?? 50.0;
          final message = data['message']?.toString() ?? 'Slider updated';
          if (mounted) {
            setState(() {
              _sliderValue = sliderValue;
              _isSliderLoading = false;
              _lastResult = '$message: $sliderValue';
            });
          }
          break;

        case 'error':
          final errorMessage = data['message']?.toString() ?? 'Unknown error';
          if (mounted) {
            setState(() {
              _lastError = errorMessage;
              _lastResult = '';
              _isCalculating = false;
              _isSliderLoading = false;
            });
          }
          break;

        default:
          print('Unknown message type: $type');
      }
    } catch (e) {
      print('Error parsing message: $e');
      if (mounted) {
        setState(() {
          _lastError = 'Error parsing server response: $e';
          _isCalculating = false;
          _isSliderLoading = false;
        });
      }
    }
  }

  // Запрос значения ползунка с сервера
  void _requestSliderValue() {
    if (!_isConnected || _channel == null) return;

    final message = json.encode({
      'type': 'get_slider_value',
    });

    print('Requesting slider value: $message');
    _channel!.sink.add(message);
  }

  // Отправка нового значения ползунка на сервер
  void _sendSliderValue(double value) {
    if (!_isConnected || _channel == null) return;

    setState(() {
      _isSliderLoading = true;
    });

    final message = json.encode({
      'type': 'update_slider',
      'value': value,
    });

    print('Sending slider value: $message');
    _channel!.sink.add(message);
  }

  void _sendCalculation(String operation) {
    if (!_isConnected || _channel == null) {
      _showError('Not connected to server');
      return;
    }

    final a = _aController.text.trim();
    final b = _bController.text.trim();

    if (a.isEmpty || b.isEmpty) {
      _showError('Please enter both numbers');
      return;
    }

    if (double.tryParse(a) == null || double.tryParse(b) == null) {
      _showError('Please enter valid numbers');
      return;
    }

    setState(() {
      _isCalculating = true;
      _lastError = '';
    });

    final message = json.encode({
      'operation': operation,
      'a': double.parse(a),
      'b': double.parse(b),
    });

    print('Sending: $message');

    try {
      _channel!.sink.add(message);
    } catch (e) {
      if (mounted) {
        setState(() {
          _isCalculating = false;
          _lastError = 'Failed to send message: $e';
        });
      }
    }
  }

  void _showError(String message) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(message),
        backgroundColor: Colors.red,
        duration: const Duration(seconds: 3),
      ),
    );
  }

  void _reconnect() {
    if (_channel != null) {
      _channel!.sink.close();
    }
    _connectToServer();
  }

  void _clearFields() {
    _aController.clear();
    _bController.clear();
    if (mounted) {
      setState(() {
        _lastResult = '';
        _lastError = '';
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return GestureDetector(
      onTap: _handleBackgroundTap,
      behavior: HitTestBehavior.opaque,
      child: Scaffold(
        appBar: AppBar(
          title: const Text('Arithmetic Calculator'),
          backgroundColor: Colors.blue,
          foregroundColor: Colors.white,
          actions: [
            IconButton(
              icon: const Icon(Icons.keyboard_hide),
              onPressed: _closeKeyboard,
              tooltip: 'Hide Keyboard',
            ),
            IconButton(
              icon: const Icon(Icons.refresh),
              onPressed: _reconnect,
              tooltip: 'Reconnect',
            ),
          ],
        ),
        body: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            children: [
              // Статус подключения
              Container(
                padding: const EdgeInsets.all(8),
                decoration: BoxDecoration(
                  color: _isConnected ? Colors.green[50] : Colors.red[50],
                  border: Border.all(
                    color: _isConnected ? Colors.green : Colors.red,
                  ),
                  borderRadius: BorderRadius.circular(8),
                ),
                child: Row(
                  children: [
                    Icon(
                      _isConnected ? Icons.check_circle : Icons.error,
                      color: _isConnected ? Colors.green : Colors.red,
                    ),
                    const SizedBox(width: 8),
                    Expanded(
                      child: Text(
                        _connectionStatus,
                        style: TextStyle(
                          color: _isConnected ? Colors.green : Colors.red,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ),
                  ],
                ),
              ),

              const SizedBox(height: 20),

              // Поля ввода чисел
              Row(
                children: [
                  Expanded(
                    child: TextField(
                      controller: _aController,
                      focusNode: _aFocusNode,
                      decoration: const InputDecoration(
                        labelText: 'Number A',
                        border: OutlineInputBorder(),
                        prefixIcon: Icon(Icons.numbers),
                      ),
                      keyboardType: TextInputType.number,
                      textInputAction: TextInputAction.next,
                      onSubmitted: (_) {
                        FocusScope.of(context).requestFocus(_bFocusNode);
                      },
                    ),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: TextField(
                      controller: _bController,
                      focusNode: _bFocusNode,
                      decoration: const InputDecoration(
                        labelText: 'Number B',
                        border: OutlineInputBorder(),
                        prefixIcon: Icon(Icons.numbers),
                      ),
                      keyboardType: TextInputType.number,
                      textInputAction: TextInputAction.done,
                      onSubmitted: (_) => _closeKeyboard(),
                    ),
                  ),
                ],
              ),

              const SizedBox(height: 20),

              // Кнопки операций
              Wrap(
                spacing: 12,
                runSpacing: 12,
                children: [
                  _buildOperationButton('+', Colors.green, 'Addition'),
                  _buildOperationButton('-', Colors.orange, 'Subtraction'),
                  _buildOperationButton('*', Colors.blue, 'Multiplication'),
                  _buildOperationButton('/', Colors.purple, 'Division'),
                ],
              ),

              const SizedBox(height: 20),

              if (_isCalculating) ...[
                const CircularProgressIndicator(),
                const SizedBox(height: 16),
                const Text('Calculating...'),
                const SizedBox(height: 20),
              ],

              // Результат
              if (_lastResult.isNotEmpty) ...[
                Container(
                  width: double.infinity,
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: Colors.green[50],
                    border: Border.all(color: Colors.green),
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      const Text(
                        'Result:',
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: Colors.green,
                        ),
                      ),
                      const SizedBox(height: 8),
                      Text(
                        _lastResult,
                        style: const TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 20),
              ],

              // Ошибка
              if (_lastError.isNotEmpty) ...[
                Container(
                  width: double.infinity,
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: Colors.red[50],
                    border: Border.all(color: Colors.red),
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      const Text(
                        'Error:',
                        style: TextStyle(
                          fontWeight: FontWeight.bold,
                          color: Colors.red,
                        ),
                      ),
                      const SizedBox(height: 8),
                      Text(
                        _lastError,
                        style: const TextStyle(fontSize: 12),
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 20),
              ],


              Card(
                elevation: 2,
                child: Padding(
                  padding: const EdgeInsets.all(16),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        children: [
                          const Icon(Icons.slideshow, color: Colors.purple),
                          const SizedBox(width: 8),
                          const Text(
                            'Slider Control',
                            style: TextStyle(
                              fontSize: 16,
                              fontWeight: FontWeight.bold,
                            ),
                          ),
                          const Spacer(),
                          if (_isSliderLoading)
                            const SizedBox(
                              width: 16,
                              height: 16,
                              child: CircularProgressIndicator(strokeWidth: 2),
                            ),
                        ],
                      ),
                      const SizedBox(height: 12),
                      Row(
                        children: [
                          Expanded(
                            child: Slider(
                              value: _sliderValue,
                              min: 0,
                              max: 100,
                              divisions: 100,
                              label: _sliderValue.round().toString(),
                              onChanged: _isConnected ? (double value) {
                                setState(() {
                                  _sliderValue = value;
                                });
                                // Отправляем значение при изменении
                                _sendSliderValue(value);
                              } : null,
                              activeColor: Colors.purple,
                              inactiveColor: Colors.purple.shade100,
                            ),
                          ),
                          const SizedBox(width: 12),
                          Container(
                            width: 60,
                            padding: const EdgeInsets.symmetric(
                              horizontal: 12,
                              vertical: 6,
                            ),
                            decoration: BoxDecoration(
                              color: Colors.purple.shade50,
                              borderRadius: BorderRadius.circular(8),
                              border: Border.all(color: Colors.purple.shade200),
                            ),
                            child: Text(
                              _sliderValue.toStringAsFixed(1),
                              textAlign: TextAlign.center,
                              style: const TextStyle(
                                fontWeight: FontWeight.bold,
                                fontSize: 16,
                              ),
                            ),
                          ),
                        ],
                      ),
                      const SizedBox(height: 8),
                      Text(
                        'Current value: ${_sliderValue.toStringAsFixed(1)}',
                        style: TextStyle(
                          color: Colors.grey[600],
                          fontSize: 12,
                        ),
                      ),
                    ],
                  ),
                ),
              ),

              const Spacer(),

              if (MediaQuery.of(context).viewInsets.bottom > 0) ...[
                OutlinedButton.icon(
                  icon: const Icon(Icons.keyboard_hide),
                  label: const Text('Hide Keyboard'),
                  onPressed: _closeKeyboard,
                ),
                const SizedBox(height: 20),
              ],
            ],
          ),
        ),
        floatingActionButton: Column(
          mainAxisAlignment: MainAxisAlignment.end,
          children: [
            if (MediaQuery.of(context).viewInsets.bottom > 0)
              FloatingActionButton(
                onPressed: _closeKeyboard,
                tooltip: 'Hide Keyboard',
                backgroundColor: Colors.grey,
                mini: true,
                child: const Icon(Icons.keyboard_hide),
              ),
            const SizedBox(height: 16),
            FloatingActionButton(
              onPressed: _reconnect,
              tooltip: 'Reconnect',
              child: const Icon(Icons.refresh),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildOperationButton(String operation, Color color, String tooltip) {
    return Tooltip(
      message: tooltip,
      child: ElevatedButton(
        onPressed: _isConnected ? () {
          _closeKeyboard();
          _sendCalculation(operation);
        } : null,
        style: ElevatedButton.styleFrom(
          backgroundColor: color,
          foregroundColor: Colors.white,
          minimumSize: const Size(70, 70),
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(12),
          ),
        ),
        child: Text(
          operation,
          style: const TextStyle(
            fontSize: 24,
            fontWeight: FontWeight.bold,
          ),
        ),
      ),
    );
  }
}