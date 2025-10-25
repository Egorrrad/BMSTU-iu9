import 'package:flutter/material.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/io.dart';
import 'dart:convert';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter WebSocket Demo',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
      ),
      home: const MyHomePage(title: 'Flutter WebSocket Counter'),
    );
  }
}

class MyHomePage extends StatefulWidget {
  const MyHomePage({super.key, required this.title});

  final String title;

  @override
  State<MyHomePage> createState() => _MyHomePageState();
}

class _MyHomePageState extends State<MyHomePage> {
  int _counter = 0;
  bool _serviceEnabled = true;
  bool _isLoading = false;
  bool _isConnected = false;
  String _lastUpdate = '';
  String _connectionStatus = 'Disconnected';
  Color _connectionColor = Colors.red;

  late WebSocketChannel _channel;
  final String _websocketUrl = 'ws://88.218.122.244:8091';

  @override
  void initState() {
    super.initState();
    _connectToWebSocket();
  }

  void _connectToWebSocket() {
    setState(() {
      _isLoading = true;
      _connectionStatus = 'Connecting...';
      _connectionColor = Colors.orange;
    });

    try {
      _channel = IOWebSocketChannel.connect(_websocketUrl);

      _channel.stream.listen(
            (message) {
          _handleWebSocketMessage(message);
        },
        onDone: () {
          _handleWebSocketDisconnection();
        },
        onError: (error) {
          _handleWebSocketError(error);
        },
      );

    } catch (error) {
      _handleWebSocketError(error);
    }
  }

  void _handleWebSocketMessage(dynamic message) {
    print("📨 Received: $message");

    try {
      final jsonResponse = json.decode(message);
      final String type = jsonResponse['type'];

      switch (type) {
        case 'connection':
          setState(() {
            _isConnected = true;
            _isLoading = false;
            _connectionStatus = 'Connected';
            _connectionColor = Colors.green;
          });
          break;

        case 'service_status':
          setState(() {
            _serviceEnabled = jsonResponse['enabled'];
            _isLoading = false;
          });
          break;

        case 'counter_value':
          setState(() {
            _counter = jsonResponse['value'];
            _lastUpdate = jsonResponse['last_update'] ?? '';
            _isLoading = false;
          });
          break;

        case 'error':
          setState(() {
            _isLoading = false;
          });
          _showErrorSnackBar(jsonResponse['message']);
          break;

        case 'success':
          setState(() {
            _isLoading = false;
          });
          _showSuccessSnackBar(jsonResponse['message']);
          break;
      }
    } catch (error) {
      print("Error parsing WebSocket message: $error");
    }
  }

  void _handleWebSocketDisconnection() {
    setState(() {
      _isConnected = false;
      _connectionStatus = 'Disconnected';
      _connectionColor = Colors.red;
    });
    _showErrorSnackBar('Disconnected from server');
  }

  void _handleWebSocketError(dynamic error) {
    setState(() {
      _isConnected = false;
      _isLoading = false;
      _connectionStatus = 'Connection Failed';
      _connectionColor = Colors.red;
    });
    print("WebSocket error: $error");
    _showErrorSnackBar('Connection error: $error');
  }

  void _sendWebSocketMessage(Map<String, dynamic> message) {
    if (!_isConnected) {
      _showErrorSnackBar('Not connected to server');
      return;
    }

    try {
      final jsonMessage = json.encode(message);
      _channel.sink.add(jsonMessage);
      print("📤 Sent: $jsonMessage");
    } catch (error) {
      print("Error sending WebSocket message: $error");
      _showErrorSnackBar('Error sending message');
    }
  }

  void _getServiceStatus() {
    _sendWebSocketMessage({
      'command': 'get_status'
    });
  }

  void _getCounterValue() {
    setState(() {
      _isLoading = true;
    });
    _sendWebSocketMessage({
      'command': 'get_counter'
    });
  }

  void _incrementCounter() {
    if (!_serviceEnabled || !_isConnected) return;

    _sendWebSocketMessage({
      'command': 'increment'
    });
  }

  void _decrementCounter() {
    if (!_serviceEnabled || !_isConnected) return;

    _sendWebSocketMessage({
      'command': 'decrement'
    });
  }

  void _resetCounter() {
    if (!_serviceEnabled || !_isConnected) return;

    _sendWebSocketMessage({
      'command': 'reset'
    });
  }

  void _enableService() {
    _sendWebSocketMessage({
      'command': 'enable_service'
    });
  }

  void _disableService() {
    _sendWebSocketMessage({
      'command': 'disable_service'
    });
  }

  void _reconnect() {
    _connectToWebSocket();
  }

  void _showErrorSnackBar(String message) {
    ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text(message),
          backgroundColor: Colors.red,
        )
    );
  }

  void _showSuccessSnackBar(String message) {
    ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text(message),
          backgroundColor: Colors.green,
        )
    );
  }

  @override
  void dispose() {
    _channel.sink.close();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        title: Text(widget.title),
        actions: [
          IconButton(
            icon: const Icon(Icons.refresh),
            onPressed: _reconnect,
            tooltip: 'Reconnect',
          ),
        ],
      ),
      body: Center(
        child: _isLoading
            ? const Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            CircularProgressIndicator(),
            SizedBox(height: 20),
            Text('Loading...'),
          ],
        )
            : Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            // Connection Status
            Container(
              padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
              decoration: BoxDecoration(
                color: _connectionColor.withOpacity(0.1),
                borderRadius: BorderRadius.circular(20),
                border: Border.all(color: _connectionColor),
              ),
              child: Row(
                mainAxisSize: MainAxisSize.min,
                children: [
                  Icon(
                    _isConnected ? Icons.wifi : Icons.wifi_off,
                    color: _connectionColor,
                    size: 16,
                  ),
                  const SizedBox(width: 8),
                  Text(
                    _connectionStatus,
                    style: TextStyle(
                      color: _connectionColor,
                      fontWeight: FontWeight.bold,
                    ),
                  ),
                ],
              ),
            ),

            const SizedBox(height: 20),

            // Service Status
            Text(
              'Service Status:',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            Text(
              _serviceEnabled ? 'ENABLED' : 'DISABLED',
              style: TextStyle(
                color: _serviceEnabled ? Colors.green : Colors.red,
                fontWeight: FontWeight.bold,
                fontSize: 20,
              ),
            ),

            const SizedBox(height: 30),

            // Counter Value
            Text(
              'Counter Value:',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            Text(
              '$_counter',
              style: const TextStyle(
                fontSize: 40,
                fontWeight: FontWeight.bold,
              ),
            ),

            if (_lastUpdate.isNotEmpty) ...[
              const SizedBox(height: 10),
              Text(
                'Last update: $_lastUpdate',
                style: TextStyle(
                  color: Colors.grey[600],
                  fontSize: 12,
                ),
              ),
            ],

            const SizedBox(height: 30),

            // Counter Control
            Text(
              'Counter Control:',
              style: Theme.of(context).textTheme.titleMedium,
            ),

            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.red,
                    foregroundColor: Colors.white,
                    minimumSize: const Size(60, 60),
                    shape: const CircleBorder(),
                  ),
                  onPressed: _serviceEnabled && _isConnected ? _decrementCounter : null,
                  child: const Icon(Icons.remove, size: 30),
                ),

                const SizedBox(width: 20),

                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.green,
                    foregroundColor: Colors.white,
                    minimumSize: const Size(60, 60),
                    shape: const CircleBorder(),
                  ),
                  onPressed: _serviceEnabled && _isConnected ? _incrementCounter : null,
                  child: const Icon(Icons.add, size: 30),
                ),
              ],
            ),

            const SizedBox(height: 20),

            // Service Control
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.blue,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _isConnected ? _enableService : null,
                  child: const Text('TURN ON SERVICE'),
                ),

                const SizedBox(width: 10),

                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.grey,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _isConnected ? _disableService : null,
                  child: const Text('TURN OFF SERVICE'),
                ),
              ],
            ),

            const SizedBox(height: 20),

            // Reset Counter
            ElevatedButton(
              style: ElevatedButton.styleFrom(
                backgroundColor: Colors.orange,
                foregroundColor: Colors.white,
                minimumSize: const Size(200, 50),
              ),
              onPressed: _serviceEnabled && _isConnected ? _resetCounter : null,
              child: const Text('RESET COUNTER TO 0'),
            ),

            const SizedBox(height: 10),

            // Range info
            Text(
              'Range: -100 to 100',
              style: TextStyle(
                color: Colors.grey[600],
                fontStyle: FontStyle.italic,
              ),
            ),

            const SizedBox(height: 20),

            // Refresh buttons
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.purple,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _isConnected ? _getCounterValue : null,
                  child: const Text('REFRESH COUNTER'),
                ),

                const SizedBox(width: 10),

                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.purple,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _isConnected ? _getServiceStatus : null,
                  child: const Text('REFRESH STATUS'),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }
}