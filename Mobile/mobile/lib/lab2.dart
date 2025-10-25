import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Flutter Demo',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.deepPurple),
      ),
      home: const MyHomePage(title: 'Flutter Demo Home Page'),
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
  String _lastUpdate = '';

  @override
  void initState() {
    super.initState();
    _checkServiceStatus();
  }

  void _checkServiceStatus() async {
    try {
      final uri = Uri.parse('http://iocontrol.ru/api/sendData/DiachkovLab2/button');
      final response = await http.get(uri);
      if (response.statusCode == 200) {
        setState(() {
          _serviceEnabled = response.body == "1";
        });

        if (_serviceEnabled) {
          _getCounterValueFromServer();
        }
      }
    } catch (error) {
      print("Error checking service status: $error");
    }
  }

  void _getCounterValueFromServer() async {
    setState(() {
      _isLoading = true;
    });

    try {
      final uri = Uri.parse('http://iocontrol.ru/api/readData/DiachkovLab2/barCounter');
      final response = await http.get(uri);

      if (response.statusCode == 200) {
        final jsonResponse = json.decode(response.body);
        final bool check = jsonResponse['check'] ?? false;

        if (check) {
          final counterValue = int.tryParse(jsonResponse['value'] ?? '0') ?? 0;
          final updateTime = jsonResponse['date'] ?? '';

          setState(() {
            _counter = counterValue;
            _lastUpdate = updateTime;
            _isLoading = false;
          });

          print("Counter value received from server: $counterValue");
          print("Last update: $updateTime");
        } else {
          setState(() {
            _isLoading = false;
          });
          print("Server returned check: false");
        }
      } else {
        setState(() {
          _isLoading = false;
        });
        print("Failed to get counter value. Status code: ${response.statusCode}");
      }
    } catch (error) {
      setState(() {
        _isLoading = false;
      });
      print("Error getting counter value: $error");
    }
  }

  void _incrementCounter() {
    if (!_serviceEnabled) return;

    setState(() {
      _counter++;
    });
    _updateCounterOnServer(_counter);
  }

  void _decrementCounter() {
    if (!_serviceEnabled) return;

    setState(() {
      _counter--;
    });
    _updateCounterOnServer(_counter);
  }

  void _updateCounterOnServer(int value) async {
    try {
      final uri = Uri.parse('http://iocontrol.ru/api/sendData/DiachkovLab2/barCounter/$value');
      final response = await http.get(uri);
      if (response.statusCode == 200) {
        print("Counter updated successfully: $value");
        _getCounterValueFromServer();
      }
    } catch (error) {
      print("Error updating counter: $error");
    }
  }

  void _resetCounter() {
    if (!_serviceEnabled) return;

    setState(() {
      _counter = 0;
    });
    _updateCounterOnServer(0);
  }

  void _getBitovkaLampRequestON() {
    setState(() {
      _isLoading = true;
    });

    final uri = Uri.parse('http://iocontrol.ru/api/sendData/DiachkovLab2/button/1');
    http.get(uri).then((response) {
      print("Response status: ${response.statusCode}");
      print("Response body: ${response.body}");

      _getCounterValueFromServer();

      setState(() {
        _serviceEnabled = true;
      });
    }).catchError((error){
      setState(() {
        _isLoading = false;
      });
      print("Error turning on service: $error");
    });
  }

  void _getBitovkaLampRequestOFF() {
    setState(() {
      _isLoading = true;
    });

    final uri = Uri.parse('http://iocontrol.ru/api/sendData/DiachkovLab2/button/0');
    http.get(uri).then((response) {
      print("Response status: ${response.statusCode}");
      print("Response body: ${response.body}");
      setState(() {
        _serviceEnabled = false;
        _isLoading = false;
      });
    }).catchError((error){
      setState(() {
        _isLoading = false;
      });
      print("Error turning off service: $error");
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        backgroundColor: Theme.of(context).colorScheme.inversePrimary,
        title: Text(widget.title),
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
                  onPressed: _serviceEnabled ? _decrementCounter : null,
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
                  onPressed: _serviceEnabled ? _incrementCounter : null,
                  child: const Icon(Icons.add, size: 30),
                ),
              ],
            ),

            const SizedBox(height: 20),
            Row(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.blue,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _getBitovkaLampRequestON,
                  child: const Text('TURN ON SERVICE'),
                ),

                const SizedBox(width: 10),

                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    backgroundColor: Colors.grey,
                    foregroundColor: Colors.white,
                  ),
                  onPressed: _getBitovkaLampRequestOFF,
                  child: const Text('TURN OFF SERVICE'),
                ),
              ],
            ),

            const SizedBox(height: 20),
            ElevatedButton(
              style: ElevatedButton.styleFrom(
                backgroundColor: Colors.orange,
                foregroundColor: Colors.white,
                minimumSize: const Size(200, 50),
              ),
              onPressed: _serviceEnabled ? _resetCounter : null,
              child: const Text('RESET COUNTER TO 0'),
            ),

            const SizedBox(height: 10),
            Text(
              'Range: -100 to 100',
              style: TextStyle(
                color: Colors.grey[600],
                fontStyle: FontStyle.italic,
              ),
            ),

          ],
        ),
      ),
    );
  }
}