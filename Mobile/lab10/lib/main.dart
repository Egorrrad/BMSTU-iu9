import 'package:flutter/cupertino.dart';
import 'package:flutter/services.dart';

void main() {
  runApp(const AoapApp());
}

class AoapApp extends StatelessWidget {
  const AoapApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const CupertinoApp(title: 'USB Communicator', home: AoapHomePage());
  }
}

class AoapHomePage extends StatefulWidget {
  const AoapHomePage({super.key});

  @override
  State<AoapHomePage> createState() => _AoapHomePageState();
}

class _AoapHomePageState extends State<AoapHomePage> {
  static const MethodChannel _platform = MethodChannel('aoap_channel');

  String _status = 'Disconnected';
  final List<String> _messages = [];
  final TextEditingController _controller = TextEditingController();

  @override
  void initState() {
    super.initState();

    _platform.setMethodCallHandler((call) async {
      try {
        switch (call.method) {
          case 'onMessage':
            final String text = call.arguments as String;
            setState(() {
              _messages.add('PC: $text');
            });
            break;
          case 'onStatus':
            final String text = call.arguments as String;
            setState(() {
              _status = text;
            });
            break;
          default:
            break;
        }
      } catch (e, st) {
        debugPrint('MethodChannel error: $e\n$st');
      }
    });
  }

  Future<void> _connect() async {
    try {
      await _platform.invokeMethod('connect');
    } on PlatformException catch (e) {
      setState(() {
        _status = 'Error: ${e.message}';
      });
    }
  }

  Future<void> _send() async {
    final text = _controller.text.trim();
    if (text.isEmpty) return;

    try {
      await _platform.invokeMethod('send', {'text': text});
      setState(() {
        _messages.add('Me: $text');
        _controller.clear();
      });
    } on PlatformException catch (e) {
      setState(() {
        _status = 'Send error: ${e.message}';
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: const CupertinoNavigationBar(
        middle: Text('USB Communicator'),
      ),
      child: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Column(
                children: [
                  Text(
                    'Connection status: $_status\n',
                    style: CupertinoTheme.of(context).textTheme.textStyle,
                  ),
                  CupertinoButton.filled(
                    onPressed: _connect,
                    child: const Text('Connect'),
                  ),
                  const SizedBox(height: 20),
                ],
              ),

              Expanded(
                child: Container(
                  width: double.infinity,
                  padding: const EdgeInsets.all(8.0),
                  decoration: BoxDecoration(
                    border: Border.all(color: CupertinoColors.systemGrey),
                    borderRadius: BorderRadius.circular(8.0),
                  ),
                  child: ListView.builder(
                    itemCount: _messages.length,
                    itemBuilder: (context, index) {
                      return Padding(
                        padding: const EdgeInsets.symmetric(vertical: 2.0),
                        child: Text(
                          _messages[index],
                          style: CupertinoTheme.of(context).textTheme.textStyle,
                        ),
                      );
                    },
                  ),
                ),
              ),

              const SizedBox(height: 20),

              Row(
                children: [
                  Expanded(
                    child: CupertinoTextField(
                      controller: _controller,
                      placeholder: 'Enter your message',
                      padding: const EdgeInsets.all(12.0),
                      clearButtonMode: OverlayVisibilityMode.editing,
                      onSubmitted: (_) => _send(),
                    ),
                  ),
                  const SizedBox(width: 10),
                  CupertinoButton.filled(
                    onPressed: _send,
                    child: const Text('Send'),
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }
}
