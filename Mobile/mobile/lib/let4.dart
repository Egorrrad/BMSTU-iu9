import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:mqtt_client/mqtt_client.dart';
import 'package:mqtt_client/mqtt_server_client.dart';

class MQTTService extends StatefulWidget {
  const MQTTService({super.key});

  @override
  State<MQTTService> createState() => _MQTTServiceState();
}

class _MQTTServiceState extends State<MQTTService> {
  int _currentScreen = 0; // 0 - подключение, 1 - чат

  // Данные подключения
  String _broker = '';
  String _port = '';
  String _topic = '';
  String _name = '';

  // Данные сообщений
  String _message = '';
  final List<String> _messages = [];
  final TextEditingController _messageController = TextEditingController();

  // MQTT клиент
  MqttServerClient? _client;
  bool _isConnected = false;
  bool _isLoading = false;

  @override
  void dispose() {
    _client?.disconnect();
    _messageController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        middle: Text(_getAppBarTitle()),
        trailing: _currentScreen == 1
            ? CupertinoButton(
          padding: EdgeInsets.zero,
          child: const Icon(CupertinoIcons.arrow_left_circle_fill),
          onPressed: _goBack,
        )
            : null,
      ),
      child: SafeArea(
        child: Stack(
          children: [
            _buildCurrentScreen(),
            if (_isLoading)
              Container(
                color: CupertinoColors.black.withOpacity(0.3),
                child: const Center(
                  child: CupertinoActivityIndicator(radius: 16),
                ),
              ),
          ],
        ),
      ),
    );
  }

  String _getAppBarTitle() {
    switch (_currentScreen) {
      case 0: return 'MQTT Подключение';
      case 1: return 'MQTT Чат ($_name)';
      default: return 'MQTT Сервис';
    }
  }

  Widget _buildCurrentScreen() {
    switch (_currentScreen) {
      case 0: return _buildConnectionScreen();
      case 1: return _buildChatScreen();
      default: return _buildConnectionScreen();
    }
  }

  Widget _buildConnectionScreen() {
    return Padding(
      padding: const EdgeInsets.all(16.0),
      child: Column(
        children: [
          _buildCupertinoTextField('MQTT Broker', _broker, (value) => _broker = value,
              placeholder: 'test.mosquitto.org'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Port', _port, (value) => _port = value,
              keyboardType: TextInputType.number, placeholder: '1883'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Topic', _topic, (value) => _topic = value,
              placeholder: 'flutter/mqtt'),
          const SizedBox(height: 12),
          _buildCupertinoTextField('Name', _name, (value) => _name = value,
              placeholder: 'Ваше имя'),
          const SizedBox(height: 24),
          CupertinoButton.filled(
            child: const Text('Подключиться'),
            onPressed: _handleConnect,
          ),
        ],
      ),
    );
  }

  Widget _buildChatScreen() {
    return Column(
      children: [
        // Поле ввода сообщения
        Container(
          padding: const EdgeInsets.all(16),
          decoration: BoxDecoration(
            color: CupertinoColors.systemGrey6,
            border: Border(
              bottom: BorderSide(color: CupertinoColors.systemGrey4),
            ),
          ),
          child: Row(
            children: [
              Expanded(
                child: CupertinoTextField(
                  controller: _messageController,
                  onChanged: (value) => _message = value,
                  placeholder: 'Введите сообщение...',
                  padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                  decoration: BoxDecoration(
                    color: CupertinoColors.systemBackground,
                    border: Border.all(color: CupertinoColors.systemGrey4),
                    borderRadius: BorderRadius.circular(20),
                  ),
                ),
              ),
              const SizedBox(width: 12),
              CupertinoButton(
                padding: const EdgeInsets.all(12),
                minSize: 20,
                child: const Icon(CupertinoIcons.paperplane_fill, size: 20),
                onPressed: _sendMessage,
              ),
            ],
          ),
        ),

        // Статус подключения
        Container(
          padding: const EdgeInsets.symmetric(vertical: 8, horizontal: 16),
          decoration: BoxDecoration(
            color: _isConnected ? CupertinoColors.systemGreen : CupertinoColors.systemRed,
          ),
          child: Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              Icon(
                _isConnected ? CupertinoIcons.checkmark_alt_circle : CupertinoIcons.xmark_circle,
                size: 16,
                color: CupertinoColors.white,
              ),
              const SizedBox(width: 8),
              Text(
                _isConnected ? 'Подключено' : 'Отключено',
                style: const TextStyle(
                  color: CupertinoColors.white,
                  fontWeight: FontWeight.w500,
                ),
              ),
            ],
          ),
        ),

        // Список сообщений
        Expanded(
          child: _messages.isEmpty
              ? Center(
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Icon(
                  CupertinoIcons.chat_bubble,
                  size: 64,
                  color: CupertinoColors.systemGrey3,
                ),
                const SizedBox(height: 16),
                Text(
                  'Сообщений пока нет',
                  style: CupertinoTheme.of(context).textTheme.textStyle.copyWith(
                    color: CupertinoColors.systemGrey,
                  ),
                ),
                const SizedBox(height: 8),
                Text(
                  'Отправьте первое сообщение',
                  style: CupertinoTheme.of(context).textTheme.textStyle.copyWith(
                    color: CupertinoColors.systemGrey2,
                    fontSize: 14,
                  ),
                ),
              ],
            ),
          )
              : CupertinoScrollbar(
            child: ListView.builder(
              reverse: true,
              itemCount: _messages.length,
              itemBuilder: (context, index) {
                final message = _messages.reversed.toList()[index];
                return _buildMessageItem(message);
              },
            ),
          ),
        ),

        // Кнопка отключения
        Padding(
          padding: const EdgeInsets.all(16.0),
          child: CupertinoButton(
            child: const Text('Отключиться'),
            onPressed: _disconnect,
          ),
        ),
      ],
    );
  }

  Widget _buildMessageItem(String message) {
    final isMyMessage = message.startsWith('$_name:');

    return Container(
      margin: const EdgeInsets.symmetric(vertical: 4, horizontal: 16),
      child: Row(
        mainAxisAlignment: isMyMessage ? MainAxisAlignment.end : MainAxisAlignment.start,
        children: [
          Container(
            constraints: BoxConstraints(
              maxWidth: MediaQuery.of(context).size.width * 0.7,
            ),
            padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
            decoration: BoxDecoration(
              color: isMyMessage
                  ? CupertinoTheme.of(context).primaryColor
                  : CupertinoColors.systemGrey5,
              borderRadius: BorderRadius.circular(16),
            ),
            child: Text(
              message,
              style: TextStyle(
                color: isMyMessage
                    ? CupertinoColors.white
                    : CupertinoColors.label,
                fontSize: 14,
              ),
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildCupertinoTextField(String label, String initialValue, Function(String) onChanged,
      {bool isPassword = false, TextInputType keyboardType = TextInputType.text, String placeholder = ''}) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Text(
          label,
          style: TextStyle(
            color: CupertinoColors.label.resolveFrom(context),
            fontSize: 14,
            fontWeight: FontWeight.w500,
          ),
        ),
        const SizedBox(height: 4),
        CupertinoTextField(
          obscureText: isPassword,
          keyboardType: keyboardType,
          onChanged: onChanged,
          placeholder: placeholder.isNotEmpty ? placeholder : 'Введите $label',
          controller: TextEditingController(text: initialValue),
          padding: const EdgeInsets.all(12),
          decoration: BoxDecoration(
            color: CupertinoColors.systemGrey6,
            border: Border.all(color: CupertinoColors.systemGrey4),
            borderRadius: BorderRadius.circular(8),
          ),
        ),
      ],
    );
  }

  Future<void> _handleConnect() async {
    if (_broker.isEmpty || _port.isEmpty || _topic.isEmpty || _name.isEmpty) {
      _showAlert('Ошибка', 'Заполните все поля');
      return;
    }

    setState(() => _isLoading = true);

    try {
      final port = int.tryParse(_port) ?? 1883;

      _client = MqttServerClient(_broker, '');
      _client!.port = port;
      _client!.logging(on: false);

      // Устанавливаем callback'и
      _client!.onConnected = _onConnected;
      _client!.onDisconnected = _onDisconnected;
      _client!.onSubscribed = _onSubscribed;

      final connMessage = MqttConnectMessage()
          .withClientIdentifier('flutter_client_${DateTime.now().millisecondsSinceEpoch}')
          .startClean();

      _client!.connectionMessage = connMessage;

      await _client!.connect();

    } catch (e) {
      _showAlert('Ошибка подключения', 'Не удалось подключиться: $e');
      setState(() => _isLoading = false);
    }
  }

  void _onConnected() {
    setState(() {
      _isConnected = true;
      _isLoading = false;
    });

    // Подписываемся на топик
    _client!.subscribe(_topic, MqttQos.atMostOnce);

    // Слушаем входящие сообщения
    _client!.updates!.listen((List<MqttReceivedMessage<MqttMessage>> messages) {
      for (final message in messages) {
        final payload = message.payload as MqttPublishMessage;
        final decodedMessage = MqttPublishPayload.bytesToStringAsString(payload.payload.message);

        setState(() {
          _messages.add(decodedMessage);
        });
      }
    });

    _showAlert('Успех', 'Подключение к MQTT брокеру установлено');
    setState(() => _currentScreen = 1);
  }

  void _onDisconnected() {
    setState(() {
      _isConnected = false;
    });
  }

  void _onSubscribed(String topic) {
    print('Подписан на топик: $topic');
  }

  void _sendMessage() {
    if (_message.isEmpty || _client == null || !_isConnected) {
      _showAlert('Ошибка', 'Нет подключения или сообщение пустое');
      return;
    }

    try {
      final fullMessage = '$_name: $_message';
      final builder = MqttClientPayloadBuilder();
      builder.addString(fullMessage);

      _client!.publishMessage(_topic, MqttQos.atMostOnce, builder.payload!);

      setState(() {
        _message = '';
        _messageController.clear();
      });

    } catch (e) {
      _showAlert('Ошибка', 'Не удалось отправить сообщение: $e');
    }
  }

  void _disconnect() {
    try {
      _client?.disconnect();
      setState(() {
        _isConnected = false;
        _messages.clear();
        _currentScreen = 0;
      });
    } catch (e) {
      print('Ошибка отключения: $e');
    }
  }

  void _goBack() {
    _disconnect();
  }

  void _showAlert(String title, String message) {
    showCupertinoDialog(
      context: context,
      builder: (context) => CupertinoAlertDialog(
        title: Text(title),
        content: Text(message),
        actions: [
          CupertinoDialogAction(
            child: const Text('OK'),
            onPressed: () => Navigator.pop(context),
          ),
        ],
      ),
    );
  }
}