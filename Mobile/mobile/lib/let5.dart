import 'package:flutter/cupertino.dart';
import 'package:mailer/mailer.dart';
import 'package:mailer/smtp_server.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const CupertinoApp(
      title: 'Yandex Mail Sender',
      theme: CupertinoThemeData(
        primaryColor: CupertinoColors.activeBlue,
      ),
      home: LoginScreen(),
    );
  }
}

// --- ЭКРАН ВХОДА ---
class LoginScreen extends StatefulWidget {
  const LoginScreen({super.key});

  @override
  State<LoginScreen> createState() => _LoginScreenState();
}

class _LoginScreenState extends State<LoginScreen> {
  final _emailController = TextEditingController(text: 'testmail@yandex.ru');
  final _passwordController = TextEditingController(text: '');

  void _login() {
    if (_emailController.text.isEmpty || _passwordController.text.isEmpty) {
      _showAlert('Ошибка', 'Заполните все поля');
      return;
    }
    Navigator.push(
      context,
      CupertinoPageRoute(
        builder: (_) => ComposeScreen(
          username: _emailController.text,
          password: _passwordController.text,
        ),
      ),
    );
  }

  void _showAlert(String title, String message) {
    showCupertinoDialog(
      context: context,
      builder: (ctx) => CupertinoAlertDialog(
        title: Text(title),
        content: Text(message),
        actions: [
          CupertinoDialogAction(
            child: const Text('OK'),
            onPressed: () => Navigator.pop(ctx),
          )
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: const CupertinoNavigationBar(
        middle: Text('Вход в Yandex Mail'),
      ),
      child: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(20.0),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              const Icon(
                CupertinoIcons.mail_solid,
                size: 100,
                color: CupertinoColors.systemRed,
              ),
              const SizedBox(height: 40),
              CupertinoTextField(
                controller: _emailController,
                placeholder: 'Email (Yandex)',
                prefix: const Padding(
                  padding: EdgeInsets.only(left: 8),
                  child: Icon(CupertinoIcons.person, size: 20),
                ),
                padding: const EdgeInsets.all(12),
                keyboardType: TextInputType.emailAddress,
                decoration: BoxDecoration(
                  color: CupertinoColors.systemGrey6,
                  borderRadius: BorderRadius.circular(8),
                ),
              ),
              const SizedBox(height: 16),
              CupertinoTextField(
                controller: _passwordController,
                placeholder: 'Пароль приложения',
                prefix: const Padding(
                  padding: EdgeInsets.only(left: 8),
                  child: Icon(CupertinoIcons.lock, size: 20),
                ),
                padding: const EdgeInsets.all(12),
                obscureText: true,
                decoration: BoxDecoration(
                  color: CupertinoColors.systemGrey6,
                  borderRadius: BorderRadius.circular(8),
                ),
              ),
              const SizedBox(height: 12),
              const Text(
                'Используйте пароль приложения, а не обычный пароль!',
                style: TextStyle(
                  color: CupertinoColors.systemGrey,
                  fontSize: 13,
                ),
                textAlign: TextAlign.center,
              ),
              const SizedBox(height: 40),
              SizedBox(
                width: double.infinity,
                child: CupertinoButton.filled(
                  onPressed: _login,
                  child: const Text('Войти'),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  @override
  void dispose() {
    _emailController.dispose();
    _passwordController.dispose();
    super.dispose();
  }
}

// --- ЭКРАН НАПИСАНИЯ ПИСЬМА ---
class ComposeScreen extends StatefulWidget {
  final String username;
  final String password;

  const ComposeScreen({
    super.key,
    required this.username,
    required this.password,
  });

  @override
  State<ComposeScreen> createState() => _ComposeScreenState();
}

class _ComposeScreenState extends State<ComposeScreen> {
  final _toController = TextEditingController();
  final _ccController = TextEditingController();
  final _bccController = TextEditingController();
  final _subjectController = TextEditingController();
  final _bodyController = TextEditingController();

  bool _isSending = false;
  bool _showCc = false;
  bool _showBcc = false;

  Future<void> _sendEmail() async {
    if (_toController.text.isEmpty) {
      _showAlert('Ошибка', 'Укажите получателя');
      return;
    }

    setState(() => _isSending = true);

    try {
      final smtpServer = SmtpServer(
        'smtp.yandex.com',
        port: 465,
        ssl: true,
        username: widget.username,
        password: widget.password,
      );

      final message = Message()
        ..from = Address(widget.username, 'Flutter App User(Egor)')
        ..recipients.add(_toController.text)
        ..subject = _subjectController.text.isEmpty
            ? '(Без темы)'
            : _subjectController.text
        ..text = _bodyController.text
        ..html = "<p>${_bodyController.text.replaceAll('\n', '<br>')}</p>";

      // Добавляем CC получателей
      if (_ccController.text.isNotEmpty) {
        final ccList = _ccController.text
            .split(',')
            .map((e) => e.trim())
            .where((e) => e.isNotEmpty);
        message.ccRecipients.addAll(ccList);
      }

      // Добавляем BCC получателей
      if (_bccController.text.isNotEmpty) {
        final bccList = _bccController.text
            .split(',')
            .map((e) => e.trim())
            .where((e) => e.isNotEmpty);
        message.bccRecipients.addAll(bccList);
      }

      final sendReport = await send(message, smtpServer);
      print('Message sent: $sendReport');

      setState(() => _isSending = false);

      _showSuccessDialog();
    } catch (e) {
      setState(() => _isSending = false);
      _showAlert('Ошибка отправки', e.toString());
    }
  }

  void _showSuccessDialog() {
    showCupertinoDialog(
      context: context,
      builder: (ctx) => CupertinoAlertDialog(
        title: const Text('Успешно'),
        content: const Text('Письмо отправлено!'),
        actions: [
          CupertinoDialogAction(
            child: const Text('OK'),
            onPressed: () {
              Navigator.pop(ctx);
              _clearFields();
            },
          )
        ],
      ),
    );
  }

  void _clearFields() {
    _toController.clear();
    _ccController.clear();
    _bccController.clear();
    _subjectController.clear();
    _bodyController.clear();
    setState(() {
      _showCc = false;
      _showBcc = false;
    });
  }

  void _showAlert(String title, String message) {
    showCupertinoDialog(
      context: context,
      builder: (ctx) => CupertinoAlertDialog(
        title: Text(title),
        content: Text(message),
        actions: [
          CupertinoDialogAction(
            child: const Text('OK'),
            onPressed: () => Navigator.pop(ctx),
          )
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: CupertinoNavigationBar(
        middle: const Text('Новое письмо'),
        leading: CupertinoButton(
          padding: EdgeInsets.zero,
          child: const Text('Отмена'),
          onPressed: () => Navigator.pop(context),
        ),
        trailing: _isSending
            ? const CupertinoActivityIndicator()
            : CupertinoButton(
          padding: EdgeInsets.zero,
          child: const Text('Отправить'),
          onPressed: _sendEmail,
        ),
      ),
      child: SafeArea(
        child: SingleChildScrollView(
          child: Column(
            children: [
              // Поле "Кому"
              _buildTextField(
                controller: _toController,
                placeholder: 'Кому',
                icon: CupertinoIcons.person,
              ),
              Container(height: 1, color: CupertinoColors.separator),

              // Кнопки CC/BCC
              if (!_showCc || !_showBcc)
                Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
                  child: Row(
                    children: [
                      if (!_showCc)
                        CupertinoButton(
                          padding: EdgeInsets.zero,
                          child: const Text('Копия', style: TextStyle(fontSize: 14)),
                          onPressed: () => setState(() => _showCc = true),
                        ),
                      if (!_showCc && !_showBcc) const SizedBox(width: 16),
                      if (!_showBcc)
                        CupertinoButton(
                          padding: EdgeInsets.zero,
                          child: const Text('Скрытая копия', style: TextStyle(fontSize: 14)),
                          onPressed: () => setState(() => _showBcc = true),
                        ),
                    ],
                  ),
                ),

              // Поле "Копия"
              if (_showCc) ...[
                _buildTextField(
                  controller: _ccController,
                  placeholder: 'Копия (через запятую)',
                  icon: CupertinoIcons.person_2,
                ),
                Container(height: 1, color: CupertinoColors.separator),
              ],

              // Поле "Скрытая копия"
              if (_showBcc) ...[
                _buildTextField(
                  controller: _bccController,
                  placeholder: 'Скрытая копия (через запятую)',
                  icon: CupertinoIcons.eye_slash,
                ),
                Container(height: 1, color: CupertinoColors.separator),
              ],

              // Поле "Тема"
              _buildTextField(
                controller: _subjectController,
                placeholder: 'Тема',
                icon: CupertinoIcons.text_bubble,
              ),
              Container(height: 1, color: CupertinoColors.separator),

              // Поле "Текст письма"
              Container(
                padding: const EdgeInsets.all(16),
                constraints: const BoxConstraints(minHeight: 300),
                child: CupertinoTextField(
                  controller: _bodyController,
                  placeholder: 'Текст письма...',
                  maxLines: null,
                  decoration: null,
                  style: const TextStyle(fontSize: 16),
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildTextField({
    required TextEditingController controller,
    required String placeholder,
    required IconData icon,
  }) {
    return Padding(
      padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 12),
      child: Row(
        children: [
          Icon(icon, size: 22, color: CupertinoColors.systemGrey),
          const SizedBox(width: 12),
          Expanded(
            child: CupertinoTextField(
              controller: controller,
              placeholder: placeholder,
              decoration: null,
              style: const TextStyle(fontSize: 16),
            ),
          ),
        ],
      ),
    );
  }

  @override
  void dispose() {
    _toController.dispose();
    _ccController.dispose();
    _bccController.dispose();
    _subjectController.dispose();
    _bodyController.dispose();
    super.dispose();
  }
}