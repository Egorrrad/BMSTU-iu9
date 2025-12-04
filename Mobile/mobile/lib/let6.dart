import 'package:flutter/cupertino.dart';
import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context) {
    return const CupertinoApp(
      title: 'Умножение больших чисел',
      debugShowCheckedModeBanner: false,
      theme: CupertinoThemeData(
        primaryColor: CupertinoColors.systemBlue,
        brightness: Brightness.light,
      ),
      home: MultiplicationPage(),
    );
  }
}

typedef BigNum = List<int>;

class BigNumOperations {
  static BigNum normalize(BigNum num) {
    if (num.isEmpty) return [0];
    final result = List<int>.from(num);
    while (result.length > 1 && result.last == 0) {
      result.removeLast();
    }
    return result;
  }

  static BigNum padZeros(BigNum num, int n) {
    final result = List<int>.from(num);
    while (result.length < n) {
      result.add(0);
    }
    return result;
  }

  static BigNum naiveMultiply(BigNum a, BigNum b) {
    final n = a.length;
    final m = b.length;
    final result = List<int>.filled(n + m, 0);

    for (int i = 0; i < n; i++) {
      for (int j = 0; j < m; j++) {
        result[i + j] += a[i] * b[j];
      }
    }

    int carry = 0;
    for (int i = 0; i < result.length; i++) {
      int total = result[i] + carry;
      result[i] = total % 2;
      carry = total ~/ 2;
    }

    while (carry > 0) {
      result.add(carry % 2);
      carry = carry ~/ 2;
    }

    return normalize(result);
  }

  static BigNum addBigNum(BigNum a, BigNum b) {
    final n = a.length > b.length ? a.length : b.length;
    final result = List<int>.filled(n + 1, 0);
    int carry = 0;

    for (int i = 0; i < n; i++) {
      final digitA = i < a.length ? a[i] : 0;
      final digitB = i < b.length ? b[i] : 0;
      final total = digitA + digitB + carry;
      result[i] = total % 2;
      carry = total ~/ 2;
    }

    if (carry > 0) {
      result[n] = carry;
    }

    return normalize(result);
  }

  static BigNum subtractBigNum(BigNum a, BigNum b) {
    final result = List<int>.from(a);
    int borrow = 0;

    for (int i = 0; i < b.length; i++) {
      result[i] -= b[i] + borrow;
      if (result[i] < 0) {
        result[i] += 2;
        borrow = 1;
      } else {
        borrow = 0;
      }
    }

    for (int i = b.length; i < result.length; i++) {
      if (borrow == 0) break;
      result[i] -= borrow;
      if (result[i] < 0) {
        result[i] += 2;
        borrow = 1;
      } else {
        borrow = 0;
      }
    }

    return normalize(result);
  }

  static BigNum shiftLeft(BigNum a, int n) {
    if (n == 0 || (a.length == 1 && a[0] == 0)) {
      return List<int>.from(a);
    }
    return [...List<int>.filled(n, 0), ...a];
  }

  static BigNum karatsubaMultiply(BigNum a, BigNum b) {
    final nLen = a.length > b.length ? a.length : b.length;

    if (nLen <= 32) {
      return naiveMultiply(a, b);
    }

    final m = nLen ~/ 2;

    final aPadded = padZeros(a, nLen);
    final bPadded = padZeros(b, nLen);

    final a0 = aPadded.sublist(0, m);
    final a1 = aPadded.sublist(m);
    final b0 = bPadded.sublist(0, m);
    final b1 = bPadded.sublist(m);

    final z0 = karatsubaMultiply(a0, b0);
    final z2 = karatsubaMultiply(a1, b1);

    final a1PlusA0 = addBigNum(a1, a0);
    final b1PlusB0 = addBigNum(b1, b0);

    final z1Temp = karatsubaMultiply(a1PlusA0, b1PlusB0);
    final z1 = subtractBigNum(subtractBigNum(z1Temp, z2), z0);

    final result = addBigNum(
      addBigNum(shiftLeft(z2, 2 * m), shiftLeft(z1, m)),
      z0,
    );

    return normalize(result);
  }

  static BigNum fromBinaryString(String binary) {
    if (binary.isEmpty) return [0];
    final result = <int>[];
    for (int i = binary.length - 1; i >= 0; i--) {
      result.add(binary[i] == '1' ? 1 : 0);
    }
    return normalize(result);
  }

  static String toBinaryString(BigNum num) {
    if (num.isEmpty) return '0';
    final normalized = normalize(num);
    final buffer = StringBuffer();

    for (int i = normalized.length - 1; i >= 0; i--) {
      buffer.write(normalized[i]);
    }
    return buffer.toString();
  }
}

class MultiplicationPage extends StatefulWidget {
  const MultiplicationPage({super.key});

  @override
  State<MultiplicationPage> createState() => _MultiplicationPageState();
}

class _MultiplicationPageState extends State<MultiplicationPage> {
  final _firstNumberController = TextEditingController();
  final _secondNumberController = TextEditingController();
  String _result = '';
  String _naiveTime = '';
  String _karatsubaTime = '';
  String? _error;

  @override
  void dispose() {
    _firstNumberController.dispose();
    _secondNumberController.dispose();
    super.dispose();
  }

  bool _isValidBinary(String value) {
    if (value.isEmpty) return false;
    return RegExp(r'^[01]+$').hasMatch(value);
  }

  void _calculate() {
    FocusScope.of(context).unfocus();

    final first = _firstNumberController.text.trim();
    final second = _secondNumberController.text.trim();

    if (!_isValidBinary(first) || !_isValidBinary(second)) {
      setState(() {
        _error = 'Введите корректные двоичные числа (только 0 и 1)';
        _result = '';
      });
      return;
    }

    setState(() {
      _error = null;
    });

    final a = BigNumOperations.fromBinaryString(first);
    final b = BigNumOperations.fromBinaryString(second);

    final naiveStart = DateTime.now();
    final naiveResult = BigNumOperations.naiveMultiply(a, b);
    final naiveEnd = DateTime.now();
    final naiveDuration = naiveEnd.difference(naiveStart);

    final karatsubaStart = DateTime.now();
    final karatsubaResult = BigNumOperations.karatsubaMultiply(a, b);
    final karatsubaEnd = DateTime.now();
    final karatsubaDuration = karatsubaEnd.difference(karatsubaStart);

    final naiveStr = BigNumOperations.toBinaryString(naiveResult);
    final karatsubaStr = BigNumOperations.toBinaryString(karatsubaResult);

    setState(() {
      if (naiveStr == karatsubaStr) {
        _result = naiveStr;
        _naiveTime = _formatDuration(naiveDuration);
        _karatsubaTime = _formatDuration(karatsubaDuration);
      } else {
        _error = 'Ошибка алгоритма: результаты не совпадают!';
      }
    });
  }

  void _generateRandom() {
    final random = DateTime.now().millisecondsSinceEpoch;

    final length1 = 500 + (random % 500);
    final length2 = 500 + ((random ~/ 2) % 500);

    final buffer1 = StringBuffer('1');
    final buffer2 = StringBuffer('1');

    for (int i = 1; i < length1; i++) {
      buffer1.write(DateTime.now().microsecond % 2);
    }

    for (int i = 1; i < length2; i++) {
      buffer2.write(DateTime.now().microsecond % 2);
    }

    setState(() {
      _firstNumberController.text = buffer1.toString();
      _secondNumberController.text = buffer2.toString();
      _result = '';
      _naiveTime = '';
      _karatsubaTime = '';
      _error = null;
    });
  }

  String _formatDuration(Duration duration) {
    if (duration.inMilliseconds >= 1000) {
      return '${(duration.inMilliseconds / 1000).toStringAsFixed(3)} с';
    } else if (duration.inMilliseconds >= 1) {
      return '${duration.inMilliseconds} мс';
    } else {
      return '${duration.inMicroseconds} мкс';
    }
  }

  @override
  Widget build(BuildContext context) {
    return CupertinoPageScaffold(
      navigationBar: const CupertinoNavigationBar(
        middle: Text('Умножение больших чисел'),
      ),
      child: SafeArea(
        child: SingleChildScrollView(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              const Text(
                'Введите двоичные числа:',
                style: TextStyle(
                  fontSize: 14,
                  color: CupertinoColors.systemGrey,
                ),
              ),
              const SizedBox(height: 8),
              CupertinoTextField(
                controller: _firstNumberController,
                placeholder: 'Первое число (0 и 1)',
                padding: const EdgeInsets.all(12),
                maxLines: 3,
                minLines: 1,
                inputFormatters: [
                  FilteringTextInputFormatter.allow(RegExp(r'[01]')),
                ],
              ),
              const SizedBox(height: 16),
              CupertinoTextField(
                controller: _secondNumberController,
                placeholder: 'Второе число (0 и 1)',
                padding: const EdgeInsets.all(12),
                maxLines: 3,
                minLines: 1,
                inputFormatters: [
                  FilteringTextInputFormatter.allow(RegExp(r'[01]')),
                ],
              ),
              const SizedBox(height: 24),
              Row(
                children: [
                  Expanded(
                    child: CupertinoButton(
                      padding: EdgeInsets.zero,
                      onPressed: _generateRandom,
                      color: CupertinoColors.systemGrey,
                      child: const Text('Случайно'),
                    ),
                  ),
                  const SizedBox(width: 16),
                  Expanded(
                    child: CupertinoButton(
                      padding: EdgeInsets.zero,
                      onPressed: _calculate,
                      color: CupertinoColors.systemBlue,
                      child: const Text('Вычислить'),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 24),
              if (_error != null)
                Container(
                  padding: const EdgeInsets.all(12),
                  decoration: BoxDecoration(
                    color: CupertinoColors.systemRed.withOpacity(0.1),
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: Text(
                    _error!,
                    style: const TextStyle(color: CupertinoColors.systemRed),
                  ),
                ),
              if (_result.isNotEmpty) ...[
                const SizedBox(height: 16),
                const Text(
                  'Результат:',
                  style: TextStyle(fontWeight: FontWeight.bold, fontSize: 16),
                ),
                const SizedBox(height: 8),
                Container(
                  width: double.infinity,
                  padding: const EdgeInsets.all(12),
                  decoration: BoxDecoration(
                    color: CupertinoColors.systemGrey6,
                    borderRadius: BorderRadius.circular(8),
                    border: Border.all(color: CupertinoColors.systemGrey4),
                  ),
                  child: SelectableText(
                    _result.length > 300
                        ? '${_result.substring(0, 150)}...${_result.substring(_result.length - 150)}'
                        : _result,
                    style: const TextStyle(fontFamily: 'Courier', fontSize: 14),
                  ),
                ),
                const SizedBox(height: 8),
                Text(
                  'Длина результата: ${_result.length} бит',
                  style: const TextStyle(color: CupertinoColors.secondaryLabel),
                ),
                const SizedBox(height: 24),
                const Text(
                  'Сравнение производительности:',
                  style: TextStyle(fontWeight: FontWeight.bold, fontSize: 16),
                ),
                const SizedBox(height: 8),
                Container(
                  padding: const EdgeInsets.all(12),
                  decoration: BoxDecoration(
                    color: CupertinoColors.systemGrey6,
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          const Text('Наивный алгоритм:'),
                          Text(
                            _naiveTime,
                            style: const TextStyle(fontWeight: FontWeight.bold),
                          ),
                        ],
                      ),
                      const Divider(),
                      Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          const Text('Алгоритм Карацубы:'),
                          Text(
                            _karatsubaTime,
                            style: const TextStyle(
                              fontWeight: FontWeight.bold,
                              color: CupertinoColors.activeGreen,
                            ),
                          ),
                        ],
                      ),
                    ],
                  ),
                ),
              ],
              const SizedBox(height: 40),
            ],
          ),
        ),
      ),
    );
  }
}
