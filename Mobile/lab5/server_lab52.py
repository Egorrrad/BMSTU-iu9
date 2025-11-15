import asyncio
import json
import logging
from websockets import serve
from websockets.exceptions import ConnectionClosedOK, ConnectionClosedError
from typing import Dict, Any, Optional

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("ArithmeticServer")


class ArithmeticServer:
    def __init__(self):
        self._clients: Dict[str, Any] = {}
        self._client_counter = 0
        self._current_slider_value = 50.0
        self._client_slider_values: Dict[str, float] = {}

    async def start(self, host: str = "0.0.0.0", port: int = 8080):
        """Start the WebSocket server"""
        try:
            async with serve(self._handle_connection, host, port):
                logger.info("Arithmetic WebSocket Server Started!")
                logger.info(f"Local:  ws://localhost:{port}")
                logger.info(f"Network: ws://YOUR_IP:{port} (replace YOUR_IP with your actual IP)")
                logger.info("Supported operations: +, -, *, /")
                logger.info('Message format: {"operation": "+", "a": 5, "b": 3}')
                logger.info("Slider commands: get_slider_value, update_slider")
                logger.info("Press Ctrl+C to stop the server")

                await self._show_network_info(port)
                await asyncio.Future()

        except Exception as e:
            logger.error(f"Failed to start server: {e}")
            raise

    async def _show_network_info(self, port: int):
        """Show network interfaces information"""
        try:
            import socket
            hostname = socket.gethostname()
            local_ip = socket.gethostbyname(hostname)
            logger.info(f"Hostname: {hostname}")
            logger.info(f"Local IP: {local_ip}")

            try:
                import urllib.request
                external_ip = urllib.request.urlopen('https://ident.me').read().decode('utf8')
                logger.info(f"External IP: {external_ip}")
            except:
                logger.info("External IP: Use 'ipconfig' (Windows) or 'ifconfig' (Mac/Linux) to find your IP")

        except Exception as e:
            logger.info(f"Network info unavailable: {e}")

    async def _handle_connection(self, websocket):
        """Handle new client connection"""
        client_id = f"client_{self._client_counter + 1}"
        self._client_counter += 1
        self._clients[client_id] = websocket
        self._client_slider_values[client_id] = self._current_slider_value

        client_ip = websocket.remote_address[0] if websocket.remote_address else "unknown"
        logger.info(f"🔌 Client {client_id} connected from {client_ip}")

        try:

            await self._send_to_client(websocket, {
                'type': 'connection',
                'status': 'connected',
                'clientId': client_id,
                'message': 'Connected to Arithmetic Server'
            })


            await self._send_to_client(websocket, {
                'type': 'slider_value',
                'value': self._client_slider_values[client_id]
            })


            async for message in websocket:
                await self._handle_message(message, client_id, websocket)

        except ConnectionClosedOK:
            logger.info(f"Client {client_id} disconnected")
        except ConnectionClosedError as e:
            logger.error(f"Error with client {client_id}: {e}")
        except Exception as e:
            logger.error(f"Unexpected error with client {client_id}: {e}")
        finally:

            self._clients.pop(client_id, None)
            self._client_slider_values.pop(client_id, None)

    async def _handle_message(self, message: str, client_id: str, websocket):
        """Process incoming message from client"""
        try:
            logger.info(f"Received from {client_id}: {message}")

            data = self._parse_message(message)
            if data is None:
                await self._send_error(websocket, 'Invalid message format')
                return

            message_type = data.get('type')


            if message_type == 'get_slider_value':
                await self._send_to_client(websocket, {
                    'type': 'slider_value',
                    'value': self._client_slider_values.get(client_id, self._current_slider_value)
                })
                return

            if message_type == 'update_slider':
                value = data.get('value')
                if isinstance(value, (int, float)) and 0 <= value <= 100:
                    self._client_slider_values[client_id] = float(value)
                    self._current_slider_value = float(value)

                    await self._send_to_client(websocket, {
                        'type': 'slider_updated',
                        'value': value,
                        'message': 'Slider value updated successfully'
                    })

                    logger.info(f"🎚️ Client {client_id} updated slider to: {value}")


                    await self._broadcast_to_all({
                        'type': 'slider_updated',
                        'value': value,
                        'message': f'Slider updated by {client_id}',
                        'clientId': client_id
                    })
                else:
                    await self._send_error(websocket, 'Invalid slider value. Must be between 0 and 100')
                return


            operation = data.get('operation')
            a = data.get('a')
            b = data.get('b')


            a = self._safe_convert_to_float(a)
            b = self._safe_convert_to_float(b)

            if a is None or b is None:
                await self._send_error(websocket, 'Invalid numbers provided')
                return

            if operation not in ['+', '-', '*', '/']:
                await self._send_error(websocket, 'Invalid operation. Use +, -, *, or /')
                return


            result = self._calculate(operation, a, b)

            if result is None:
                await self._send_error(websocket, 'Calculation error')
                return


            await self._send_to_client(websocket, {
                'type': 'result',
                'operation': f'{a} {operation} {b}',
                'result': result,
                'a': a,
                'b': b,
                'operationSymbol': operation
            })

            logger.info(f"Client {client_id}: {a} {operation} {b} = {result}")

        except Exception as e:
            logger.error(f"Error processing message from {client_id}: {e}")
            await self._send_error(websocket, f'Server error: {str(e)}')

    def _safe_convert_to_float(self, value) -> Optional[float]:
        """Safely convert value to float"""
        if value is None:
            return None
        try:
            return float(value)
        except (ValueError, TypeError):
            return None

    def _calculate(self, operation: str, a: float, b: float) -> Optional[float]:
        """Perform arithmetic calculation"""
        try:
            if operation == '+':
                return a + b
            elif operation == '-':
                return a - b
            elif operation == '*':
                return a * b
            elif operation == '/':
                if b == 0:
                    raise ValueError('Division by zero')
                return a / b
            else:
                return None
        except Exception as e:
            logger.error(f"Calculation error: {e}")
            return None

    def _parse_message(self, message: str) -> Optional[Dict[str, Any]]:
        """Parse JSON message with fallback"""
        try:
            return json.loads(message)
        except json.JSONDecodeError as e:
            logger.warning(f"JSON decode error: {e}, trying fallback parsing")
            return self._fallback_parse(message)

    def _fallback_parse(self, message: str) -> Optional[Dict[str, Any]]:
        """Fallback parsing for malformed JSON"""
        try:
            if not message.startswith('{') or not message.endswith('}'):
                return None


            content = message[1:-1].strip()
            if not content:
                return {}

            result = {}

            pairs = []
            current_pair = ""
            brace_count = 0
            bracket_count = 0

            for char in content + ',':
                if char == '{':
                    brace_count += 1
                elif char == '}':
                    brace_count -= 1
                elif char == '[':
                    bracket_count += 1
                elif char == ']':
                    bracket_count -= 1

                if char == ',' and brace_count == 0 and bracket_count == 0:
                    pairs.append(current_pair.strip())
                    current_pair = ""
                else:
                    current_pair += char

            for pair in pairs:
                if ':' in pair:
                    key, value = pair.split(':', 1)
                    key = key.strip().strip('"\'')
                    value = value.strip()


                    if value.startswith('"') and value.endswith('"'):
                        result[key] = value[1:-1]
                    elif value.startswith("'") and value.endswith("'"):
                        result[key] = value[1:-1]
                    elif value.lower() == 'true':
                        result[key] = True
                    elif value.lower() == 'false':
                        result[key] = False
                    elif value.lower() == 'null':
                        result[key] = None
                    else:
                        try:

                            if '.' in value:
                                result[key] = float(value)
                            else:
                                result[key] = int(value)
                        except ValueError:
                            result[key] = value

            return result if result else None

        except Exception as e:
            logger.error(f"Fallback parse error: {e}")
            return None

    async def _send_to_client(self, websocket, data: Dict[str, Any]):
        """Send message to specific client"""
        try:
            message = json.dumps(data)
            await websocket.send(message)
            logger.debug(f"📤 Sent to client: {message}")
        except Exception as e:
            logger.error(f"Error sending to client: {e}")

    async def _send_error(self, websocket, error: str):
        """Send error message to client"""
        await self._send_to_client(websocket, {
            'type': 'error',
            'message': error
        })

    async def _broadcast_to_all(self, data: Dict[str, Any]):
        """Broadcast message to all connected clients"""
        if not self._clients:
            return

        disconnected_clients = []

        for client_id, websocket in self._clients.items():
            try:
                await self._send_to_client(websocket, data)
            except Exception as e:
                logger.error(f"Error broadcasting to client {client_id}: {e}")
                disconnected_clients.append(client_id)


        for client_id in disconnected_clients:
            self._clients.pop(client_id, None)
            self._client_slider_values.pop(client_id, None)


async def main():
    server = ArithmeticServer()
    await server.start(port=8090)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("Server stopped by user")
