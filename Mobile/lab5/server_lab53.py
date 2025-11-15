import asyncio
import json
import logging
from datetime import datetime
from websockets import serve
from websockets.exceptions import ConnectionClosedOK, ConnectionClosedError
from typing import Dict, Any, Set

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("Lab5WebSocketServer")


class Lab2WebSocketServer:
    def __init__(self):
        self._clients: Set[Any] = set()
        self._counter_value = 0
        self._service_enabled = True
        self._max_clients = 10

    async def start(self, host: str = "0.0.0.0", port: int = 8082):
        """Start the WebSocket server"""
        try:
            async with serve(self._handle_connection, host, port):
                logger.info("Lab5 WebSocket Server Started!")
                logger.info(f"Local:  ws://localhost:{port}")
                logger.info(f"Network: ws://YOUR_IP:{port}")
                logger.info(
                    "Supported commands: get_status, get_counter, increment, decrement, reset, enable_service, disable_service")
                logger.info("Press Ctrl+C to stop the server")

                await asyncio.Future()

        except Exception as e:
            logger.error(f"Failed to start server: {e}")
            raise

    async def _handle_connection(self, websocket):
        """Handle new client connection"""
        if len(self._clients) >= self._max_clients:
            await self._send_error(websocket, "Server is at maximum capacity")
            return

        self._clients.add(websocket)
        client_ip = websocket.remote_address[0] if websocket.remote_address else "unknown"
        logger.info(f"New client connected from {client_ip}. Total clients: {len(self._clients)}")

        try:

            await self._send_initial_state(websocket)


            async for message in websocket:
                await self._handle_message(message, websocket, client_ip)

        except ConnectionClosedOK:
            logger.info(f"Client {client_ip} disconnected normally")
        except ConnectionClosedError as e:
            logger.error(f"Client {client_ip} disconnected with error: {e}")
        except Exception as e:
            logger.error(f"Unexpected error with client {client_ip}: {e}")
        finally:

            self._clients.discard(websocket)
            logger.info(f"Client {client_ip} removed. Total clients: {len(self._clients)}")

    async def _send_initial_state(self, websocket):
        """Send initial state to newly connected client"""
        await self._send_to_client(websocket, {
            'type': 'connection',
            'status': 'connected',
            'message': 'Connected to Lab2 Counter Server'
        })

        await self._send_to_client(websocket, {
            'type': 'service_status',
            'enabled': self._service_enabled
        })

        await self._send_to_client(websocket, {
            'type': 'counter_value',
            'value': self._counter_value,
            'last_update': self._get_current_timestamp()
        })

    async def _handle_message(self, message: str, websocket, client_ip: str):
        """Process incoming message from client"""
        try:
            logger.info(f"Received from {client_ip}: {message}")

            data = self._parse_message(message)
            if data is None:
                await self._send_error(websocket, 'Invalid message format')
                return

            command = data.get('command')

            if command == 'get_status':
                await self._send_service_status(websocket)

            elif command == 'get_counter':
                await self._send_counter_value(websocket)

            elif command == 'increment':
                await self._handle_increment(websocket, client_ip)

            elif command == 'decrement':
                await self._handle_decrement(websocket, client_ip)

            elif command == 'reset':
                await self._handle_reset(websocket, client_ip)

            elif command == 'enable_service':
                await self._handle_enable_service(websocket, client_ip)

            elif command == 'disable_service':
                await self._handle_disable_service(websocket, client_ip)

            else:
                await self._send_error(websocket, f'Unknown command: {command}')

        except Exception as e:
            logger.error(f"Error processing message from {client_ip}: {e}")
            await self._send_error(websocket, f'Server error: {str(e)}')

    async def _handle_increment(self, websocket, client_ip: str):
        """Handle increment counter command"""
        if not self._service_enabled:
            await self._send_error(websocket, 'Service is disabled')
            return

        self._counter_value += 1

        self._counter_value = min(self._counter_value, 100)

        logger.info(f"Client {client_ip} incremented counter to: {self._counter_value}")
        await self._broadcast_counter_update()

    async def _handle_decrement(self, websocket, client_ip: str):
        """Handle decrement counter command"""
        if not self._service_enabled:
            await self._send_error(websocket, 'Service is disabled')
            return

        self._counter_value -= 1

        self._counter_value = max(self._counter_value, -100)

        logger.info(f"Client {client_ip} decremented counter to: {self._counter_value}")
        await self._broadcast_counter_update()

    async def _handle_reset(self, websocket, client_ip: str):
        """Handle reset counter command"""
        if not self._service_enabled:
            await self._send_error(websocket, 'Service is disabled')
            return

        self._counter_value = 0
        logger.info(f"Client {client_ip} reset counter to: {self._counter_value}")
        await self._broadcast_counter_update()

    async def _handle_enable_service(self, websocket, client_ip: str):
        """Handle enable service command"""
        self._service_enabled = True
        logger.info(f"Client {client_ip} enabled service")
        await self._broadcast_service_status()
        await self._send_success(websocket, 'Service enabled successfully')

    async def _handle_disable_service(self, websocket, client_ip: str):
        """Handle disable service command"""
        self._service_enabled = False
        logger.info(f"Client {client_ip} disabled service")
        await self._broadcast_service_status()
        await self._send_success(websocket, 'Service disabled successfully')

    async def _send_service_status(self, websocket):
        """Send current service status to client"""
        await self._send_to_client(websocket, {
            'type': 'service_status',
            'enabled': self._service_enabled
        })

    async def _send_counter_value(self, websocket):
        """Send current counter value to client"""
        await self._send_to_client(websocket, {
            'type': 'counter_value',
            'value': self._counter_value,
            'last_update': self._get_current_timestamp()
        })

    async def _broadcast_counter_update(self):
        """Broadcast counter update to all connected clients"""
        message = {
            'type': 'counter_value',
            'value': self._counter_value,
            'last_update': self._get_current_timestamp()
        }
        await self._broadcast_to_all(message)

    async def _broadcast_service_status(self):
        """Broadcast service status update to all connected clients"""
        message = {
            'type': 'service_status',
            'enabled': self._service_enabled
        }
        await self._broadcast_to_all(message)

    def _parse_message(self, message: str) -> Dict[str, Any]:
        """Parse JSON message"""
        try:
            return json.loads(message)
        except json.JSONDecodeError as e:
            logger.error(f"JSON decode error: {e}")
            return None

    def _get_current_timestamp(self) -> str:
        """Get current timestamp in readable format"""
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    async def _send_to_client(self, websocket, data: Dict[str, Any]):
        """Send message to specific client"""
        try:
            message = json.dumps(data)
            await websocket.send(message)
            logger.debug(f"Sent to client: {message}")
        except Exception as e:
            logger.error(f"Error sending to client: {e}")

    async def _send_error(self, websocket, error: str):
        """Send error message to client"""
        await self._send_to_client(websocket, {
            'type': 'error',
            'message': error
        })

    async def _send_success(self, websocket, message: str):
        """Send success message to client"""
        await self._send_to_client(websocket, {
            'type': 'success',
            'message': message
        })

    async def _broadcast_to_all(self, data: Dict[str, Any]):
        """Broadcast message to all connected clients"""
        if not self._clients:
            return

        disconnected_clients = []
        message = json.dumps(data)

        for websocket in self._clients.copy():
            try:
                await websocket.send(message)
            except Exception as e:
                logger.error(f"Error broadcasting to client: {e}")
                disconnected_clients.append(websocket)


        for websocket in disconnected_clients:
            self._clients.discard(websocket)


async def main():
    server = Lab2WebSocketServer()
    await server.start(port=8091)


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("Server stopped by user")