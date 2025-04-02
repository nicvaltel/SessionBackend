import requests
import websockets
import asyncio
import time

BASE_URL = "http://localhost:3000/api"
WS_URL = "ws://127.0.0.1:1234"

# Login and get session tokens
def login(username, password):
    response = requests.post(f"{BASE_URL}/login", json={"username": username, "password": password})
    return response.json().get("session_id")

session_id1 = login("hello@mail.md", "123456Hello")
session_id2 = login("hello2@mail.md", "123456Hello2")

print(f"Session Token1: {session_id1}")
print(f"Session Token2: {session_id2}")


# Send WebSocket message
async def send_ws_message_session(s_id):
    async with websockets.connect(WS_URL) as websocket:
        await websocket.send(f"session_id::{s_id}\n")
        ws_response = await websocket.recv()
        print(f"ws_response: {ws_response}")

# Run the WebSocket communication in the background without blocking execution
async def main():
    task = asyncio.create_task(send_ws_message_session(session_id1))
    time.sleep(4)
    # await task

async def main2():
    task = asyncio.create_task(send_ws_message_session(session_id1))
    time.sleep(1)

# Start the async event loop
asyncio.run(main())
# asyncio.run(main2())




# # Send WebSocket message
# async def send_ws_message():
#     async with websockets.connect(WS_URL) as websocket:
#         await websocket.send(f"session_id::{session_id1}")
#         ws_response1 = await websocket.recv()
#         print(f"WS_RESPONSE1: {ws_response1}")

# asyncio.run(send_ws_message())



# Create a game room
response_create_room = requests.post(f"{BASE_URL}/create-room", json={"game_type": "casual"}, cookies={"session_id": session_id1})
lobby_room_id = response_create_room.json().get("lobby_room_id")
print(f"lobbyRoomId: {lobby_room_id}")

# Get lobby rooms
response_get_lobby = requests.get(f"{BASE_URL}/lobby", cookies={"session_id": session_id2})
lobby_all_rooms = response_get_lobby.json().get("lobby_rooms_id", [])
lobby_first_room = lobby_all_rooms[0] if lobby_all_rooms else None
print(f"all lobby rooms: {lobby_all_rooms}")
print(f"lobby first room: {lobby_first_room}")



# Join a room if available
if lobby_first_room:
    response_join_room = requests.get(f"{BASE_URL}/join-room/{lobby_first_room}", cookies={"session_id": session_id2})
    joined_room = response_join_room.json().get("room_id")
    print(f"joined room: {joined_room}")
